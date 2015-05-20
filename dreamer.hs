{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import System.Locale
import Data.Time
import Data.Text (Text, unpack)
import Yesod
import Yesod.Static
import Yesod.Form.Jquery
import Yesod.Default.Handlers
import Text.Hamlet
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Applicative ((<$>))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Dream
  date UTCTime
  content Text
  deriving Show
SavedEmail
  address String
|]

staticFiles "static"

data App = App 
    { getStatic :: Static
    , pool :: ConnectionPool
    }

data Email = Email
    { emailAddress :: Text
    }
    deriving Show

mkYesod "App" [parseRoutes|
/    HomeR    GET

/email    EmailR   POST
/email/remove EmailRemoveR   POST

/static      StaticR  Static getStatic
/favicon.ico FaviconR GET
/robots.txt  RobotsR  GET
|]

instance Yesod App where
    defaultLayout contents = do
      PageContent title headTags bodyTags <- widgetToPageContent $ do 
        addStylesheet $ StaticR css_bootstrap_min_css
        contents
      withUrlRenderer $(hamletFile "dream_catcher.hamlet")

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodJquery App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
      App static pool <- getYesod
      runSqlPool action pool

emailForm :: Html -> MForm Handler (FormResult Email, Widget)
emailForm = renderDivs $ Email
    <$> areq emailField "Email address:  " Nothing

postEmailRemoveR :: Handler Html
postEmailRemoveR = do
  ((removeFormResult, removeFormWidget), encType) <- runFormPost emailForm
  case removeFormResult of
    FormSuccess email -> do
        maybeDbResult <- runDB $ do
          selectFirst [SavedEmailAddress ==. unpack (emailAddress email)] []
        case maybeDbResult of
          Just entity -> do
            runDB $ do deleteWhere [SavedEmailAddress ==. unpack (emailAddress email)]
            defaultLayout [whamlet| <br>
                                    <p>Success! The email address:
                                    <p><code>#{ emailAddress email }</code>
                                    <p>Was removed from the database.
                                  |]
          Nothing -> do
            defaultLayout [whamlet| <br>
                                    <p>Could not remove from mailing list, email address:
                                    <p><code>#{ emailAddress email }</code>
                                    <p>Is not in the database
                                  |]

    _ -> defaultLayout [whamlet|<p>Error removing email from database|]


postEmailR :: Handler Html
postEmailR = do
  ((formResult, formWidget), encType) <- runFormPost emailForm
  case formResult of
    FormSuccess email -> do
        maybeDbResult <- runDB $ do
          selectFirst [SavedEmailAddress ==. unpack (emailAddress email)] []
        case maybeDbResult of
          Nothing -> do  
              runDB $ do insert . SavedEmail . unpack . emailAddress $ email
              defaultLayout [whamlet| <br>
                                      <p>Success! The email address:
                                      <p><code>#{ emailAddress email }</code>
                                      <p>will be emailed on days with a new dream
                                    |]
          Just (Entity savedEmailId savedEmail) -> do 
              (removeFormWidget, removeEncType) <- generateFormPost emailForm
              defaultLayout [whamlet| <br>
                                      <p>Your email address is already in the database.
                                      <p><code>#{ emailAddress email }</code>
                                      <br>
                                      <p>Would you like to remove yourself from the mailing list?
                                      <form method=post action=@{EmailRemoveR} enctype=#{removeEncType}>
                                        ^{removeFormWidget}
                                        <button class="btn btn-default">Yes, please remove me
                                    |]
    _ -> defaultLayout [whamlet| <br>
                                 <p>Invalid input
                               |]

getHomeR :: Handler Html
getHomeR = do
  dateTime <- liftIO dateIn1905
  maybeDream <- runDB $ selectFirst [DreamDate ==. dateTime] []
  (formWidget, encType) <- generateFormPost emailForm
  defaultLayout [whamlet|
                  <body>
                    <h3>#{showGregorian (utctDay (dateTime))}
                    $maybe Entity dreamid dream <- maybeDream
                      <p>#{dreamContent dream}
                    $nothing
                      <p>No dream today...
                    <br>
                    <br>
                    <div class="jumbotron">
                      <form method=post action=@{EmailR} enctype=#{encType}>
                        ^{formWidget}
                        <button class="btn btn-default">Submit
                        <br>
                        <br>
                        Want to be notified on days with a new dream? Then enter
                        your email here. I promise to never send you anything
                        other than alerts for these dreams (30 total between the
                        months of April and June), and to never share your email
                        with anyone else.
                |]

dateIn1905 :: IO UTCTime
dateIn1905 = do 
  c <- getCurrentTime 
  let (_,m,d) = toGregorian $ utctDay c
  return $ UTCTime (fromGregorian 1905 m d) 0

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "dream_diary.db"
  openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
      runMigration migrateAll
    static@(Static settings) <- static "static"
    warp 3000 $ App static pool
