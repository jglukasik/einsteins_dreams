{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import System.Locale
import Data.Time
import Data.Text (Text)
import Yesod
import Text.Hamlet
import Data.List as L
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Dream
  date UTCTime
  content Text
  deriving Show
|]

data App = App ConnectionPool

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
    defaultLayout contents = do
      PageContent title headTags bodyTags <- widgetToPageContent contents
      withUrlRenderer $(hamletFile "dream_world.hamlet")

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
      App pool <- getYesod
      runSqlPool action pool

getHomeR :: Handler Html
getHomeR = do
  dateTime <- liftIO dateIn1905
  maybeDream <- runDB $ selectFirst [DreamDate ==. dateTime] []
  defaultLayout [whamlet|
                  <body>
                    <h1>#{showGregorian (utctDay (dateTime))}
                    $maybe Entity dreamid dream <- maybeDream
                        <p>#{dreamContent dream}
                    $nothing
                      <h1>No dream today...
                |]

dateIn1905 :: IO UTCTime
dateIn1905 = do 
  c <- getCurrentTime 
  let (_,m,d) = toGregorian $ utctDay c
  return $ UTCTime (fromGregorian 1905 m d) 0

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "dream_catcher.db"
  openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
      runMigration migrateAll
    warp 3000 $ App pool

