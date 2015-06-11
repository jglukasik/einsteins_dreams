{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text, pack, unpack, concat)
import           Data.Text.Lazy (fromStrict)
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Mail.Mime
import           Network.Mail.Client.Gmail
import           Prelude hiding (concat)
import           System.Locale

import Credentials (googleAppPassword)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Dream
  date UTCTime
  content Text
  deriving Show
SavedEmail
  address String
  deriving Show
|]

main :: IO ()
main = do
    dateTime <- liftIO dateIn1905
    maybeDream <- runSqlite "dream_diary.db" $ do selectFirst [DreamDate ==. dateTime] []
    case maybeDream of
      Nothing -> return ()
      Just todaysDream -> runSqlite "dream_diary.db" $ do
        emailRows <- selectList [SavedEmailAddress !=. ""] []
        let emails = map (pack . savedEmailAddress . entityVal) emailRows 
        let dream = fromStrict . formatDream . dreamContent . entityVal $ todaysDream
        liftIO $ mapM_ (sendAlertEmail dream) emails

sendAlertEmail body recepient = sendGmail "jglukasik"
                                          googleAppPassword
                                          (Address (Just "Joseph Lukasik") "jglukasik@gmail.com")
                                          [Address Nothing recepient]
                                          []
                                          []
                                          "New Einstein's Dream"
                                          body
                                          []
                                          10000000

formatDream input = concat [ "Einstein's dream for today...\n\n"
                           , input
                           , "\n\nFrom http://ed.jgl.me \n"
                           , "Don't wan't these emails anymore? Enter your address again on the site to remove yourself from the mailing list"
                           ]

dateIn1905 :: IO UTCTime
dateIn1905 = do 
  utc <- getCurrentTime
  tz <- getCurrentTimeZone
  let time = utcToLocalTime tz utc
  let (_,m,d) = toGregorian $ localDay time
  return $ UTCTime (fromGregorian 1905 m d) 0

