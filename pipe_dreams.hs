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
import           Data.Text (Text, pack)
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Mail.Mime
import           Network.Mail.Client.Gmail
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
        liftIO $ mapM_ sendAlertEmail emails

sendAlertEmail recepient = sendGmail "jglukasik"
                                     googleAppPassword
                                     (Address (Just "Joseph Lukasik") "jglukasik@gmail.com")
                                     [Address Nothing recepient]
                                     []
                                     []
                                     "New Einstein's Dream"
                                     "Check out today's dream at http://ed.jgl.me"
                                     []
                                     10000000

dateIn1905 :: IO UTCTime
dateIn1905 = do 
  utc <- getCurrentTime
  tz <- getCurrentTimeZone
  let time = utcToLocalTime tz utc
  let (_,m,d) = toGregorian $ localDay time
  return $ UTCTime (fromGregorian 1905 m d) 0

