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
import           Data.Text (pack)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Mail.Mime
import           Network.Mail.Client.Gmail

import Credentials (googleAppPassword)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SavedEmail
  address String
  deriving Show
|]

main :: IO ()
main = runSqlite "dream_diary.db" $ do
    runMigration migrateAll
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
                                     "Check out the new dream at http://ed.jgl.me"
                                     []
                                     10000000
