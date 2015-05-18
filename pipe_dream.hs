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
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SavedEmail
  address String
  deriving Show
|]

main :: IO ()
main = runSqlite "dream_diary.db" $ do
    runMigration migrateAll
    maybeEmail <- selectFirst [SavedEmailAddress !=. ""] []
    case maybeEmail of
      Nothing -> liftIO $ putStrLn "Nothing found!"
      Just oneEmail -> liftIO $ 
        print . savedEmailAddress . entityVal $ oneEmail
