{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hairy.Models where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share,
  sqlSettings)

share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|
Task json
  content Text
  created UTCTime default=now()
|]
