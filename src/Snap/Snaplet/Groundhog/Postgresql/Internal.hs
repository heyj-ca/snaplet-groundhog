
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Snaplet.Groundhog.Postgresql.Internal where

import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Trans               (lift)
import           Control.Monad.Trans.Control       (MonadBaseControl (..),
                                                    control)
import           Control.Monad.Trans.Identity      (IdentityT (IdentityT))
import           Control.Monad.Trans.List          (ListT (ListT))
import           Control.Monad.Trans.Maybe         (MaybeT (MaybeT))
import           Control.Monad.Trans.Reader        (ReaderT (ReaderT))
import qualified Control.Monad.Trans.RWS.Lazy      as LRWS
import qualified Control.Monad.Trans.RWS.Strict    as SRWS
import qualified Control.Monad.Trans.State.Lazy    as LS
import qualified Control.Monad.Trans.State.Strict  as SS
import qualified Control.Monad.Trans.Writer.Lazy   as LW
import qualified Control.Monad.Trans.Writer.Strict as SW
import           Data.ByteString                   (ByteString)
import           Data.Monoid                       (Monoid)
import Data.Pool (Pool, withResource)
import           Database.Groundhog.Postgresql

data GroundhogPostgres = GroundhogPostgres
      { pgPool :: Pool Postgresql
      }


------------------------------------------------------------------------------
-- | Data type holding all the snaplet's config information.
data GHPGConfig = GHPGConfig
    { ghpgConnStr    :: ByteString
      -- ^ A libpq connection string.
    , ghpgNumStripes :: Int
      -- ^ The number of distinct sub-pools to maintain. The smallest
      -- acceptable value is 1.
    , ghpgIdleTime   :: Double
      -- ^ Amount of time for which an unused resource is kept open. The
      -- smallest acceptable value is 0.5 seconds.
    , ghpgResources  :: Int
      -- ^ Maximum number of resources to keep open per stripe. The smallest
      -- acceptable value is 1.
    }


class HasGroundhogPostgres m where
    getGroundhogPostgresState :: m GroundhogPostgres
------------------------------------------------------------------------------
-- | Returns a config object with default values and the specified connection
-- string.
pgsDefaultConfig :: ByteString
                   -- ^ A connection string such as \"host=localhost
                   -- port=5432 dbname=mydb\"
                 -> GHPGConfig
pgsDefaultConfig connstr = GHPGConfig connstr 1 5 20




