{-# LANGUAGE OverloadedStrings, ViewPatterns,
             PackageImports, ScopedTypeVariables, RecordWildCards, FlexibleContexts #-}

module Snap.Snaplet.Groundhog.Postgresql
        ( initGroundhogPostgres
        , GroundhogPostgres
        , HasGroundhogPostgres(..)
        , runGH

        , (==.), (&&.), (=.), (||.)
        , (/=.), (<.), (<=.), (>.), (>=.)
        , (~>), limitTo, offsetBy, orderBy
        , insert
        , insert_
        , insertBy
        , insertByAll
        , replace
        , replaceBy
        , select
        , selectAll
        , get
        , getBy
        , update
        , delete
        , deleteBy
        , deleteAll
        , count
        , countAll
        , project
        , migrate

        , executeRaw
        , queryRaw

        , insertList
        , getList

        )
  where

import           Prelude hiding ((++))
import           Control.Applicative
import           Control.Monad (liftM)
import           Control.Monad.Logger
import           Control.Monad.IO.Class
import           Snap hiding (get)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import           Data.Pool
import           Data.Monoid
import           Data.Ratio
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import           Database.Groundhog
import           Database.Groundhog.Postgresql
import           Database.Groundhog.Core
import           Paths_snaplet_groundhog
import           Snap.Snaplet.Groundhog.Postgresql.Internal

(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

-- Taken from snaplet-postgresql-simple
getConnectionString :: C.Config -> IO ByteString
getConnectionString config = do
    let params =
            [ ["host"]
            , ["hostaddr"]
            , ["port"]
            , ["dbname","db"]
            , ["user"]
            , ["password","pass"]
            , ["connection_timeout"]
            , ["client_encoding"]
            , ["options"]
            , ["application_name"]
            , ["fallback_application_name"]
            , ["keepalives"]
            , ["keepalives_idle"]
            , ["keepalives_interval"]
            , ["keepalives_count"]
            , ["sslmode"]
            , ["sslcompression"]
            , ["sslcert"]
            , ["sslkey"]
            , ["sslrootcert"]
            , ["sslcrl"]
            , ["requirepeer"]
            , ["krbsrvname"]
            , ["gsslib"]
            , ["service"]
            ]
    connstr <- mconcat <$> mapM showParam params
    extra <- TB.fromText <$> C.lookupDefault "" config "connectionString"
    return $! T.encodeUtf8 (TL.toStrict (TB.toLazyText (connstr ++ extra)))
  where
    qt = TB.singleton '\''
    bs = TB.singleton '\\'
    sp = TB.singleton ' '
    eq = TB.singleton '='

    lookupConfig = foldr (\name names -> do
                            mval <- C.lookup config name
                            case mval of
                              Nothing -> names
                              Just _ -> return mval)
                         (return Nothing)

    showParam [] = undefined
    showParam names@(name:_) = do
      mval :: Maybe C.Value <- lookupConfig names
      let key = TB.fromText name ++ eq
      case mval of
        Nothing -> return mempty
        Just (C.Bool x) -> return (key ++ showBool x ++ sp)
        Just (C.String x) -> return (key ++ showText x ++ sp)
        Just (C.Number x) -> return (key ++ showNum x ++ sp)
        Just (C.List _) -> return mempty

    showBool x = TB.decimal (fromEnum x)

    showNum x = TB.formatRealFloat TB.Fixed Nothing
                   ( fromIntegral (numerator x)
                   / fromIntegral (denominator x) :: Double )

    showText x = qt ++ loop x
      where
        loop (T.break escapeNeeded -> (a,b))
          = TB.fromText a ++
              case T.uncons b of
                Nothing -> qt
                Just (c,b') -> escapeChar c ++ loop b'

    escapeNeeded c = c == '\'' || c == '\\'

    escapeChar c = case c of
                     '\'' -> bs ++ qt
                     '\\' -> bs ++ bs
                     _ -> TB.singleton c

description :: T.Text
description = "PostgreSQL abstraction using Groundhog"

datadir :: Maybe (IO FilePath)
datadir = Just $ liftM (<>"/resources/db") getDataDir

initGroundhogPostgres :: SnapletInit b GroundhogPostgres
initGroundhogPostgres = makeSnaplet "groundhog-postgresql" description datadir $ do
    config <- mkGHPGConfig =<< getSnapletUserConfig
    initHelper config

mkGHPGConfig :: MonadIO m => C.Config -> m GHPGConfig
mkGHPGConfig config = liftIO $ do
  connstr <- getConnectionString config
  stripes <- C.lookupDefault 1 config "numStripes"
  idle <- C.lookupDefault 5 config "idleTime"
  resources <- C.lookupDefault 20 config "maxResourcesPerStripe"
  return $ GHPGConfig connstr stripes idle resources

initHelper :: MonadIO m => GHPGConfig -> m GroundhogPostgres
initHelper GHPGConfig{..} = do
  pool <- liftIO $ createPostgresqlPool (B8.unpack ghpgConnStr) ghpgResources
  return $ GroundhogPostgres pool

runGH :: (MonadSnap m, HasGroundhogPostgres m)
            => Action Postgresql a
            -> m a
runGH f = do cm <- fmap pgPool getGroundhogPostgresState
             liftIO $ runDbConn f cm
