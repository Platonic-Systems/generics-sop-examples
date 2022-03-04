{-# LANGUAGE AllowAmbiguousTypes #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}

module RouteEncoding where

import Data.SOP.NP (cmap_NP, collapse_NP)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics qualified as GHC
import Generics.SOP
import System.FilePath ((</>))

data Route
  = Route_Index
  | Route_Blog BlogRoute
  deriving stock (GHC.Generic, Show)
  deriving anyclass (Generic, HasDatatypeInfo, IsRoute)

data BlogRoute
  = BlogRoute_Index
  | BlogRoute_Post PostSlug
  deriving stock (GHC.Generic, Show)
  deriving anyclass (Generic, HasDatatypeInfo, IsRoute)

newtype PostSlug = PostSlug {unPostSlug :: Text}
  deriving newtype (Show)

-- Class of routes that can be encoded to a filename.
class IsRoute r where
  encodeRoute :: r -> FilePath
  default encodeRoute ::
    (Generic r, All2 IsRoute (Code r), HasDatatypeInfo r) =>
    r ->
    FilePath
  encodeRoute = gEncodeRoute

instance IsRoute PostSlug where
  encodeRoute (PostSlug slug) = T.unpack slug <> ".html"

gEncodeRoute :: forall r. (Generic r, All2 IsRoute (Code r), HasDatatypeInfo r) => r -> FilePath
gEncodeRoute x = gEncodeRoute' @r (from x)

gEncodeRoute' :: forall r. (All2 IsRoute (Code r), HasDatatypeInfo r) => SOP I (Code r) -> FilePath
gEncodeRoute' (SOP x) =
  case hcollapse $ hcmap (Proxy @(All IsRoute)) encProd x of
    [] -> ctorSuffix <> ".html"
    [p] -> ctorSuffix </> p
    _ -> error ">1 prods"
  where
    dtInfo = datatypeInfo (Proxy @r)
    dtName = datatypeName dtInfo
    ctorName = ctorNames !! hindex x
    -- From `BlogRoute_Index`, this gets us "index"
    ctorSuffix =
      maybe (error "ctor: bad naming") (T.unpack . T.toLower) $
        T.stripPrefix (T.pack $ dtName <> "_") (T.pack ctorName)
    ctorNames :: [ConstructorName] =
      hcollapse $ hmap (K . constructorName) $ constructorInfo dtInfo
    encProd :: All IsRoute xs => NP I xs -> K [FilePath] xs
    encProd =
      K . collapse_NP . cmap_NP (Proxy @IsRoute) encTerm
    encTerm :: IsRoute b => I b -> K FilePath b
    encTerm =
      K . encodeRoute . unI
