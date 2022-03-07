{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# HLINT ignore "Use camelCase" #-}

module RouteEncoding where

import Control.Monad (guard, msum)
import Data.Maybe (fromMaybe)
import Data.SOP.Constraint
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics qualified as GHC
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Generics.SOP
import System.FilePath (joinPath, splitDirectories, splitExtension, (</>))

data Route
  = Route_Index
  | Route_Blog BlogRoute
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (Generic, HasDatatypeInfo, IsRoute)

data BlogRoute
  = BlogRoute_Index
  | BlogRoute_Post PostSlug
  | BlogRoute_Qux PostSlug
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (Generic, HasDatatypeInfo, IsRoute)

newtype PostSlug = PostSlug {unPostSlug :: Text}
  deriving newtype (Eq, Show, IsString)

-- Class of routes that can be encoded to a filename.
class IsRoute r where
  encodeRoute :: r -> FilePath
  default encodeRoute ::
    (Generic r, All2 IsRoute (Code r), All IsRouteProd (Code r), HasDatatypeInfo r) =>
    r ->
    FilePath
  encodeRoute = gEncodeRoute
  decodeRoute :: FilePath -> Maybe r
  default decodeRoute ::
    (Generic r, All IsRouteProd (Code r), All2 IsRoute (Code r), HasDatatypeInfo r) =>
    FilePath ->
    Maybe r
  decodeRoute = gDecodeRoute

instance IsRoute PostSlug where
  encodeRoute (PostSlug slug) = T.unpack slug <> ".html"
  decodeRoute fp = do
    [part] <- pure $ splitDirectories fp
    (part', ".html") <- pure $ splitExtension part
    pure $ PostSlug $ T.pack part'

gEncodeRoute :: forall r. (Generic r, All2 IsRoute (Code r), All IsRouteProd (Code r), HasDatatypeInfo r) => r -> FilePath
gEncodeRoute x = gEncodeRoute' @r (from x)

-- Like `HCollapse`, but limited to 0 or 1 products in a `NP`.
class HCollapseMaybe h xs where
  hcollapseMaybe :: SListIN h xs => h (K a) xs -> Maybe a

instance HCollapseMaybe NP '[] where
  hcollapseMaybe Nil = Nothing

instance HCollapseMaybe NP '[p] where
  hcollapseMaybe (K x :* Nil) = Just x

instance (ps ~ TypeError ('Text "Expected at most 1 product")) => HCollapseMaybe NP (p ': p1 ': ps) where
  hcollapseMaybe _ = Nothing -- Unreachable, due to TypeError

class (All IsRoute xs, All Top xs, HCollapseMaybe NP xs) => IsRouteProd xs

instance (All IsRoute xs, All Top xs, HCollapseMaybe NP xs) => IsRouteProd xs

gEncodeRoute' :: forall r. (All2 IsRoute (Code r), All IsRouteProd (Code r), HasDatatypeInfo r) => SOP I (Code r) -> FilePath
gEncodeRoute' (SOP x) =
  case hcollapse $ hcmap (Proxy @IsRouteProd) encProd x of
    Nothing -> ctorSuffix <> ".html"
    Just p -> ctorSuffix </> p
  where
    ctorSuffix = getCtorSuffix @r x
    encProd :: (IsRouteProd xs) => NP I xs -> K (Maybe FilePath) xs
    encProd =
      K . hcollapseMaybe . hcmap (Proxy @IsRoute) encTerm
    encTerm :: IsRoute b => I b -> K FilePath b
    encTerm =
      K . encodeRoute . unI

-- From `BlogRoute_Foo`, this gets us "foo"
getCtorSuffix :: forall r. (All2 IsRoute (Code r), HasDatatypeInfo r) => NS (NP I) (Code r) -> FilePath
getCtorSuffix x =
  maybe (error "ctor: bad naming") (T.unpack . T.toLower) $
    T.stripPrefix (T.pack $ dtName <> "_") (T.pack ctorName)
  where
    dtInfo = datatypeInfo (Proxy @r)
    dtName = datatypeName dtInfo
    ctorName = ctorNames !! hindex x
    ctorNames :: [ConstructorName] =
      hcollapse $ hmap (K . constructorName) $ constructorInfo dtInfo

gDecodeRoute :: forall r. (Generic r, All IsRouteProd (Code r), All2 IsRoute (Code r), HasDatatypeInfo r) => FilePath -> Maybe r
gDecodeRoute fp = do
  let ctors = constructorInfo dtInfo
  basePath : restPath <- pure $ splitDirectories fp
  r <- mcana_NS @IsRouteProd @_ @_ @(NP I) @(Code r) Proxy (go basePath restPath) ctors
  pure $ to $ SOP r
  where
    dtInfo = datatypeInfo (Proxy @r)
    dtName = datatypeName dtInfo
    go :: forall y ys. (IsRouteProd y) => FilePath -> [FilePath] -> NP ConstructorInfo (y ': ys) -> Either (Maybe (NP I y)) (NP ConstructorInfo ys)
    go base rest (p :* ps) =
      fromMaybe (Right ps) $ do
        let cname = constructorName p
            ctorSuffix =
              maybe (error "ctor: bad naming") (T.unpack . T.toLower) $
                T.stripPrefix (T.pack $ dtName <> "_") (T.pack cname)
        -- FIXME: term case
        Left <$> case sList @y of
          SNil -> do
            guard $ ctorSuffix <> ".html" == base
            guard $ null rest
            pure $ pure Nil
          SCons -> do
            guard $ ctorSuffix == base
            let np = mcana_NP @_ @_ @_ @I (Proxy @IsRoute) f Proxy
            pure np
      where
        f :: forall y1 ys1. (IsRoute y1, SListI ys1) => Proxy (y1 ': ys1) -> Maybe (I y1, Proxy ys1)
        f Proxy = case sList @ys1 of
          SNil -> do
            guard $ not $ null rest
            r' <- decodeRoute @y1 $ joinPath rest
            pure (I r', Proxy)
          SCons -> Nothing

-- | Like mcana_NS but returns a Maybe
mcana_NS ::
  forall c proxy s f xs.
  (All c xs) =>
  proxy c ->
  (forall y ys. c y => s (y ': ys) -> Either (Maybe (f y)) (s ys)) ->
  s xs ->
  Maybe (NS f xs)
mcana_NS _ decide = go sList
  where
    go :: forall ys. (All c ys) => SList ys -> s ys -> Maybe (NS f ys)
    go SNil _ = Nothing
    go SCons s = case decide s of
      Left x -> Z <$> x
      Right s' -> S <$> go sList s'

mcana_NP ::
  forall c proxy s f xs.
  (All c xs) =>
  proxy c ->
  (forall y ys. (c y, SListI ys) => s (y ': ys) -> Maybe (f y, s ys)) ->
  s xs ->
  Maybe (NP f xs)
mcana_NP _ uncons = go sList
  where
    go :: forall ys. (All c ys) => SList ys -> s ys -> Maybe (NP f ys)
    go SNil _ = pure Nil
    go SCons s = do
      (x, s') <- uncons s
      xs <- go sList s'
      pure $ x :* xs
