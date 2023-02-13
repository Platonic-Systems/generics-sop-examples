{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# HLINT ignore "Use camelCase" #-}

module RouteEncoding where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.SOP.Constraint (SListIN)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics qualified as GHC
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Generics.SOP
import System.FilePath (joinPath, splitDirectories, splitExtension, (</>))
import Test.QuickCheck

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

instance Arbitrary PostSlug where
  arbitrary = PostSlug . T.pack <$> listOf1 (elements ['a' .. 'z'])

-- TODO (exercise for the reader!): generate these generically
instance Arbitrary BlogRoute where
  arbitrary = oneof [pure BlogRoute_Index, BlogRoute_Post <$> arbitrary, BlogRoute_Qux <$> arbitrary]

instance Arbitrary Route where
  arbitrary = oneof [pure Route_Index, Route_Blog <$> arbitrary]

-- Class of routes that can be encoded to a filename.
class IsRoute r where
  -- | Encode a route to file path on disk.
  encodeRoute :: r -> FilePath
  default encodeRoute ::
    (Generic r, All2 IsRoute (Code r), All IsRouteProd (Code r), HasDatatypeInfo r) =>
    r ->
    FilePath
  encodeRoute = gEncodeRoute

  -- | Decode a route from its encoded filepath
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

-- | Like `HCollapse`, but limited to 0 or 1 products in a n-ary structure.
class HCollapseMaybe h xs where
  hcollapseMaybe :: SListIN h xs => h (K a) xs -> Maybe a

instance HCollapseMaybe NP '[] where
  hcollapseMaybe Nil = Nothing

instance HCollapseMaybe NP '[p] where
  hcollapseMaybe (K x :* Nil) = Just x

instance (ps ~ TypeError ('Text "Expected at most 1 product")) => HCollapseMaybe NP (p ': p1 ': ps) where
  hcollapseMaybe _ = Nothing -- Unreachable, due to TypeError

class (All IsRoute xs, HCollapseMaybe NP xs) => IsRouteProd xs

instance (All IsRoute xs, HCollapseMaybe NP xs) => IsRouteProd xs

gEncodeRoute :: forall r. (Generic r, All2 IsRoute (Code r), All IsRouteProd (Code r), HasDatatypeInfo r) => r -> FilePath
gEncodeRoute x = gEncodeRoute' @r (from x)

gEncodeRoute' :: forall r. (All2 IsRoute (Code r), All IsRouteProd (Code r), HasDatatypeInfo r) => SOP I (Code r) -> FilePath
gEncodeRoute' (SOP x) =
  let ctorSuffix = ctorStripPrefix @r ctorName
   in case hcollapse $ hcmap (Proxy @IsRouteProd) encProd x of
        Nothing -> ctorSuffix <> ".html"
        Just p -> ctorSuffix </> p
  where
    encProd :: (IsRouteProd xs) => NP I xs -> K (Maybe FilePath) xs
    encProd =
      K . hcollapseMaybe . hcmap (Proxy @IsRoute) encTerm
    encTerm :: IsRoute b => I b -> K FilePath b
    encTerm =
      K . encodeRoute . unI
    ctorName :: ConstructorName
    ctorName =
      hcollapse $
        hzipWith
          (\c _ -> K (constructorName c))
          (datatypeCtors @r)
          x

datatypeCtors :: forall a. HasDatatypeInfo a => NP ConstructorInfo (Code a)
datatypeCtors = constructorInfo $ datatypeInfo (Proxy @a)

ctorStripPrefix :: forall a. HasDatatypeInfo a => ConstructorName -> String
ctorStripPrefix ctorName =
  let name = datatypeName $ datatypeInfo (Proxy @a)
   in maybe (error "ctor: bad naming") (T.unpack . T.toLower) $
        T.stripPrefix (T.pack $ name <> "_") (T.pack ctorName)

gDecodeRoute :: forall r. (Generic r, All IsRouteProd (Code r), All2 IsRoute (Code r), HasDatatypeInfo r) => FilePath -> Maybe r
gDecodeRoute fp = do
  basePath : restPath <- pure $ splitDirectories fp
  -- Build the sum using an anamorphism
  to . SOP
    <$> mcana_NS @IsRouteProd @_ @_ @(NP I)
      Proxy
      (anamorphismSum basePath restPath)
      (datatypeCtors @r)
  where
    anamorphismSum :: forall xs xss. IsRouteProd xs => FilePath -> [FilePath] -> NP ConstructorInfo (xs ': xss) -> Either (Maybe (NP I xs)) (NP ConstructorInfo xss)
    anamorphismSum base rest (p :* ps) =
      fromMaybe (Right ps) $ do
        let ctorSuffix = ctorStripPrefix @r (constructorName p)
        Left <$> case sList @xs of
          SNil -> do
            -- Constructor without arguments
            guard $ ctorSuffix <> ".html" == base && null rest
            pure $ Just Nil
          SCons -> do
            -- Constructor with an argument
            guard $ ctorSuffix == base
            pure $
              mcana_NP @_ @_ @_ @I
                (Proxy @IsRoute)
                anamorphismProduct
                Proxy
      where
        anamorphismProduct :: forall y1 ys1. (IsRoute y1, SListI ys1) => Proxy (y1 ': ys1) -> Maybe (I y1, Proxy ys1)
        anamorphismProduct Proxy = case sList @ys1 of
          SNil -> do
            -- Recurse into the only product argument
            guard $ not $ null rest
            r' <- decodeRoute @y1 $ joinPath rest
            pure (I r', Proxy)
          SCons ->
            -- Not reachable, due to HCollapseMaybe constraint
            Nothing

-- | Like `mcana_NS` but returns a Maybe
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

-- | Like `cana_NP` but returns a Maybe
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
