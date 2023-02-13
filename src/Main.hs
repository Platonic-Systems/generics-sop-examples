module Main where

import Control.Monad (forM_, unless)
import Equality (These (That, These, This), geq, nc_geq)
import RouteEncoding
  ( BlogRoute (..),
    IsRoute (decodeRoute, encodeRoute),
    PostSlug (PostSlug),
    Route (..),
  )
import Test.QuickCheck (quickCheck)

prop_routeEncodingIsoMorphism :: Route -> Bool
prop_routeEncodingIsoMorphism r =
  let fp = encodeRoute r
   in decodeRoute fp == Just r

main :: IO ()
main = do
  putStrLn "# Equality"
  putStrLn "Non-combinator version:"
  print $ nc_geq (This True) (That False)
  print $ nc_geq (These 42 "Hello") (These 42 "Hello" :: These Int String)
  putStrLn "Combinator version:"
  print $ geq (This True) (That False)
  print $ geq (These 42 "Hello") (These 42 "Hello" :: These Int String)

  putStrLn "# RouteEncoding"
  let r = Route_Blog $ BlogRoute_Post $ PostSlug "hello"
  print r
  putStrLn $ encodeRoute r
  putStrLn $ encodeRoute Route_Index
  putStrLn $ encodeRoute $ Route_Blog BlogRoute_Index
  putStrLn "# RouteEncoding - decodeRoute"
  print $ decodeRoute @Route "blog/post/hello.html"
  print $ decodeRoute @Route "index.html"
  putStrLn "# RouteEncoding - iso"
  forM_ ([Route_Index] <> fmap Route_Blog [BlogRoute_Index, BlogRoute_Post "foo"]) $ \r -> do
    putStrLn $ "### " <> show r
    putStrLn $ encodeRoute r
    let r' = decodeRoute @Route (encodeRoute r)
    print r'
    unless (Just r == r') $ do
      error "non"

  putStrLn "# QuickCheck"
  quickCheck prop_routeEncodingIsoMorphism
