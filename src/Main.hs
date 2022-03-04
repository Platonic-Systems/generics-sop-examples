module Main where

import Equality (These (That, These, This), geq, nc_geq)
import RouteEncoding
  ( BlogRoute (BlogRoute_Index, BlogRoute_Post),
    IsRoute (encodeRoute),
    PostSlug (PostSlug),
    Route (Route_Blog, Route_Index),
  )

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
