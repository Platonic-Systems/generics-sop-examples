{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Equality (These (That, These, This), geq, nc_geq)

main :: IO ()
main = do
  putStrLn "Non-combinator version:"
  print $ nc_geq (This True) (That False)
  print $ nc_geq (These 42 "Hello") (These 42 "Hello" :: These Int String)
  putStrLn "Combinator version:"
  print $ geq (This True) (That False)
  print $ geq (These 42 "Hello") (These 42 "Hello" :: These Int String)
