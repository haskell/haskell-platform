module OS
    ( OS(..)
    , osFromConfig
    )
  where

import OS.Internal
import OS.Mac
import OS.Posix
import OS.Win
import Types

osFromConfig :: BuildConfig -> OS
osFromConfig bc = case bcOs bc of
    "darwin" -> macOsFromConfig bc
    "mingw32" -> case bcArch bc of -- FIXME error if not one of these?
                   "i386"   -> winOsFromConfig bc
                   "x86_64" -> winOsFromConfig bc
                   _        -> winOsFromConfig bc
    _ -> posixOS bc
