module OS
    ( OS(..)
    , osFromConfig
    )
  where

import OS.Internal
import OS.Mac
import Types

osFromConfig :: BuildConfig -> OS
osFromConfig bc = case bcOs bc of
    "darwin" -> macOsFromConfig bc
    _ -> genericOS bc
