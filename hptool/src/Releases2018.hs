module Releases2018 where

import PlatformDB
import Types

releases2018 :: [Release]
releases2018 = [hp_8_4_2, hp_8_4_3]


hp_8_4_2 :: Release
hp_8_4_2 =
    releaseWithMinimal "8.4.2"
        [ incGHC                            "8.4.2"

        , incGHCLib "Cabal"                 "2.2.0.1"
        , incGHCLib "array"                 "0.5.2.0"
        , incGHCLib "base"                  "4.11.1.0"
        , incGHCLib "bytestring"            "0.10.8.2"
        , incGHCLib "containers"            "0.5.11.0"
        , incGHCLib "deepseq"               "1.4.3.0"
        , incGHCLib "directory"             "1.3.1.5"
        , incGHCLib "filepath"              "1.4.2"
        , incGHCLib "hpc"                   "0.6.0.3"
        , incGHCLib "mtl"                   "2.2.2"
        , incGHCLib "parsec"                "3.1.13.0"
        , incGHCLib "pretty"                "1.1.3.6"
        , incGHCLib "process"               "1.6.3.0"
        , incGHCLib "stm"                   "2.4.5.0"
        , incGHCLib "template-haskell"      "2.13.0.0"
        , incGHCLib "text"                  "1.2.3.0"
        , incGHCLib "time"                  "1.8.0.2"
        , incGHCLib "transformers"          "0.5.5.0"
        , incGHCLib "xhtml"                 "3000.2.2.1"

        {- These packages are in the GHC distribution, and hence bundled with
        the Platform. However, they are not officially part of the Platform,
        and as such, do not carry the same stability guaruntees.
        , incGHCLib "binary"                "0.8.5.1"
        , incGHCLib "ghc-compact"           "0.1.0.0"
        , incGHCLib "ghc-prim"              "0.5.2.0"
        , incGHCLib "haskeline"             "0.7.4.2"
        , incGHCLib "hoopl"                 "3.10.2.2"
        , incGHCLib "integer-gmp"           "1.0.2.0"
        , incGHCLib "terminfo"              "0.4.1.0"
        -}

        , notWindows $  incGHCLib "unix"    "2.7.2.2"


        --, onlyWindows $ incGHCLib "Win32"   "2.6.1.0"

        , incTool "alex"                    "3.2.4"
        , incTool "happy"                   "1.19.9"

        , incTool "hscolour"                "1.24.4"
        , incGHCTool "haddock"              "2.19.0.1"
        , incGHCTool "cabal-install"        "2.2.0.0"
        , incGHCTool "stack"                "1.7.1"
        ]
        [
          incLib "async"                    "2.2.1"
        , incLib "attoparsec"               "0.13.2.2"
        , incLib "case-insensitive"         "1.2.0.10"
        , incLib "fgl"                      "5.6.0.0"
        , incLib "GLUT"                     "2.7.0.12"
        , incLib "GLURaw"                   "2.0.0.3"
        , incLib "haskell-src"              "1.0.3.0"
        , incLib "hashable"                 "1.2.7.0"
        , incLib "html"                     "1.0.1.2"
        , incLib "HTTP"                     "4000.3.11"
        , incLib "HUnit"                    "1.6.0.0"
        , incLib "network"                  "2.6.3.5"
        , incLib "OpenGL"                   "3.0.2.0"
        , incLib "OpenGLRaw"                "3.2.7.0"
        , incLib "parallel"                 "3.2.1.1"
        , incLib "regex-base"               "0.93.2"
        , incLib "regex-compat"             "0.95.1"
        , incLib "regex-posix"              "0.95.2"
        , incLib "split"                    "0.2.3.3"
        , incLib "syb"                      "0.7"
        , incLib "unordered-containers"     "0.2.9.0"
        , incLib "vector"                   "0.12.0.1"
        , incLib "zlib"                     "0.6.2"

        --needed for happy or alex
        , incLib "primitive"                "0.6.3.0"
        , incLib "random"                   "1.1"
        , incLib "QuickCheck"               "2.11.3"

        -- Libs required by newer version of stuff - but not cleared for HP

        -- needed by alex & QuickCheck
        , incLib "tf-random"                "0.5"

        -- was split out of network, so was in HP, just under different pacakge
        , incLib "network-uri"              "2.6.1.0"

        -- needed by attoparsec
        , incLib "scientific"               "0.3.5.2"
        , incLib "integer-logarithms"       "1.0.2.1"

        -- needed by OpenGL
        , incLib "ObjectName"               "1.1.0.1"
        , incLib "StateVar"                 "1.1.0.4"
        , incLib "half"                     "0.2.2.3"
        , incLib "fixed"                    "0.2.1.1"

        -- needed by HUnit
        , incLib "call-stack"               "0.1.0"
        ]

hp_8_4_3 :: Release
hp_8_4_3 =
    (uncurry $ releaseWithMinimal "8.4.3") $ deltaFrom hp_8_4_2
        [ incGHC                            "8.4.3"
        , incLib "primitive"                "0.6.4.0"
        ]

hp_8_6_3 :: Release
hp_8_6_3 =
    releaseWithMinimal "8.6.3"
        [ incGHC                            "8.6.3"

        , incGHCLib "Cabal"                 "2.4.0.1"
        , incGHCLib "array"                 "0.5.3.0"
        , incGHCLib "base"                  "4.12.0.0"
        , incGHCLib "bytestring"            "0.10.8.2"
        , incGHCLib "containers"            "0.6.0.1"
        , incGHCLib "deepseq"               "1.4.4.0"
        , incGHCLib "directory"             "1.3.3.0"
        , incGHCLib "filepath"              "1.4.2.1"
        , incGHCLib "hpc"                   "0.6.0.3"
        , incGHCLib "mtl"                   "2.2.2"
        , incGHCLib "parsec"                "3.1.13.0"
        , incGHCLib "pretty"                "1.1.3.6"
        , incGHCLib "process"               "1.6.3.0"
        , incGHCLib "stm"                   "2.4.5.0"
        , incGHCLib "template-haskell"      "2.14.0.0"
        , incGHCLib "text"                  "1.2.3.1"
        , incGHCLib "time"                  "1.8.0.2"
        , incGHCLib "transformers"          "0.5.5.0"
        , incGHCLib "xhtml"                 "3000.2.2.1"

        {- These packages are in the GHC distribution, and hence bundled with
        the Platform. However, they are not officially part of the Platform,
        and as such, do not carry the same stability guaruntees.
        , incGHCLib "binary"                "0.8.6.0"
        , incGHCLib "ghc-compact"           "0.1.0.0"
        , incGHCLib "ghc-prim"              "0.5.3"
        , incGHCLib "haskeline"             "0.7.4.3"
        , incGHCLib "hoopl"                 "3.10.2.2"
        , incGHCLib "integer-gmp"           "1.0.2.0"
        , incGHCLib "terminfo"              "0.4.1.0"
        -}

        , notWindows $  incGHCLib "unix"    "2.7.2.2"


        --, onlyWindows $ incGHCLib "Win32"   "2.6.1.0"

        , incTool "alex"                    "3.2.4"
        , incTool "happy"                   "1.19.9"

        , incTool "hscolour"                "1.24.4" --drop it?
        , incGHCTool "haddock"              "2.21.0" --Correct?
        , incGHCTool "cabal-install"        "2.4.1.0"
        , incGHCTool "stack"                "1.9.1"
        ]
        [
          incLib "async"                    "2.2.1"
        , incLib "attoparsec"               "0.13.2.2"
        , incLib "case-insensitive"         "1.2.0.11"
        , incLib "fgl"                      "5.7.0.1"
        , incLib "GLUT"                     "2.7.0.14"
        , incLib "GLURaw"                   "2.0.0.4"
        , incLib "haskell-src"              "1.0.3.0"
        , incLib "hashable"                 "1.2.7.0"
        , incLib "html"                     "1.0.1.2"
        , incLib "HTTP"                     "4000.3.12"
        , incLib "HUnit"                    "1.6.0.0"
        , incLib "network"                  "2.8.0.0"
        , incLib "OpenGL"                   "3.0.2.2"
        , incLib "OpenGLRaw"                "3.3.1.0"
        , incLib "parallel"                 "3.2.2.0"
        , incLib "regex-base"               "0.93.2"
        , incLib "regex-compat"             "0.95.1"
        , incLib "regex-posix"              "0.95.2"
        , incLib "split"                    "0.2.3.3"
        , incLib "syb"                      "0.7"
        , incLib "unordered-containers"     "0.2.9.0"
        , incLib "vector"                   "0.12.0.1"
        , incLib "zlib"                     "0.6.2"

        --needed for happy or alex
        , incLib "primitive"                "0.6.4.0"
        , incLib "random"                   "1.1"
        , incLib "QuickCheck"               "2.12.6.1"

        -- Libs required by newer version of stuff - but not cleared for HP

        -- needed by alex & QuickCheck
        , incLib "tf-random"                "0.5"

        -- was split out of network, so was in HP, just under different pacakge
        , incLib "network-uri"              "2.6.1.0"

        -- needed by attoparsec
        , incLib "scientific"               "0.3.6.2"
        , incLib "integer-logarithms"       "1.0.2.2"

        -- needed by OpenGL
        , incLib "ObjectName"               "1.1.0.1"
        , incLib "StateVar"                 "1.1.1.1"
        , incLib "half"                     "0.3"
        , incLib "fixed"                    "0.2.1.1"

        -- needed by HUnit
        , incLib "call-stack"               "0.1.0"
        ]

-- TO add: binary? semigroups? regexlib? safe? tagsoup? tagged? tasty? optparse-applicative? clock? criterion? reflection?
