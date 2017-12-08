module Releases2017 where

import PlatformDB
import Types

releases2017 :: [Release]
releases2017 = [hp_8_2_1, hp_8_2_2]


hp_8_2_1 :: Release
hp_8_2_1 =
    releaseWithMinimal "8.2.1"
        [ incGHC                            "8.2.1"

        , incGHCLib "Cabal"                 "2.0.0.2"
        , incGHCLib "array"                 "0.5.2.0"
        , incGHCLib "base"                  "4.10.0.0"
        , incGHCLib "bytestring"            "0.10.8.2"
        , incGHCLib "containers"            "0.5.10.2"
        , incGHCLib "deepseq"               "1.4.3.0"
        , incGHCLib "directory"             "1.3.0.2"
        , incGHCLib "filepath"              "1.4.1.2"
        , incGHCLib "hpc"                   "0.6.0.3"
        , incGHCLib "pretty"                "1.1.3.3"
        , incGHCLib "process"               "1.6.1.0"
        , incGHCLib "template-haskell"      "2.12.0.0"
        , incGHCLib "time"                  "1.8.0.2"
        , incGHCLib "transformers"          "0.5.2.0"
        , incGHCLib "xhtml"                 "3000.2.2"

        {- These packages are in the GHC distribution, and hence bundled with
        the Platform. However, they are not officially part of the Platform,
        and as such, do not carry the same stability guaruntees.
        , incGHCLib "binary"                "0.8.5.1"
        , incGHCLib "ghc-prim"              "0.5.1.0"
        , incGHCLib "haskeline"             "0.7.4.0"
        , incGHCLib "hoopl"                 "3.10.2.2"
        , incGHCLib "integer-gmp"           "1.0.1.0"
        , incGHCLib "terminfo"              "0.4.1.0"
        -}

        , notWindows $  incGHCLib "unix"    "2.7.2.2"


        --, onlyWindows $ incGHCLib "Win32"   "2.5.4.1"
        --, incTool "cabal-install"           "2.0.0.0"

        , incTool "alex"                    "3.2.1"
        , incTool "happy"                   "1.19.5"

        , incTool "hscolour"                "1.24.1"
        , incGHCTool "haddock"              "2.18.1"
        , incGHCTool "cabal-install"        "2.0.0.0"
        , incGHCTool "stack"                "1.5.1"
        ]
        [
          incLib "async"                    "2.1.1.1"
        , incLib "attoparsec"               "0.13.2.0"
        , incLib "case-insensitive"         "1.2.0.10"
        , incLib "fgl"                      "5.6.0.0"
        , incLib "GLUT"                     "2.7.0.12"
        , incLib "GLURaw"                   "2.0.0.3"
        , incLib "haskell-src"              "1.0.2.0"
        , incLib "hashable"                 "1.2.6.1"
        , incLib "html"                     "1.0.1.2"
        , incLib "HTTP"                     "4000.3.7"
        , incLib "HUnit"                    "1.6.0.0"
        , incLib "network"                  "2.6.3.2"
        , incLib "OpenGL"                   "3.0.2.0"
        , incLib "OpenGLRaw"                "3.2.5.0"
        , incLib "parallel"                 "3.2.1.1"
        , incLib "parsec"                   "3.1.11"
        , incLib "regex-base"               "0.93.2"
        , incLib "regex-compat"             "0.95.1"
        , incLib "regex-posix"              "0.95.2"
        , incLib "split"                    "0.2.3.2"
        , incLib "stm"                      "2.4.4.1"
        , incLib "syb"                      "0.7"
        , incLib "text"                     "1.2.2.2"
        , incLib "unordered-containers"     "0.2.8.0"
        , incLib "vector"                   "0.12.0.1"
        , incLib "zlib"                     "0.6.1.2"

        --needed for happy or alex
        , incLib "mtl"                      "2.2.1"
        , incLib "primitive"                "0.6.2.0"
        , incLib "random"                   "1.1"
        , incLib "QuickCheck"               "2.10.0.1"

        -- Libs required by newer version of stuff - but not cleared for HP

        -- needed by alex & QuickCheck
        , incLib "tf-random"                "0.5"

        -- was split out of network, so was in HP, just under different pacakge
        , incLib "network-uri"              "2.6.1.0"

        -- needed by attoparsec
        , incLib "scientific"               "0.3.5.1"
        , incLib "integer-logarithms"       "1.0.2"

        -- needed by OpenGL
        , incLib "ObjectName"               "1.1.0.1"
        , incLib "StateVar"                 "1.1.0.4"
        , incLib "half"                     "0.2.2.3"
        , incLib "fixed"                    "0.2.1.1"

        -- needed by HUnit
        , incLib "call-stack"               "0.1.0"
        ]


hp_8_2_2 :: Release
hp_8_2_2 =
    (uncurry $ releaseWithMinimal "8.2.2") $ deltaFrom hp_8_2_1
        [ incGHC                            "8.2.2"
        , incGHCLib "Cabal"                 "2.0.1.0"
        , incGHCLib "base"                  "4.10.1.0"
        {- These packages are in the GHC distribution, and hence bundled with
        the Platform. However, they are not officially part of the Platform,
        and as such, do not carry the same stability guaruntees.
        , incGHCLib "ghc-prim"              "0.5.1.1"
        -}

        , incTool "alex"                    "3.2.3"
        , incTool "happy"                   "1.19.8"

        , incTool "hscolour"                "1.24.2"
        , incLib "HTTP"                     "4000.3.8"
        , incLib "QuickCheck"               "2.10.1"
        , incLib "scientific"               "0.3.5.2"

        , incGHCTool "cabal-install"        "2.0.0.1"
        , incGHCTool "stack"                "1.6.1"
        ]



-- TO add: binary? semigroups? regexlib? safe? tagsoup? tagged? tasty? optparse-applicative? clock? criterion? reflection?
