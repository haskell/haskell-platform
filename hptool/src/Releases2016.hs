module Releases2016 where

import PlatformDB
import Types

releases2016 :: [Release]
releases2016 = [hp_8_0_1]


hp_8_0_1 :: Release
hp_8_0_1 =
    releaseWithMinimal "8.0.1"
        [ incGHC                            "8.0.1"

        , incGHCLib "Cabal"                 "1.24.0.0"
        , incGHCLib "array"                 "0.5.1.1"
        , incGHCLib "base"                  "4.9.0.0"
        , incGHCLib "bytestring"            "0.10.8.0"
        , incGHCLib "containers"            "0.5.7.1"
        , incGHCLib "deepseq"               "1.4.2.0"
        , incGHCLib "directory"             "1.2.6.2"
        , incGHCLib "filepath"              "1.4.1.0"
        , incGHCLib "hpc"                   "0.6.0.3"
        , incGHCLib "pretty"                "1.1.3.3"
        , incGHCLib "process"               "1.4.2.0"
        , incGHCLib "template-haskell"      "2.11.0.0"
        , incGHCLib "time"                  "1.6"
        , incGHCLib "transformers"          "0.5.2.0"
        , incGHCLib "xhtml"                 "3000.2.1"

        {- These packages are in the GHC distribution, and hence bundeled with
        the Platform. However, they are not officially part of the Platform,
        and as such, do not carry the same stability guaruntees.
        , incGHCLib "binary"                "0.8.3.0"
        , incGHCLib "ghc-prim"              "0.5.0.0"
        , incGHCLib "haskeline"             "0.7.2.3"
        , incGHCLib "hoopl"                 "3.10.2.1"
        , incGHCLib "integer-gmp"           "1.0.0.1"
        , incGHCLib "terminfo"              "0.4.0.2"
        -}

        , notWindows $  incGHCLib "unix"    "2.7.2.0"


        --, onlyWindows $ incGHCLib "Win32"   "2.3.1.0"
        --, incTool "cabal-install"           "1.25.0.0"

        , incTool "alex"                    "3.1.7"
        , incTool "happy"                   "1.19.5"

        , incTool "hscolour"                "1.24"
        , incGHCTool "haddock"              "2.16.1"
        ]
        [
          incLib "async"                    "2.1.0"
        , incLib "attoparsec"               "0.13.0.2"
        , incLib "case-insensitive"         "1.2.0.6"
--        , incLib "cgi"                      "3001.2.2.2"
        , incLib "fgl"                      "5.5.2.3"
        , incLib "GLUT"                     "2.7.0.7"
        , incLib "GLURaw"                   "2.0.0.1"
        , incLib "haskell-src"              "1.0.2.0"
        , incLib "hashable"                 "1.2.4.0"
        , incLib "html"                     "1.0.1.2"
        , incLib "HTTP"                     "4000.3.3"
        , incLib "HUnit"                    "1.3.1.1"
        , incLib "network"                  "2.6.2.1"
        , incLib "OpenGL"                   "3.0.0.2"
        , incLib "OpenGLRaw"                "3.1.0.0"
        , incLib "parallel"                 "3.2.1.0"
        , incLib "parsec"                   "3.1.9"
        , incLib "regex-base"               "0.93.2"
        , incLib "regex-compat"             "0.95.1"
        , incLib "regex-posix"              "0.95.2"
        , incLib "split"                    "0.2.3"
        , incLib "stm"                      "2.4.4.1"
        , incLib "syb"                      "0.6"
        , incLib "text"                     "1.2.2.1"
        , incLib "unordered-containers"     "0.2.7.0"
        , incLib "vector"                   "0.11.0.0"
        , incLib "zlib"                     "0.6.1.1"

        --needed for happy or alex but otherwise would be in full only
        , incLib "mtl"                      "2.2.1"
        , incLib "primitive"                "0.6.1.0"
        , incLib "random"                   "1.1"
        , incLib "QuickCheck"               "2.8.2"

        -- Libs required by newer version of stuff - but not cleared for HP

        -- needed by alex & QuickCheck
        , incLib "tf-random"                "0.5"

        -- was split out of network, so was in HP, just under different pacakge
        , incLib "network-uri"              "2.6.1.0"

        -- needed by attoparsec
        , incLib "scientific"               "0.3.4.6"

        -- needed by OpenGL
        , incLib "ObjectName"               "1.1.0.1"
        , incLib "StateVar"                 "1.1.0.4"
        , incLib "half"                     "0.2.2.3"
        , incLib "fixed"                    "0.2.1.1"
        ]
