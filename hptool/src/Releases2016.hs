module Releases2016 where

import PlatformDB
import Types

releases2016 :: [Release]
releases2016 = [hp_8_0_0]


hp_8_0_0 :: Release
hp_8_0_0 =
    releaseWithMinimal "8.0.0"
        [ incGHC                            "7.10.3"

        , incGHCLib "Cabal"                 "1.22.5.0"
        , incGHCLib "array"                 "0.5.1.0"
        , incGHCLib "base"                  "4.8.2.0"
        , incGHCLib "bytestring"            "0.10.6.0"
        , incGHCLib "containers"            "0.5.6.2"
        , incGHCLib "deepseq"               "1.4.1.1"
        , incGHCLib "directory"             "1.2.2.0"
        , incGHCLib "filepath"              "1.4.0.0"
        , incGHCLib "hpc"                   "0.6.0.2"
        , incGHCLib "pretty"                "1.1.2.0"
        , incGHCLib "process"               "1.2.3.0"
        , incGHCLib "template-haskell"      "2.10.0.0"
        , incGHCLib "time"                  "1.5.0.1"
        , incGHCLib "transformers"          "0.4.2.0"
        , incGHCLib "xhtml"                 "3000.2.1"

        {- These packages are in the GHC distribution, and hence bundeled with
        the Platform. However, they are not officially part of the Platform,
        and as such, do not carry the same stability guaruntees.

        , incGHCLib "bin-package-db"        "0.0.0.0"
        , incGHCLib "binary"                "0.7.5.0"
        , incGHCLib "ghc-prim"              "0.4.0.0"
        , incGHCLib "haskeline"             "0.7.2.1"
        , incGHCLib "hoopl"                 "3.10.0.2"
        , incGHCLib "integer-gmp"           "1.0.0.0"
        , incGHCLib "terminfo"              "0.4.0.1"
        -}

        , notWindows $  incGHCLib "unix"    "2.7.1.0"

        --, onlyWindows $ incGHCLib "Win32"   "2.3.1.0"
        , incTool "cabal-install"           "1.22.6.0"

        , incTool "alex"                    "3.1.4"
        , incTool "happy"                   "1.19.5"

        , incTool "hscolour"                "1.23"
        , incGHCTool "haddock"              "2.16.1"

        ]
        [
          incLib "async"                    "2.0.2"
        , incLib "attoparsec"               "0.13.0.1"
        , incLib "case-insensitive"         "1.2.0.5"
        , incLib "cgi"                      "3001.2.2.2"
        , incLib "fgl"                      "5.5.2.3"
        , incLib "GLUT"                     "2.7.0.3"
        , incLib "GLURaw"                   "1.5.0.2"
        , incLib "haskell-src"              "1.0.2.0"
        , incLib "hashable"                 "1.2.3.3"
        , incLib "html"                     "1.0.1.2"
        , incLib "HTTP"                     "4000.2.20"
        , incLib "HUnit"                    "1.3.0.0"
        , incLib "mtl"                      "2.2.1"
        , incLib "network"                  "2.6.2.1"
        , incLib "OpenGL"                   "2.13.1.0"
        , incLib "OpenGLRaw"                "2.6.0.0"
        , incLib "parallel"                 "3.2.0.6"
        , incLib "parsec"                   "3.1.9"
        , incLib "primitive"                "0.6.1.0"
        , incLib "QuickCheck"               "2.8.1"
        , incLib "random"                   "1.1"
        , incLib "regex-base"               "0.93.2"
        , incLib "regex-compat"             "0.95.1"
        , incLib "regex-posix"              "0.95.2"
        , incLib "split"                    "0.2.2"
        , incLib "stm"                      "2.4.4"
        , incLib "syb"                      "0.6"
        , incLib "text"                     "1.2.1.3"
        , incLib "unordered-containers"     "0.2.5.1"
        , incLib "vector"                   "0.11.0.0"
        , incLib "zlib"                     "0.5.4.2"
            -- held back because cabal-install needs < 0.6 -- ick

        -- Libs required by newer version of stuff - but not cleared for HP
        , incLib "tf-random"                "0.5"
            -- needed by alex & QuickCheck

        -- these two were in the old HP
        , incLib "old-locale"               "1.0.0.7"
        , incLib "old-time"                 "1.1.0.3"
            -- needed by cabal-install, cgi, & HTTP

        -- was split out of network, so was in HP, just under different pacakge
        , incLib "network-uri"              "2.6.0.3"
            -- needed by cabal-install, cgi, & HTTP

        -- needed by cgi
        , incLib "exceptions"               "0.8.0.2"
        , incLib "transformers-compat"      "0.4.0.4"
        , incLib "multipart"                "0.1.2"

        -- needed by attoparsec, held back due to attoparsec
        , incLib "scientific"               "0.3.3.8"

        -- needed by OpenGL
        , incLib "ObjectName"               "1.1.0.0"
        , incLib "StateVar"                 "1.1.0.1"
        , incLib "half"                     "0.2.2.1"
        ]
