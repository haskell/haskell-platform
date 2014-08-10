module Releases2014 where

import PlatformDB
import Types

releases2014 :: [Release]
releases2014 = [hp2014_2_0_0]

hp2014_2_0_0 :: Release
hp2014_2_0_0 =
    release "2014.2.0.0"
        [ incGHC                            "7.8.3"
        , incGHCLib "array"                 "0.5.0.0"
        , incGHCLib "base"                  "4.7.0.1"
        , incGHCLib "bytestring"            "0.10.4.0"
        , incGHCLib "Cabal"                 "1.18.1.3"
        , incGHCLib "containers"            "0.5.5.1"
        , incGHCLib "deepseq"               "1.3.0.2"
        , incGHCLib "directory"             "1.2.1.0"
        , incGHCLib "filepath"              "1.3.0.2"
        , incGHCLib "haskell2010"           "1.1.2.0"
        , incGHCLib "haskell98"             "2.0.0.3"
        , incGHCLib "hpc"                   "0.6.0.1"
        , incGHCLib "old-locale"            "1.0.0.6"
        , incGHCLib "old-time"              "1.1.0.2"
        , incGHCLib "pretty"                "1.1.1.1"
        , incGHCLib "process"               "1.2.0.0"
        , incGHCLib "template-haskell"      "2.9.0.0"
        , incGHCLib "time"                  "1.4.2"
        , incGHCLib "transformers"          "0.3.0.0"
        , incGHCLib "xhtml"                 "3000.2.1"

        , notWindows $  incGHCLib "unix"    "2.7.0.1"
        --, onlyWindows $ incGHCLib "Win32"   "2.3.0.2"

        , incLib "async"                    "2.0.1.5"
        , incLib "attoparsec"               "0.10.4.0"
        , incLib "case-insensitive"         "1.1.0.3"
--        , incLib "cgi"                      "3001.1.7.5" -- no longer builds
        , incLib "fgl"                      "5.5.0.1"
        , incLib "GLUT"                     "2.5.1.1"
        , incLib "GLURaw"                   "1.4.0.1"
        , incLib "haskell-src"              "1.0.1.6"
        , incLib "hashable"                 "1.2.2.0"
        , incLib "html"                     "1.0.1.2"
        , incLib "HTTP"                     "4000.2.10"
        , incLib "HUnit"                    "1.2.5.2"
        , incLib "mtl"                      "2.1.3.1"
        , incLib "network"                  "2.4.2.3"
        , incLib "OpenGL"                   "2.9.2.0"
        , incLib "OpenGLRaw"                "1.5.0.0"
        , incLib "parallel"                 "3.2.0.4"
        , incLib "parsec"                   "3.1.5"
        , incLib "primitive"                "0.5.2.1"
        , incLib "QuickCheck"               "2.6"
        , incLib "random"                   "1.0.1.1"
        , incLib "regex-base"               "0.93.2"
        , incLib "regex-compat"             "0.95.1"
        , incLib "regex-posix"              "0.95.2"
        , incLib "split"                    "0.2.2"
        , incLib "stm"                      "2.4.2"
        , incLib "syb"                      "0.4.1"
        , incLib "text"                     "1.1.0.0"
        , incLib "unordered-containers"     "0.2.4.0"
        , incLib "vector"                   "0.10.9.1"
        , incLib "zlib"                     "0.5.4.1"

        , incTool "cabal-install"           "1.18.0.5"
        , incTool "alex"                    "3.1.3"
        , incTool "happy"                   "1.19.4"

        , incTool "hscolour"                "1.20.3"
        , incGHCTool "haddock"              "2.14.2"
        ]

