module Releases2013 where

import PlatformDB
import Types

releases2013 :: [Release]
releases2013 = [hp2013_2_0_0]

hp2013_2_0_0 :: Release
hp2013_2_0_0 =
    release "2013.2.0.0"
        [ incGHC                            "7.6.2"
        , incGHCLib "array"                 "0.4.0.1"

        , incGHCLib "base"                  "4.6.0.1"
        , incGHCLib "bytestring"            "0.10.0.2"
        , incGHCLib "Cabal"                 "1.16.0"
        , incGHCLib "containers"            "0.5.0.0"
        , incGHCLib "deepseq"               "1.3.0.1"
        , incGHCLib "directory"             "1.2.0.1"
        , incGHCLib "filepath"              "1.3.0.1"
        , incGHCLib "haskell2010"           "1.1.1.0"
        , incGHCLib "haskell98"             "2.0.0.2"
        , incGHCLib "hpc"                   "0.6.0.0"
        , incGHCLib "old-locale"            "1.0.0.5"
        , incGHCLib "old-time"              "1.1.0.1"
        , incGHCLib "pretty"                "1.1.1.0"
        , incGHCLib "process"               "1.1.0.2"
        , incGHCLib "template-haskell"      "2.8.0.0"
        , incGHCLib "time"                  "1.4.0.1"

        , notWindows $  incGHCLib "unix"    "2.6.0.1"

        , onlyWindows $ incGHCLib "Win32"   "2.3.0.0"

        , incLib "async"                    "2.0.1.4"
        , incLib "attoparsec"               "0.10.4.0"
        , incLib "case-insensitive"         "1.0.0.1"
        , incLib "cgi"                      "3001.1.7.5"
        , incLib "fgl"                      "5.4.2.4"
        , incLib "GLUT"                     "2.4.0.0"
        , incLib "GLURaw"                   "1.3.0.0"
        , incLib "haskell-src"              "1.0.1.5"
        , incLib "hashable"                 "1.1.2.5"
        , incLib "html"                     "1.0.1.2"
        , incLib "HTTP"                     "4000.2.8"
        , incLib "HUnit"                    "1.2.5.2"
        , incLib "mtl"                      "2.1.2"
        , incLib "network"                  "2.4.1.2"
        , incLib "OpenGL"                   "2.8.0.0"
        , incLib "OpenGLRaw"                "1.3.0.0"
        , incLib "parallel"                 "3.2.0.3"
        , incLib "parsec"                   "3.1.3"
        , incLib "QuickCheck"               "2.6"
        , incLib "random"                   "1.0.1.1"
        , incLib "regex-base"               "0.93.2"
        , incLib "regex-compat"             "0.95.1"
        , incLib "regex-posix"              "0.95.2"
        , incLib "split"                    "0.2.2"
        , incLib "stm"                      "2.4.2"
        , incLib "syb"                      "0.4.0"
        , incLib "text"                     "0.11.3.1"
        , incLib "transformers"             "0.3.0.0"
        , incLib "unordered-containers"     "0.2.3.0"
        , incLib "vector"                   "0.10.0.1"
        , incLib "xhtml"                    "3000.2.1"
        , incLib "zlib"                     "0.5.4.1"
        , incLib "primitive"                "0.5.0.1"

        , incTool "cabal-install"           "0.16.0.2"
        , incTool "alex"                    "3.0.5"
        , incTool "happy"                   "1.18.10"
        , incTool "haddock"                 "2.13.2"
        ]
