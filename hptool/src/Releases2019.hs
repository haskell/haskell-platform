module Releases2019 where

import PlatformDB
import Types
import Releases2018

releases2019 :: [Release]
releases2019 = [hp_8_6_5]



hp_8_6_5 :: Release
hp_8_6_5 =
    (uncurry $ releaseWithMinimal "8.6.5") $ deltaFrom hp_8_6_3
        [ incGHC                            "8.6.5"
        , incGHCLib "transformers"          "0.5.6.2"
        , incGHCLib "process"               "1.6.5.0"
        ]

-- TO add: binary? semigroups? regexlib? safe? tagsoup? tagged? tasty? optparse-applicative? clock? criterion? reflection?
