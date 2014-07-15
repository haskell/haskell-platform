module Releases
    ( module Releases2012
    , module Releases2013
    , module Releases2014
    , releases
    )

where

import Releases2012
import Releases2013
import Releases2014
import Types

releases :: [Release]
releases = concat
    [ releases2012
    , releases2013
    , releases2014
    ]
