#! /usr/bin/env python

import os
import os.path
import fileinput

# Change this to the appropriate version.
DIR = "C:\\ghc\\ghc-7.4.2\\lib\\package.conf.d"

FILES = [os.path.join(DIR, f) for f in os.listdir(DIR) if f.endswith("conf")]

for line in fileinput.FileInput(FILES, inplace=1):
    print line.replace("C:\\\\Program Files (x86)\\\\Haskell",
                       "$topdir\\\\extralibs").replace(
        "C:\\\\Documents and Settings\\\\hatemachine\\\\Application Data" \
            "\\\\cabal",
        "$topdir\\\\extralibs"),
