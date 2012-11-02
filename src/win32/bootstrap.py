#! /usr/bin/env python

# Bootstrap script for the Haskell Platform Windows installer. Written in
# Python cause I can't depend on necessary Haskell libs being installed.

# TODO
# * Move the lib dir to $GHC_DIR/extralibs
# * Modify $GHC_DIR/package.conf
# * Copy $GHC_DIR to $PWD/files
# * Add icons


bootstraplibs = [("mtl", "1.1.0.2"),
                 ("parsec", "2.1.0.1"),
                 ("network", "2.2.1.7"),
                 ("HTTP", "4000.0.9"),
                 ("zlib", "0.5.2.0")]

extratools = [("alex", "2.3.3"),
              ("happy", "1.18.5"),
              ("haddock", "2.7.2"),
              ("cabal-install", "0.8.2")]

def install(lib):
    name, version = lib

def main():
    for lib in (extralibs + extratools):
        install(lib)

if __name__ == "__main__":
    main()
