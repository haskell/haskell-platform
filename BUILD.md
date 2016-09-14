
Synopsis of build instructions:

1. Use `find-bindist` to download a GHC bindist:

        $ ./find-bindist apple
        ... url displayed ...
        $ wget ...url...

2. If necessary, build `cabal` version >= 1.24:

        $ cabal get cabal-install-1.24.0.0
        $ cd cabal-install-1.24.0.0
        $ cabal sandbox init
        $ cabal install --only-dependencies
        $ cabal install
        $ cp ./dist/dist-sandbox-.../build/cabal/cabal /usr/local/bin/cabal

3. Use `platform-init` to initialize the `config.platform` file:

        $ ./platform-init

    Make sure the version of cabal is >= 1.24. Edit entries as needed.

4. Run `./new-platform.sh`

        $ ./new-platform.sh build-all
        $ ./new-platform.sh build-website

    The website files reside in `build/product/website`.

5. In a new terminal, start up the python server:

        $ ./server ./build/product/website 12345

    You can leave this running - it will track changes as you re-build
    the website.

