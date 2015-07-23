This directory contains the mu-template files for various
Haskell Platform site designs.

  - original/        - the original site design
  - plan-a/          - the "Plan A" site design
  - plan-b/          - the "Plan B" site design
  - templates/       - mu-include files (used by all the designs for `{{> ...}}` includes)

Build all of the designs using:

    ./platform.sh <ghc-bindist-path> build-website

For the plan-a and plan-b designs there are two template files which
specify where the main haskell.org is and where the downloads are located:


  templates/haskell-org-root.mu -- the location of the haskell.org site
                                           e.g. "//haskell.org/"

  templates/downloads-root.mu   -- the location of the downloads site
                                           e.g. "//haskell.org/platform/"

These files should _not_ end in a newline since they are concatenated to build other urls.
One can use `echo -n` to avoid adding a trailing newline when setting the contents
of one of these files, e.g.:

    echo -n '//haskell.org/' > templates/haskell-org-root.mu
    echo -n '//haskell.org/platform/' > templates/downloads-root.mu
