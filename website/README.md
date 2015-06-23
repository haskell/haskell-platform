This directory contains the mu-template files for various
Haskell Platform site designs.

  - original/        - the original site design
  - plan-a/          - the "Plan A" site design
  - plan-b/          - the "Plan B" site design
  - templates/       - mu-include files (used by all the designs for `{{> ...}}` includes)

Build all of designs using:

    ./platform.sh <ghc-bindist-path> build-website

