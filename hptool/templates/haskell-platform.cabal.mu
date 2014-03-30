name:                haskell-platform
version:             {{hpVersion}}
homepage:            http://haskell.org/platform
license:             BSD3
license-file:        LICENSE
author:              libraries@haskell.org
maintainer:          haskell-platform@projects.haskell.org
category:            System
synopsis:            The Haskell Platform
description:
    The Haskell Platform (HP) is the blessed set of libraries and tools on
    which to build further Haskell libraries and applications. It is
    intended to provide a comprehensive, stable, and quality tested base for
    Haskell projects to work from.
    .
    This version specifies the following additional developer tools be
    installed, for a system to be in full compliance:
    .
    * cabal-install
    * alex
    * happy
    * haddock

cabal-version:       >= 1.8
build-type:          Custom
tested-with:         GHC =={{ghcVersion}}

flag include-ghc-depends
  description: Include all the GHC provided packages in the dependencies
  default:     False

library
  if flag(include-ghc-depends)
    build-depends:
      ghc =={{ghcVersion}},

      -- Core libraries: provided by every ghc installation
      -- We don't include "non-API" packages here.
{{#ghcLibs}}
      {{name}} =={{version}}{{comma}}
{{/ghcLibs}}
  if !os(windows)
    build-depends:
{{#nonWindowsLibs}}
      {{name}} =={{version}}{{comma}}
{{/nonWindowsLibs}}
  else
    build-depends:
{{#onlyWindowsLibs}}
      {{name}} =={{version}}{{comma}}
{{/onlyWindowsLibs}}

  build-depends:
    -- Libraries in addition to what GHC provides:
    -- Note: newer versions of cgi need monad-catchio.
{{#platformLibs}}
      {{name}} =={{version}}{{comma}}
{{/platformLibs}}

    -- Libraries that are needed to support the above,
    -- though are not officially part of the platform
-- FIXME: should have some separate designation?

  -- Depending on programs does not work, they are not registered
  -- We list them to help distro packaging.
  build-tools:
{{#tools}}
      {{name}} =={{version}}{{comma}}
{{/tools}}
