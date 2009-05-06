% Haskell: Batteries Included
%

[The Platform Specification] is a cabal file stating required tools and
packages. There content is summarised below:

[The Platform Specification]: http://hackage.haskell.org/platform/2009.2.0/haskell-platform.cabal

Compiler and Tools
------------------

 * [GHC] 6.10.2
        - The state-of-the-art optimzing compiler for Haskell.
 * GHCi 6.10.2
        - A bytecode interpreter and interactive environment for Haskell
 * The GHCi debugger
        - An interactive, imperative-style debugger for Haskell
 * The GHC parallel runtime - a multicore-ready runtime, featuring
   lightweight threads, thread sparks, affinity control, and a parallel
   garbage collector
 * The [Happy] parser generator
        - Happy 1.18.2, a yacc-like parser generator for Haskell
 * The [Alex] lexer generator
        - Alex 2.3.1, a lex-like lexer generator for Haskell
 * The [Haddock] Documentation tool
        - generator   Haddock 2.4.2
 * The [Cabal] package distribution tool
        - cabal-install 0.6.2, download and install new Haskell packages from [Hackage]
 * The hsc2hs foreign language binding tool
        - a preprocessor for binding Haskell to C

[GHC]: http://haskell.org/ghc
[Happy]: http://haskell.org/happy
[Alex]: http://haskell.org/alex
[Haddock]: http://haskell.org/haddock
[Cabal]: http://haskell.org/cabal/

Packages
--------

*  [base],  [array],  [bytestring],  [Cabal],  [containers],  [directory],  [editline],  [filepath],  ghc-prim,  [haskell98],  [hpc],  integer,  [old-locale],  [old-time],  [packedstring],  [pretty],  [process],  [random],  [syb],  [template-haskell],  [unix],  [Win32],  [cgi],  [fgl],  [GLUT],  [haskell-src],  [html],  [HUnit],  [mtl],  [network],  [OpenGL],  [parallel],  [parsec],  [QuickCheck],  [regex-base],  [regex-compat],  [regex-posix],  [stm],  [time],  [xhtml],  [zlib],  [HTTP]

Functionality
-------------

 * Control Structures
    - applicatives, arrows, monoids
    - synchronous and asynchronous (extensible) exceptions
    - monads: the mtl monad suite
    - foldables, traversables

 * Concurrency and Parallelism
    - lightweight threads
    - thread sparks / futures
    - software transactional memory
    - MVars
    - channels, semaphors, sample vars

 * Data Structures
    - basic atomic types
    - pure, impure arrays, boxed/unboxed, foreign, storable, ST, diff arrays
    - strict and lazy bytestrings
    - regular, and inductive graphs
    - finite maps
    - patricia tries
    - sets
    - fingertrees
    - mutable references
    - mutable hashtables
    - dynamic types

 * Debugging and Testing
    - tracing and debugging
    - HUnit uni testing
    - QuickCheck test generation
    - HPC code coverage framework

 * Distribution
    - The Cabal distribution framework

 * Generics
    - Template Haskell
    - Scrap-Your-Boilerplate generics

 * Graphics
    - GLUT
    - OpenGL

 * Languages
    - The C FFI specification
    - Haskell98 compatibility layer

 * Network
    - sockets, handles, URIs
    - client-side HTTP 

 * Text
    - Unicode characters
    - readp parsing combinators
    - parsec parsing combinators
    - Hughes/SPJ pretty printing combinators
    - posix regular expressions

 * System
    - IO
    - portable process and pipe support
    - directory handling
    - portable filepath abstractions
    - editline console handling
    - environment handling
    - cpu time, system info, posix, timeouts
    - random number generators
    - Unix Posix support or Win32 support
    - portable time 
    - zlib encoding/decoding
 
 * Web     
    - html, xhtml generation

[base]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/base
[array]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/array
[bytestring]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/bytestring
[Cabal]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/Cabal
[containers]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/containers
[directory]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/directory
[editline]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/editline
[filepath]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/filepath
[haskell98]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/haskell98
[hpc]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hpc
[old-locale]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/old-locale
[old-time]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/old-time
[packedstring]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/packedstring
[pretty]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/pretty
[process]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/process
[random]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/random
[syb]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/syb
[template-haskell]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/template-haskell
[unix]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/unix
[win32]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/Win32
[cgi]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/cgi
[fgl]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/fgl
[parsec]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/parsec
[GLUT]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/GLUT
[haskell-src]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/haskell-src
[html]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/html
[HUnit]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/HUnit
[mtl]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/mtl
[network]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/network
[OpenGL]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/OpenGL
[parallel]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/parallel
[QuickCheck]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/QuickCheck
[regex-base]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/regex-base
[regex-compat]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/regex-compat
[regex-posix]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/regex-posix
[stm]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/stm
[time]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/time
[xhtml]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/xhtml
[zlib]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/zlib
[HTTP]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/HTTP
[Hackage]: http://hackage.haskell.org
