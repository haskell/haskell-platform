% Haskell: Batteries Included
%

[The Platform Specification] is a cabal file stating required tools and
packages. The content of the platform is summarised below:

[The Platform Specification]: http://hackage.haskell.org/platform/2009.2.0/haskell-platform.cabal

Compiler and Tools
------------------

 * [GHC] 6.10.2
        - The state-of-the-art optimzing compiler for Haskell.
 * [GHCi] 6.10.2
        - A bytecode interpreter and interactive environment for Haskell
 * [The GHCi debugger]
        - An interactive, imperative-style debugger for Haskell
 * [The GHC parallel runtime] - a multicore language runtime, featuring
   lightweight threads, thread sparks, affinity control, and a parallel garbage collector
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
[GHCi]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html
[The GHCi debugger]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci-debugger.html
[The GHC parallel runtime]: http://www.haskell.org/ghc/docs/latest/html/users_guide/lang-parallel.html
[Happy]: http://haskell.org/happy
[Alex]: http://haskell.org/alex
[Haddock]: http://haskell.org/haddock
[Cabal]: http://haskell.org/cabal/

Packages and Documentation
--------------------------

*  [base],  [array],  [bytestring],  [Cabal],  [containers],  [directory],  [editline],  [filepath],  ghc-prim,  [haskell98],  [hpc],  integer,  [old-locale],  [old-time],  [packedstring],  [pretty],  [process],  [random],  [syb],  [template-haskell],  [unix],  [Win32],  [cgi],  [fgl],  [GLUT],  [haskell-src],  [html],  [HUnit],  [mtl],  [network],  [OpenGL],  [parallel],  [parsec],  [QuickCheck],  [regex-base],  [regex-compat],  [regex-posix],  [stm],  [time],  [xhtml],  [zlib],  [HTTP]

Functionality
-------------

 * The Haskell Prelude
 
 * Control Structures
    - applicatives, arrows, functors, monoids
    - synchronous and asynchronous (extensible) exceptions
    - monads: the mtl monad suite
    - foldables, traversables

 * Concurrency and Parallelism
    - lightweight threads
    - thread sparks / futures
    - software transactional memory
    - MVars: thread synchronization variables
    - channels, semaphors, sample vars

 * Data Structures
    - primitive and standard data types
    - strings
    - sequence types
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
    - weak and stable references

 * Debugging and Testing
    - tracing and debugging
    - HUnit uni testing
    - QuickCheck test generation
    - HPC code coverage framework
    - Heap profiling
    - Retainer profiling

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
    - C errno handling
    - C types
    - Haskell98 compatibility layer
    - Haskell language parsing and pretty printing

 * Network
    - Sockets, Handles
    - HTTP protocol client
    - CGI Common Gateway Interface support.
    - URI construction

 * Math
    - arbitrary precision integers and ratios
    - fixed precision numbers
    - complex numbers
    - random number generators

 * Text
    - Unicode characters
    - readp parsing combinators
    - parsec parsing combinators
    - Hughes/SPJ pretty printing combinators
    - posix regular expressions

 * System
    - file and handle abstractions
    - System IO
    - garbage collector services
    - portable process and pipe support
    - directory handling
    - OS portable filepath manipulation
    - editline console handling
    - environment handling
    - cpu time, system info, posix, timeouts
    - Unix Posix support or Win32 support
    - portable dates and calender times
    - read/show data serialisation
    - zlib compression
    - locale support
 
 * Web     
    - simple html and xhtml generation

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
