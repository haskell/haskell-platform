% The Haskell Platform
%

Haskell: Batteries Included
-------------------

[The Haskell Platform] is a comprehensive, robust development
environment for programming in [Haskell]. For new users the platform
makes it trivial to get up and running with a full Haskell development
environment. For experienced developers, the platform provides a
comprehensive, standard base for commercial and open source Haskell
development that maximises interoperability and stability of your code.

**Download [The Haskell Platform] for your system now!**

[The Haskell Platform]: index.html
[Haskell]: http://haskell.org

To learn more about programming in Haskell:

* Visit [haskell.org] - the center of the Haskell community, a comprehensive resource.
* Follow [Learn You a Haskell] - an online Haskell tutorial with a sense of humor.
* Or jump straight to [Real World Haskell], O'Reilly's book on professional Haskell programming.
* You can even [Try Haskell] in your browser.

[haskell.org]: http://haskell.org
[Learn You a Haskell]: http://learnyouahaskell.com
[Real World Haskell]: http://book.realworldhaskell.org
[Try Haskell]: http://tryhaskell.org

The following components are provided in the latest revision of The
Platform (see the [Changelog]):

[Changelog]: changelog.html

Compiler and Runtime
--------

**[GHC] 6.12.1**

The state-of-the-art optimzing native code compiler for Haskell.

**[GHCi] 6.12.1**

A bytecode interpreter and interactive REPL environment for Haskell

**The GHC runtime**

A multicore language runtime (virtual machine), providing
fast lightweight threads, parallel sparks and futures, software
transactional memory, core affinity control, a parallel garbage
collector, and much more.

Developer Tools
-----------

The Platform also comes with the most useful developer tools out of the
box, including:

**[Cabal]**

Cabal and cabal-install are tools for building and distributing Haskell
libraries and programs. With cabal-install you have immediate access
to thousands of Haskell libraries and tools on [Hackage] -- you'll be
sure to find something interesting.

**[Haddock]**

Haddock is a high quality documentation tool for Haskell. Comments and
types in your code are used to generate indexed and cross-referenced
online documentation.

**[The GHCi debugger]**

The Platform ships with the GHCi debugger - an interactive,
imperative-style debugger for Haskell. Type ":help" in GHCi for more
information.

**The [Happy] parser generator**

Happy is a yacc-like parser generator for Haskell for constructing
efficient parsers.

**The [Alex] lexer generator**

Alex is a lex-like lexer generator for Haskell

**The hsc2hs foreign language binding tool**

Often you need to call C libraries from Haskell. hsc2hs is a
preprocessor for binding Haskell to C, that automates much of the work.

**The GHC Profiler**

The Platform comes with several tools for analyzing your Haskell
programs performance and behaviour. Included are time and space
profiling tools, and tools for graphically visualizing the memory use
and structure of running Haskell programs.

**Haskell Code Coverage**

The Platform provides HPC - a professional-grade tool generating code
coverage information and statistics for Haskell. Code coverage
information can tell you how good your testsuite is, or what part of
your code is executing at any given time.

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

The following libraries are provided by default:

*  [base],  [array],  [bytestring],  [Cabal],  [containers], [directory],  [filepath],  [haskell98],  [hpc],  [old-locale], [time], [old-time],  [pretty],  [process],  [random],  [syb],  [template-haskell],  [unix],  [Win32],  [cgi],  [fgl],  [GLUT],  [haskell-src],  [html],  [HUnit],  [mtl],  [network],  [OpenGL],  [parallel],  [parsec],  [QuickCheck],  [regex-base],  [regex-compat],  [regex-posix],  [stm],   [xhtml],  [zlib],  [HTTP], deepseq

Functionality
-------------

The Haskell Platform comes with the "batteries" installed, with the following
rich set of features for programmers to use:

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
    - HUnit unit testing
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
