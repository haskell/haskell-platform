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

**Download [The Haskell Platform] for your system**

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

**[GHC] 7.6.3**

The state-of-the-art optimizing native code compiler for Haskell.

**[GHCi] 7.6.3**

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

Alex is a lex-like lexer generator for Haskell.

**The hsc2hs foreign language binding tool**

Often you need to call C libraries from Haskell. hsc2hs is a
preprocessor for binding Haskell to C that automates much of the work.

**The GHC Profiler**

The Platform comes with several tools for analyzing your Haskell
programs performance and behaviour. Included are time and space
profiling tools, and tools for graphically visualizing the memory use
and structure of running Haskell programs.

**Haskell Code Coverage**

The Platform provides HPC - a professional-grade tool generating code
coverage information and statistics for Haskell. Code coverage
information can tell you how good your test suite is, or what part of
your code is executing at any given time.

[GHCi]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html
[The GHCi debugger]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci-debugger.html
[The GHC parallel runtime]: http://www.haskell.org/ghc/docs/latest/html/users_guide/lang-parallel.html
[Cabal]: http://haskell.org/cabal/

Packages and Documentation
--------------------------

* [Read the documentation]

[Read the documentation]: http://lambda.haskell.org/platform/doc/current/start.html


[ghc]: http://haskell.org/ghc

[cabal-install]: http://hackage.haskell.org/package/cabal-install
[alex]: http://haskell.org/alex
[happy]: http://haskell.org/happy
[haddock]: http://haskell.org/haddock
