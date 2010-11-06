% The Haskell Platform: Changelog
%

Changelog for the Haskell Platform
-------------------

2010.2.0.0
----------

**Contents of the Haskell Platform** 

The contents of the Haskell Platform are specified
as [this Cabal file](http://code.haskell.org/haskell-platform/haskell-platform.cabal).

***Libraries***

 * [ghc] ==6.12.3
 * [array] ==0.3.0.1 
 * [base] ==4.2.0.2 and ==3.0.3.2
 * [bytestring] ==0.9.1.7
 * [Cabal] ==1.8.0.6
 * [containers] ==0.3.0.0
 * [directory] ==1.0.1.1
 * [extensible-exceptions] ==0.1.1.1
 * [filepath] ==1.1.0.4
 * [haskell98] ==1.0.1.1
 * [hpc] ==0.5.0.5
 * [old-locale] ==1.0.0.2
 * [old-time] ==1.0.0.5
 * [pretty] ==1.0.1.1
 * [process] ==1.0.1.3
 * [random] ==1.0.0.2
 * [syb] ==0.1.0.2
 * [template-haskell] ==2.4.0.1
 * [time] ==1.1.4
 * [unix] ==2.4.0.2
 * [Win32] ==2.2.0.1
 * [cgi] ==3001.1.7.3
 * [fgl] ==5.4.2.3
 * [GLUT] ==2.1.2.1
 * [haskell-src] ==1.0.1.3
 * [html] ==1.0.1.2
 * [HUnit] ==1.2.2.1
 * [mtl] ==1.1.0.2,
 * [network] ==2.2.1.7
 * [OpenGL] ==2.2.3.0
 * [parallel] ==2.2.0.1
 * [parsec] ==2.1.0.1
 * [QuickCheck] ==2.1.1.1
 * [regex-base] ==0.93.2
 * [regex-compat] ==0.93.1
 * [regex-posix] ==0.94.2
 * [stm] ==2.1.2.1
 * [xhtml] ==3000.2.0.1
 * [zlib] ==0.5.2.0
 * [HTTP] ==4000.0.9
 * [deepseq] ==1.1.0.0

***Programs and Tools***

 * [cabal-install] ==0.8.2
 * [alex] ==2.3.3
 * [happy] ==1.18.5
 * [haddock] ==2.7.2

**Bug fixes**

***GHC***

 * A bug which caused forking large processes to not terminate has been fixed.
 * Calling hSetEncoding will now set the encoding for both the read and write side of a Handle, rather than just the read side.
 * Using hReady and hWaitForInput on Handles created from Sockets will no longer block on Windows.
 * Some improvements have been made in when library functions do, and do not, block asynchronous exceptions.
 * A new flag -no-rtsopts flag allows you to disable parsing of +RTS options by the run-time system.  In some use-cases, when a program can be run as another user, this can cause security problems. For example, myprog -tsomefile will overwrite somefile if the other user has permissions to do so.
 * ghc-pkg now ensures that the conf files it writes are UTF8.

***Template Haskell***

 * A bug in the kind parser has been fixed.

***directory***

 * System.Directory.doesFileExist on Unix-like OSes now correctly identifies various types of special file as files.

**Package version changes since last release**

 * [ghc]            6.12.1      -> 6.12.3
 * [alex]           2.3.2       -> 2.3.3
 * [happy]          1.18.4      -> 1.18.5
 * [cabal-install]  0.8.0       -> 0.8.2
 * [Cabal]          1.8.0.2     -> 1.8.0.6
 * [QuickCheck]     2.1.0.3     -> 2.1.1.1
 * [array]          0.3.0.0     -> 0.3.0.1
 * [base]           4.2.0.0     -> 4.2.0.2
 * [bytestring]     0.9.1.5     -> 0.9.1.7
 * [cgi]            3001.1.7.2  -> 3001.1.7.3
 * [directory]      1.0.1.0     -> 1.0.1.1
 * [fgl]            5.4.2.2     -> 5.4.2.3
 * [filepath]       1.1.0.3     -> 1.1.0.4
 * [hpc]            0.5.0.4     -> 0.5.0.5
 * [old-time]       1.0.0.3     -> 1.0.0.5
 * [process]        1.0.1.2     -> 1.0.1.3
 * [regex-base]     0.93.1      -> 0.93.2
 * [regex-compat]   0.92        -> 0.93.1
 * [regex-posix]    0.94.1      -> 0.94.2
 * [stm]            2.1.1.2     -> 2.1.2.1
 * [template-haskell] 2.4.0.0    -> 2.4.0.1
 * [unix]           2.4.0.0     -> 2.4.0.2
 * Added: [extensible-exceptions] 0.1.1.1

------------------------------------------------------------------------

[base]: http://hackage.haskell.org/package/base-4.2.0.2
[array]: http://hackage.haskell.org/package/array-0.3.0.1
[bytestring]: http://hackage.haskell.org/package/bytestring-0.9.1.7
[Cabal]: http://hackage.haskell.org/package/Cabal-1.8.0.6
[cabal-install]: http://hackage.haskell.org/package/cabal-install-0.8.2
[containers]: http://hackage.haskell.org/package/containers-0.3.0.0
[directory]: http://hackage.haskell.org/package/directory-1.0.1.1
[extensible-exceptions]: http://hackage.haskell.org/package/extensible-exceptions-0.1.1.1
[filepath]: http://hackage.haskell.org/package/filepath-1.1.0.4
[haskell98]: http://hackage.haskell.org/package/haskell98-1.0.1.1
[hpc]: http://hackage.haskell.org/package/hpc-0.5.0.5
[old-locale]: http://hackage.haskell.org/package/old-locale-1.0.0.2
[old-time]: http://hackage.haskell.org/package/old-time-1.0.0.5
[packedstring]: http://hackage.haskell.org/package/packedstring
[pretty]: http://hackage.haskell.org/package/pretty-1.0.1.1
[process]: http://hackage.haskell.org/package/process-1.0.1.3
[random]: http://hackage.haskell.org/package/random-1.0.0.2
[syb]: http://hackage.haskell.org/package/syb-0.1.0.2
[template-haskell]: http://hackage.haskell.org/package/template-haskell-2.4.0.1
[unix]: http://hackage.haskell.org/package/unix-2.4.0.2
[win32]: http://hackage.haskell.org/package/Win32-2.2.0.1
[cgi]: http://hackage.haskell.org/package/cgi-3001.1.7.3
[fgl]: http://hackage.haskell.org/package/fgl-5.4.2.3
[parsec]: http://hackage.haskell.org/package/parsec-2.1.0.1
[GLUT]: http://hackage.haskell.org/package/GLUT-2.1.2.1
[haskell-src]: http://hackage.haskell.org/package/haskell-src-1.0.1.3
[html]: http://hackage.haskell.org/package/html-1.0.1.2
[HUnit]: http://hackage.haskell.org/package/HUnit-1.2.2.1
[mtl]: http://hackage.haskell.org/package/mtl-1.1.0.2
[network]: http://hackage.haskell.org/package/network-2.2.1.7
[OpenGL]: http://hackage.haskell.org/package/OpenGL-2.2.3.0
[parallel]: http://hackage.haskell.org/package/parallel-2.2.0.1
[QuickCheck]: http://hackage.haskell.org/package/QuickCheck-2.1.1.1
[regex-base]: http://hackage.haskell.org/package/regex-base-0.93.2
[regex-compat]: http://hackage.haskell.org/package/regex-compat-0.93.1
[regex-posix]: http://hackage.haskell.org/package/regex-posix-0.94.2
[stm]: http://hackage.haskell.org/package/stm-2.1.2.1
[time]: http://hackage.haskell.org/package/time-1.1.4
[xhtml]: http://hackage.haskell.org/package/xhtml-3000.2.0.1
[zlib]: http://hackage.haskell.org/package/zlib-0.5.2.0
[HTTP]: http://hackage.haskell.org/package/HTTP-4000.0.9
[deepseq]: http://hackage.haskell.org/package/deepseq-1.1.0.0
[ghc]: http://haskell.org/ghc
[alex]: http://haskell.org/alex
[happy]: http://haskell.org/happy
[haddock]: http://haskell.org/haddock
