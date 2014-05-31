#!/bin/sh

# ghc-clang-wrapper
#       - mzero (Mark Lentczner)
#       - v1, 2013-11-01

# Xcode 5 no longer includes *gcc*. GHC 7, whether installed directly or via
# Haskell Platform, can be made to work with this wrapper script:
#
#   1. Copy this script to /usr/bin, and make sure it is exectuable.
#   2. Run it sudo. (Running without sudo will tell you what it would do.)

inPreprocessorMode () {
    hasE=0
    hasU=0
    hasT=0
    for arg in "$@"
    do
        if [ 'x-E' = "x$arg" ];             then hasE=1; fi
        if [ 'x-undef' = "x$arg" ];         then hasU=1; fi
        if [ 'x-traditional' = "x$arg" ];   then hasT=1; fi
    done
    [ "$hasE$hasU$hasT" = '111' ]
}

gccIsClang () {
    gcc --version 2>/dev/null | grep -q clang
}

extraClangArgs="-Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs"

adjustPreprocessorLanguage () {
    newArgs=''
    while [ $# -gt 0 ]
    do
        newArgs="$newArgs $1"
        if [ "$1" = '-x' ]
        then
            shift
            if [ $# -gt 0 ]
            then
                if [ "$1" = 'c' ]
                then
                    newArgs="$newArgs assembler-with-cpp"
                else
                    newArgs="$newArgs $1"
                fi
            fi
        fi
        shift
    done
    echo $newArgs
}

installThisScript () {
    if [ \! -x /usr/bin/ghc-clang-wrapper ]
    then
        echo "Copy this execute script to /usr/bin/ghc-clang-wrapper, then re-run it."
        echo "You can copy it with these commands:"
        echo
        echo sudo cp $0 /usr/bin/ghc-clang-wrapper
        echo sudo chmod 755 /usr/bin/ghc-clang-wrapper
        echo sudo ghc-clang-wrapper
        exit 1
    fi

    settings=`ls -1 /Library/Frameworks/GHC.framework/Versions/7*/usr/lib/ghc-7*/settings 2>/dev/null`

    if [ "$settings" = '' ]
    then
        echo "You don't seem to have Haskell Platform installed."
        echo "Visit http://www.haskell.org/platform/ to get and install it."
        echo "Then run this script again."
        echo
        echo "If you have GHC installed in a non-standard place, find the file"
        echo "named 'settings', that is alongside the compiler, and edit the"
        echo "c compiler line to read:"
        echo '     ("C compiler command", "/usr/bin/ghc-clang-wrapper"),'
        exit 1
    fi

    anyNeedPatching=0
    for sfile in $settings
    do
        if grep -q ghc-clang-wrapper $sfile
        then
            echo ALREADY PATCHED: $sfile
        else
            if [ `id -u` -eq 0 ]
            then
                sed -e '/C compiler command/s:"[^ ]*gcc":"/usr/bin/ghc-clang-wrapper":' -i '.bak' $sfile
                echo PATCHED: $sfile
            else
                echo WOULD PATCH: $sfile
                anyNeedPatching=1
            fi
        fi
    done

    if [ $anyNeedPatching -eq 1 ]
    then
        echo "Please run this script sudo to actually patch GHC:"
        echo sudo $0
    fi
}

if [ $# -eq 0 ]
then
    installThisScript
else
    if gccIsClang
    then
        if inPreprocessorMode "$@"
        then
            exec gcc $extraClangArgs `adjustPreprocessorLanguage "$@"`
        else
            exec gcc $extraClangArgs "$@"
        fi
    else
        exec gcc "$@"
    fi
fi
