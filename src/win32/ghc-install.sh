#! /bin/sh

SETUP_HS=
if [ -e Setup.lhs ]
then
    SETUP_HS="Setup.lhs"
fi
if [ -e Setup.hs ]
then
    SETUP_HS="Setup.hs"
fi

if [ -z $SETUP_HS ]
then
    echo "Couldn't find the Setup.[l]hs file!"
    exit 1;
fi

runghc $SETUP_HS configure --enable-library-profiling --enable-shared --enable-split-objs --global
runghc $SETUP_HS build
runghc $SETUP_HS haddock
runghc $SETUP_HS install
