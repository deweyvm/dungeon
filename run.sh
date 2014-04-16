#!/usr/bin/env bash

mytime() {
    timestr=`(time "$@")  2>&1`
    echo "Time: $timestr" | grep 'real' | sed 's/real\t0m//g'
}

pushd src &> /dev/null && \
echo "Generating documentation..." &&\
out=`haddock -h -o ../docs Main.hs`
if [[ $? -ne 0 ]] ; then
    echo WARNING: haddock failure
    echo $out
fi
popd &> /dev/null
echo -n "Building... "
(mytime cabal build -v 0) &&\
echo -n "Running labyrinth... " &&\
(mytime ./dist/build/labyrinth/labyrinth.exe +RTS -N)
if [[ $? -ne 0 ]] ; then
    echo "failed"
fi
