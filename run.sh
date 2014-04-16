#!/usr/bin/env bash
EXE=./dist/build/labyrinth/labyrinth.exe
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
rm -f $EXE &&\
cabal build  &&\
echo -n "Running labyrinth... " &&\
$EXE +RTS -N
if [[ $? -ne 0 ]] ; then
    echo "failed"
fi
