#!/usr/bin/env bash
EXE=./dist/build/labyrinth/labyrinth.exe

pushd src &> /dev/null && \
echo "Generating documentation..." &> /dev/null &&\
out=`haddock -h -o ../docs Main.hs`
if [[ $? -ne 0 ]] ; then
    echo WARNING: haddock failure
    echo $out
fi
popd &> /dev/null
echo "Building... "  &> /dev/null
rm -f $EXE &&\
cabal build &> /dev/null &&\
echo "Running labyrinth... "  &> /dev/null &&\
time $EXE +RTS -N -K100M
if [[ $? -ne 0 ]] ; then
    echo "failed"
fi
