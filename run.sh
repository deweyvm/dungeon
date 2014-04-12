#!/usr/bin/env bash
time cabal build > /dev/null &&\
time ./dist/build/labyrinth/labyrinth.exe
if [[ $? -ne 0 ]] ; then
    echo "failed"
fi
