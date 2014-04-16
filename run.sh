#!/usr/bin/env bash
time cabal build > /dev/null &&\
time ./dist/build/labyrinth/labyrinth.exe +RTS -N
if [[ $? -ne 0 ]] ; then
    echo "failed"
fi
