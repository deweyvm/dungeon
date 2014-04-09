#!/usr/bin/env bash
cabal build > /dev/null &&\
./dist/build/labyrinth/labyrinth.exe
