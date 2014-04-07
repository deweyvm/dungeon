#!/usr/bin/env bash
cabal build > /dev/null &&\
./dist/build/dungeon/dungeon.exe
