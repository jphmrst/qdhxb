#!/usr/bin/env bash
grep -n $1 *.hs */*.hs */*/*.hs */*/*/*.hs */*/*/*/*.hs
