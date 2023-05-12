#!/usr/bin/env bash

echo --
grep ^qdhxb\'\  test/*/*.hs 
echo --
grep ^qdhxb\  test/*/*.hs 
echo --
ls test/*/*.hs 
echo --
wc -l test/*/*.xsd | sort -n
echo --
