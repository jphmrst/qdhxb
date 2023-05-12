#!/usr/bin/env bash

echo --
grep ^qdhxb\'\  test/*/*.hs 
echo --
grep ^qdhxb\  test/*/*.hs 
echo --
grep -l ^qdhxb\'\  test/*/*.hs  > .tmp-files
grep -l ^qdhxb\  test/*/*.hs   >> .tmp-files
ls test/*/*.hs > .tmp-all
grep -v -f .tmp-files .tmp-all
rm -f .tmp-files .tmp-all
echo --
wc -l test/*/*.xsd | sort -n
echo --
