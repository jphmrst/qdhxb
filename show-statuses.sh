#!/usr/bin/env bash

echo --
grep ^qdhxb\'\  test/*/*.hs 
echo --
grep ^qdhxb\  test/*/*.hs 
echo --
grep -l ^qdhxb\'\  test/*/*.hs  > .tmp-files
grep -l ^qdhxb\  test/*/*.hs   >> .tmp-files
ls test/*/*.hs > .tmp-all
grep -v -f .tmp-files .tmp-all > .tmp-nonfinished
grep TODO `cat .tmp-nonfinished`
grep -l TODO `cat .tmp-nonfinished` > .tmp-nonfinished-withTODO
grep -v -f .tmp-nonfinished-withTODO .tmp-nonfinished
rm -f .tmp-files .tmp-all .tmp-nonfinished .tmp-nonfinished-withTODO
echo --
wc -l test/*/*.xsd | sort -n
echo --
