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
grep TODO `cat .tmp-nonfinished` | sort -k 3
grep -l TODO `cat .tmp-nonfinished` > .tmp-nonfinished-withTODO
grep -v -f .tmp-nonfinished-withTODO .tmp-nonfinished
rm -f .tmp-files .tmp-all .tmp-nonfinished .tmp-nonfinished-withTODO

echo --
grep qdhxb test/*/*.hs | sed -e 's/^.*\["//' | sed -e 's/"\]$//' | sort -u > .tmp-qdhxb-calls
ls -1 test/*/*.xsd > .tmp-all-xsd
grep -v -f .tmp-qdhxb-calls .tmp-all-xsd > .tmp-nonfinished
# cat .tmp-qdhxb-calls
(for m in `cat .tmp-nonfinished`; do wc -l $m; done) | sort -n
rm .tmp-all-xsd .tmp-nonfinished .tmp-qdhxb-calls

echo --
