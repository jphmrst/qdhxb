#!/usr/bin/env bash

echo ----- Active calls with no options passed -----
grep ^qdhxb\'\  test/*/*.hs | sed -e 's|test/||g'

echo ----- Active calls with options passed -----
grep ^qdhxb\  test/*/*.hs | sed -e 's|test/||g'

echo ----- Test Haskell modules with no active QDHXB calls -----
grep -l ^qdhxb\'\  test/*/*.hs  > .tmp-files
grep -l ^qdhxb\  test/*/*.hs   >> .tmp-files
ls test/*/*.hs > .tmp-all
grep -v -f .tmp-files .tmp-all > .tmp-nonfinished
grep -l TODO `cat .tmp-nonfinished` > .tmp-nonfinished-withTODO
# First print the TODO lines from files with a TODO line
grep -Hn TODO `cat .tmp-nonfinished-withTODO` | sort -k 3 | sed -e 's|test/||g'
# Then print the names of file without a TODO line
grep -v -f .tmp-nonfinished-withTODO .tmp-nonfinished | sed -e 's|test/||g'
rm -f .tmp-files .tmp-all .tmp-nonfinished .tmp-nonfinished-withTODO

echo ----- Unused XSD files -----
grep qdhxb test/*/*.hs | sed -e 's/^.*\[ *"//' | sed -e 's/" *]//' | sort -u > .tmp-qdhxb-calls
ls -1 test/*/*.xsd > .tmp-all-xsd
grep -v -f .tmp-qdhxb-calls .tmp-all-xsd > .tmp-nonfinished
(for m in `cat .tmp-nonfinished`; do wc -l $m; done) | sort -n
rm .tmp-all-xsd .tmp-nonfinished .tmp-qdhxb-calls

echo ----- Unused XML files -----
grep 'load.\+ "test/.\+/.\+\.xml' test/*/*.hs | sed -e 's/^.*load.\+ "//' | sed -e 's/".*$//' | sort -u > .tmp-xml-calls
ls -1 test/*/*.xml > .tmp-all-xml
grep -v -f .tmp-xml-calls .tmp-all-xml > .tmp-unused
(for m in `cat .tmp-unused`; do wc -l $m; done) | sort -n
rm .tmp-all-xml .tmp-xml-calls .tmp-unused

echo ----- Finished -----

