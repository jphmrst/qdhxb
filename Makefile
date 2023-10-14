
%.ps: src/QDHXB/Internal/%.hs
	a2ps --line-numbers=5 -2r -o $@ $<

Gen-%.ps: src/QDHXB/Internal/Generate/%.hs
	a2ps --line-numbers=5 -2r -o $@ $<

Utils-%.ps: src/QDHXB/Internal/Utils/%.hs
	a2ps --line-numbers=5 -2r -o $@ $<

out.ps: out.txt Makefile
	a2ps -1r --chars-per-line=160 -o $@ $<
