
%.ps: src/QDHXB/Internal/%.hs
	a2ps -2r -o $@ $<

%.ps: src/QDHXB/Internal/Utils/%.hs
	a2ps -2r -o $@ $<