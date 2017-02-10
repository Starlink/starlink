
noinst_DATA = scripts.hlp scripts.tex

#  Scripts with compatible prologues to be included in sun258
PYSCRIPTS = \
configmeld.py \
fts2gaia.py \
jsajoin.py \
jsasplit.py \
jsatilemoc.py \
skyloop.py \
pol2map.py \
pol2sim.py \
pol2ip.py \
pol2stack.py \
pol2scan.py

# Help file
scripts.hlp: $(PYSCRIPTS)
	-rm -f scripts.hlp
	cat $(PYSCRIPTS) > all
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROHLP@ in=all out=scripts.hlp single=no
	-rm all

# Generate LaTeX include file for sun258
scripts.tex: $(PYSCRIPTS)
	-rm -f scripts.tex
	cat $(PYSCRIPTS) > all
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROLAT@ in=all out=scripts.tex single=no page=yes atask=yes document=no
	-rm all
