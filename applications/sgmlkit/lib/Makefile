#SGMLDIR=/home/norman/s/src/sgml
LIBDIR=${STARLINK_SGML_DIR}/lib
DECL=${STARLINK_SGML_DIR}/dtd/starlink.decl
STYLESHEET=${STARLINK_SGML_DIR}/dsssl/sl
SGMLPARSE=nsgmls

JADE=jade
JADEFLAGS=

# The HTML stylesheet does not output the generated HTML to standard
# output, but instead generates a filename.  It needs to do this so
# that it can link back and forth between different generated files
# effectively.  At the bottom of each HTML page, furthermore, the
# stylesheet makes a link to a printable version of the page, again by
# generating the filename root.  To help you tie these names together,
# the stylesheet chucks out a single line of the form:
#
#  root:ext1 ext2 ...
#
# where `root' is the root of the filename which it has generated, and
# ext? are the extensions it has created links to in the page footer
# (in fact, as of March 1999, the stylesheet _doesn't_ generate these links,
# but it will as soon as I start generating something to link to!)
%.html.stamp: %.sgml
	$(JADE) -t sgml -iHTML.entities $(JADEFLAGS) \
		-d $(STYLESHEET)/html/sl.dsl#html $< > $@

%.single.html: %.sgml
	$(JADE) -t sgml -isingle.file -iHTML.entities -V stream-output $(JADEFLAGS) -d $(STYLESHEET)/html/sl.dsl#html $< > $@

%.parse: %.sgml
	$(SGMLPARSE) -wall -ru $(DECL) $< >$@

%.summary: %.sgml
	 sgmlnorm -A documentsummary -d $< > $@

%.fot: %.sgml
	$(JADE) -t fot -d $(STYLESHEET)/html/sl.dsl $< >$@

# The target %.maths.stamp is satisfied when the maths has been extracted from 
# a document, and tth has been used to convert it to HTML.  Note that,
# although it's feasible, maths is _not_ routinely re-extracted by each
# run of the main stylesheet - the model is that the maths will be converted
# to HTML once, and then reused on subsequent production of the main document.
# The justification for this is that, in the typical case, this system
# won't be used for drafting, but for generating a final document
# in one run.
%.maths.stamp: %.sgml
	$(JADE) -t sgml $(JADEFLAGS) \
		-d $(STYLESHEET)/html/slmaths.dsl#maths.main $< > $@
	ROOT=`awk -F: '{print $$1}' $@`; \
	$(MAKE) $$ROOT.tth-html

%.tth-html: %.tth-eqlist
	tth -r < $< > $@

%.data: %.sgml
	$(SGMLPARSE) $(DECL) $< | \
	sed -n '/^-/{;s/^-//;s/\\n/ /g;p;};' \
	> $@

%.count: %.data
	wc $<


all: index.html

tidy:
	rm -f *.parse *~

clean:	tidy
	rm -f *.html *.stamp HTML.manifest *.tth-eqlist *.tth-html

checkhtml:
	for i in *.html; do echo $$i; $(SGMLPARSE) -s $(DECL) $$i; done
