SGMLDIR=/home/norman/s/src/sgml
LIBDIR=${SGMLDIR}/lib
DECL=${SGMLDIR}/dtd/starlink.decl
STYLESHEET=${SGMLDIR}/dsssl/sl
SGMLPARSE=nsgmls

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
%.stamp: %.sgml
	jade -t sgml $(JADEFLAGS) -d $(STYLESHEET)/html/sl.dsl#html $< > $@

%.single.html: %.sgml
	jade -t sgml -isingle.file -V stream-output $(JADEFLAGS) -d $(STYLESHEET)/html/sl.dsl#html $< > $@

%.parse: %.sgml
	$(SGMLPARSE) -wall -ru $(DECL) $< >$@

%.summary: %.sgml
	 sgmlnorm -A documentsummary -d $< > $@

%.fot: %.sgml
	jade -t fot -d $(STYLESHEET)/html/sl.dsl $< >$@

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
	rm -f *.html *.stamp HTML.manifest

checkhtml:
	for i in *.html; do echo $$i; $(SGMLPARSE) -s $(DECL) $$i; done
