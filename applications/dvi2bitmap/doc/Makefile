include $(STARLINK_SGML_DIR)/lib/Makefile.common

doc.tar-htx: dvi2bitmap.sgml
	$(STARLINK_SGML_DIR)/bin/sgml2docs --html --tarball $@ $<

doc.tar-latex: dvi2bitmap.sgml
	$(STARLINK_SGML_DIR)/bin/sgml2docs --latex --tarball $@ $<

doc.tar-htxsingle: dvi2bitmap.sgml
	$(STARLINK_SGML_DIR)/bin/sgml2docs --htmlsingle --tarball $@ $<

doc.tar: doc.tar-htx doc.tar-latex dvi2bitmap.single.html
	for f in `cat *.manifest` \
			`awk -F: '{print $$1}' dvi2bitmap.html.stamp`.ps.gz \
			*.manifest Makefile dvi2bitmap.sgml; do \
		echo doc/$$f >>tar.list; \
	done
	# Following not necessary now image gifs are included in manifest
	#ROOT=`awk -F: '{print $$1}' dvi2bitmap.html.stamp`; \
	#test -r $$ROOT.imgeq.sgml && \
	#	sed -n "s/^.*sysid='\(.*\)'.*$$/doc\/\1/p" $$ROOT.imgeq.sgml \
	#	| sort | uniq >> tar.list
	echo doc/dvi2bitmap.single.html >>tar.list
	cd ..; tar cf doc.tar `sort doc/tar.list | uniq`
	rm -f tar.list
	mv ../doc.tar .


# doc.tar: dvi2bitmap.html.stamp dvi2bitmap.latex.stamp ps.stamp dvi2bitmap.single.html
# 	rm -f tar.list
# 	for f in `cat *.manifest` \
# 			`awk -F: '{print $$1}' dvi2bitmap.html.stamp`.ps.gz \
# 			*.manifest Makefile dvi2bitmap.sgml; do \
# 		echo doc/$$f >>tar.list; \
# 	done
# 	# Following not necessary now image gifs are included in manifest
# 	#ROOT=`awk -F: '{print $$1}' dvi2bitmap.html.stamp`; \
# 	#test -r $$ROOT.imgeq.sgml && \
# 	#	sed -n "s/^.*sysid='\(.*\)'.*$$/doc\/\1/p" $$ROOT.imgeq.sgml \
# 	#	| sort | uniq >> tar.list
# 	echo doc/dvi2bitmap.single.html >>tar.list
# 	cd ..; tar cf doc.tar `sort doc/tar.list | uniq`
# 	rm -f tar.list
# 	mv ../doc.tar .

ps.stamp: dvi2bitmap.latex.stamp
	ROOT=`awk -F: '{print $$1}' $<`; \
	rm -f $$ROOT.ps.gz ;\
	$(MAKE) $$ROOT.ps.gz

dvi2bitmap.html.stamp: dvi2bitmap.sgml dvi2bitmap.imgeq.list.stamp

index.html: dvi2bitmap.html.stamp
	ln -sf `awk -F: '{print $$1}' $<`.html $@

localclean:
	rm -f doc.tar
