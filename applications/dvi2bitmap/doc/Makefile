include $(STARLINK_SGML_DIR)/lib/Makefile.common

doc.tar: dvi2bitmap.html.stamp dvi2bitmap.latex.stamp ps.stamp
	rm -f tar.list
	for f in `cat *.manifest` \
			`awk -F: '{print $$1}' dvi2bitmap.html.stamp`.ps.gz \
			*.manifest Makefile dvi2bitmap.sgml; do \
		echo doc/$$f >>tar.list; \
	done
	cd ..; tar cf doc.tar `cat doc/tar.list`
	rm -f tar.list
	mv ../doc.tar .

ps.stamp: dvi2bitmap.latex.stamp
	ROOT=`awk -F: '{print $$1}' $<`; \
	rm -f $$ROOT.ps.gz ;\
	$(MAKE) $$ROOT.ps.gz

dvi2bitmap.html.stamp: dvi2bitmap.sgml dvi2bitmap.imgeq.list.stamp

index.html: dvi2bitmap.html.stamp
	ln -sf `awk -F: '{print $$1}' $<`.html $@

