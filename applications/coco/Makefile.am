## Process this file with automake to produce Makefile.in

bin_SCRIPTS = coco

bin_PROGRAMS = cocomain

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

cocomain_SOURCES = dp.f dr2tn.f opw.f prompt.f repra.f repsys.f	\
cocoml.f dqp.f ktest.f par1.f r2dr.f repres.f tran.f cocomain.f opr.F

cocomain_LDADD = `sla_link`


# The coco script needs to have installation locations edited into it
# Can not be done during configure since @bindir@ gets converted
# to ${exec_prefix}/bin

edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@SH\@,$(SH),g' \
	-e 's,@starbindir\@,$(bindir),g'

coco: coco.in
	rm -f coco.tmp coco
	$(edit) \
	 -e 's,@edited_input\@,coco: produced from coco.in by Makefile.am,' \
	 $(srcdir)/coco.in > coco.tmp
	mv coco.tmp coco
	chmod +x coco

EXTRA_DIST = coco.in

dist_starnews_DATA = coco.news

CLEANFILES = coco
