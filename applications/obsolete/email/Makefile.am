bin_SCRIPTS = email
pkgdata_DATA = email.news

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Fixup scripts with bin location
# The scripts needs to have installation locations edited into them
edit = sed \
	-e 's,@staretcdir\@,$(staretcdir),g' \
	-e 's,@admindir\@,$(datadir)/info,g' \
	-e 's,@PACKAGE_VERSION\@,$(VERSION),g'

email: email.in
	-rm -f email.tmp email
	$(edit) \
	    $(srcdir)/email.in > email.tmp
	mv email.tmp email
	chmod +x email
