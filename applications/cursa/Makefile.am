## Process this file with automake to produce Makefile.in

SUBDIRS = libcap

bin_SCRIPTS = xcatview cursa_test.csh cursa.csh catcdsin \
 catremote UrlReader.class UrlReader.java geturl cursa.cfg

dist_noinst_SCRIPTS = cursa_link_adam

LOCAL_LINK = libcap/libcap.la `$(srcdir)/cursa_link_adam`

bin_MONOLITHS = catview cursa

catview_SOURCES = catview.f
catview_LDADD = $(LOCAL_LINK)
# no variable catview_TASKS -- catview is an ATASK

cursa_SOURCES = cursa.f
cursa_LDADD = $(LOCAL_LINK)
cursa_TASKS = catchart catchartrn catcoord catcopy catgrid  \
 catgscin catheader catpair catphotomfit catphotomlst catphotomtrn \
 catselect catsort

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

#  List of ancillary data files (all types). These need a home at some point.
EXAMPLE_FILES = examples/0README.LIS examples/angles.TXT examples/bd.grt \
examples/usno.grt examples/catpair_cart.script examples/catpair_sphplr.script \
examples/complex.dat examples/complex.grt examples/complex.TXT examples/gsc.grt \
examples/iras_psc.grt examples/kappa.TXT examples/photoprog.TXT examples/simple.TXT \
examples/photostandards.TXT examples/pisaresults.TXT examples/pisasizes.TXT \
examples/polygon.TXT examples/ppm.grt examples/propmotn.TXT examples/rc3.grt \
examples/sao.grt examples/scale.TXT examples/scosmosbw.grt examples/scosmoscol.grt \
examples/simbad.grt examples/simple.grt examples/simple_radians.TXT catremote-setup.csh \
xcatviewLib/querycat.tcl xcatviewLib/listavaildb.tcl xcatviewLib/resolvename.tcl

##  Extra files for CURSA bin directory.
cursasupportdir = $(bindir)
cursasupport_DATA = data/cursa_output.lis data/catselect.script \
 data/catpair.script data/catview.script data/5828.gsc data/targets.TXT

##  BUILT files not named anywhere else.
BUILT_SOURCES = xcatviewLib/xcatview.tcl

##  Scripts for xcatview

xcatviewsupportdir = $(bindir)/xcatviewLib
xcatviewsupport_DATA = xcatviewLib/xcatview.tcl \
 xcatviewLib/Action.tcl xcatviewLib/GetFile.tcl \
 xcatviewLib/GetScatterOver.tcl xcatviewLib/Error.tcl \
 xcatviewLib/GetFormat.tcl xcatviewLib/GetSelection.tcl \
 xcatviewLib/GetCat.tcl xcatviewLib/GetHist.tcl \
 xcatviewLib/GetStatsCols.tcl xcatviewLib/GetCatalogue.tcl \
 xcatviewLib/GetHistOver.tcl xcatviewLib/HelpText.tcl \
 xcatviewLib/GetColumns.tcl xcatviewLib/GetParam.tcl \
 xcatviewLib/OpenCat.tcl xcatviewLib/GetConfig.tcl \
 xcatviewLib/GetRange.tcl xcatviewLib/OpenRemote.tcl \
 xcatviewLib/GetDirCat.tcl xcatviewLib/GetRemoteQuery.tcl \
 xcatviewLib/RemoteList.tcl xcatviewLib/GetDispConfig.tcl \
 xcatviewLib/GetRow.tcl xcatviewLib/RemoteName.tcl \
 xcatviewLib/GetExprn.tcl xcatviewLib/GetScatter.tcl \
 xcatviewLib/RemoteQuery.tcl xcatviewLib/RemoteUrl.tcl \
 xcatviewLib/adamtask.tcl xcatviewLib/tclIndex


##  Edit scripts during make to have the correct paths.
NETWORKED = "yes"
edit = sed \
        -e 's,@bindir\@,$(bindir),g' \
        -e 's,@STARPERL\@,$(STARPERL),g' \
        -e 's,@AWISH\@,$(AWISH),g' \
        -e 's,@ATCLSH\@,$(ATCLSH),g' \
        -e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g' \
        -e 's,@NETWORKED\@,$(NETWORKED),g'

xcatview: xcatview.in
	rm -f xcatview.tmp xcatview
	$(edit) \
	   -e 's,@edited_input\@,xcatview: produced from xcatview.in by Makefile.am,' \
	   $(srcdir)/xcatview.in >xcatview.tmp
	mv xcatview.tmp xcatview

geturl: geturl.in
	rm -f geturl.tmp geturl
	$(edit) \
	   -e 's,@edited_input\@,geturl: produced from geturl.in by Makefile.am,' \
	   $(srcdir)/geturl.in >geturl.tmp
	mv geturl.tmp geturl

xcatviewLib/xcatview.tcl: xcatviewLib/xcatview.tcl.in
	rm -f xcatviewLib/xcatview.tcl.tmp xcatviewLib/xcatview.tcl
	$(edit) \
	   -e 's,@edited_input\@,xcatview.tcl: produced from catviewLib/xcatview.tcl.in by Makefile.am,' \
	   $(srcdir)/xcatviewLib/xcatview.tcl.in >xcatviewLib/xcatview.tcl.tmp
	mv xcatviewLib/xcatview.tcl.tmp xcatviewLib/xcatview.tcl

catcdsin: catcdsin.in
	rm -f catcdsin.tmp catcdsin
	$(edit) \
	   -e 's,@edited_input\@,catcdsin: produced from catcdsin.in by Makefile.am,' \
	   $(srcdir)/catcdsin.in >catcdsin.tmp
	mv catcdsin.tmp catcdsin

catremote: catremote.in
	rm -f catremote.tmp catremote
	$(edit) \
	   -e 's,@edited_input\@,catremote: produced from catremote.in by Makefile.am,' \
	   $(srcdir)/catremote.in >catremote.tmp
	mv catremote.tmp catremote

cursa.csh: cursa.csh.in
	rm -f cursa.csh.tmp cursa.csh
	$(edit) \
	   -e 's,@edited_input\@,cursa.csh: produced from cursa.csh.in by Makefile.am,' \
	   $(srcdir)/cursa.csh.in >cursa.csh.tmp
	mv cursa.csh.tmp cursa.csh

cursa_test.csh: cursa_test.csh.in
	rm -f cursa_test.csh.tmp cursa_test.csh
	$(edit) \
	   -e 's,@edited_input\@,cursa_test.csh: produced from cursa_test.csh.in by Makefile.am,' \
	   $(srcdir)/cursa_test.csh.in >cursa_test.csh.tmp
	mv cursa_test.csh.tmp cursa_test.csh

dist_starnews_DATA = cursa.news

dist_pkgdata_DATA = CURSA_CONDITIONS $(EXAMPLE_FILES)

EXTRA_DIST = xcatview.in cursa_test.csh.in cursa.csh.in catcdsin.in \
             catremote.in UrlReader.class UrlReader.java \
             $(xcatviewsupport_DATA) $(cursasupport_DATA)

CLEANFILES = version.dat catremote catcdsin cursa.csh cursa_test.csh xcatview \
             catchart.ifc catchartrn.ifc catcoord.ifc catcopy.ifc catgrid.ifc \
             catgscin.ifc catheader.ifc catpair.ifc catphotomfit.ifc catphotomlst.ifc \
             catphotomtrn.ifc catselect.ifc catsort.ifc cursa catview cursa.ifc cursa.ifl \
             catview.ifc

