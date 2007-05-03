
## The monolith for all local atasks.
bin_MONOLITHS = gaiaMon

gaiaMon_TASKS = ardspectra ardstat autocrop cat2tab tab2cat asc2tab tab2asc

gaiaMon_SOURCES = \
	gaiaMon.c \
	$(gaiaMon_TASKS:=.f) \
	$(SUBSRC) \
	$(gaiaLib)

gaiaLib = libgaia@PACKAGE_VERSION@@TCL_SHLIB_EXT@

gaiaMon_LDADD = $(LDADD) \
	libgaia4.0.0.so \
	`fio_link_adam` `ard_link_adam` `ndf_link_adam` `cat_link_adam`

SUBSRC = gai1_nxtab.f gai1_s2ang.f gai1_t2cg.f gai1_t2cn.f gai1_wrtab.f \
	gai_a2tab.f gai_c2tab.f gai_t2asc.f gai_t2cat.f rtd1_ardst.f \
	rtd1_prsh2.f

$(gaiaLib):
	if test -f $(gaiaLib); then \
	   rm -rf $(gaiaLib) ;\
	fi
	ln -s ../gaia/$(gaiaLib) .
