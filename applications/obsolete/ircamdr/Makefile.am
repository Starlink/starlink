## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

bin_SCRIPTS = ircamdr

bin_MONOLITHS = io2ro obsrap polrap plt2d rapi2d 

io2ro_SOURCES = io2ro.f
io2ro_TASKS = 
io2ro_LDADD = io2ro_f/libio2ro.la `chr_link_adam` `hds_link_adam` \
	`fio_link_adam`

obsrap_SOURCES = obsrap.f
obsrap_TASKS =
obsrap_LDADD = \
	obsrap_f/libobsrap.la \
	rapi2d_f/librapi2d.la \
	plt2d_f/libplt2d.la \
        aspker/libaspker.la \
	atlib/libatl.la \
	aif/libaif.la \
	rapsub/librapsub.la \
	rapgen/librapgen.la \
        kersub/libkersub.la \
	kergen/libkergen.la \
	`ndf_link_adam` `fio_link_adam` `mag_link_adam` `pda_link_adam`

polrap_SOURCES = polrap.f
polrap_TASKS = 
polrap_LDADD = \
polrap_f/libpolrap.la \
obsrap_f/libobsrap.la \
rapi2d_f/librapi2d.la \
plt2d_f/libplt2d.la \
aspker/libaspker.la \
atlib/libatl.la\
aif/libaif.la \
trace/libtrace.la \
rapsub/librapsub.la \
rapgen/librapgen.la \
kersub/libkersub.la \
kergen/libkergen.la \
`ndf_link_adam` `fio_link_adam` `pda_link_adam`

AM_FCFLAGS = -Iplt2d_f

plt2d_SOURCES = plt2d.f
plt2d_TASKS =
plt2d_LDADD = \
	plt2d_f/libplt2d.la \
	goodies/libgoodies.la \
	atlib/libatl.la \
	`ndf_link_adam` `sgs_link_adam` \
	`fio_link_adam` `gks_link_adam`

rapi2d_SOURCES = rapi2d.f
rapi2d_TASKS =
rapi2d_LDADD = \
	rapi2d_f/librapi2d.la \
	aspker/libaspker.la \
	atlib/libatl.la \
	aif/libaif.la \
	trace/libtrace.la \
	rapsub/librapsub.la \
	rapgen/librapgen.la \
	kersub/libkersub.la \
	kergen/libkergen.la \
	`ndf_link_adam` `fio_link_adam` `pda_link_adam`

