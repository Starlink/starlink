## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = adam_link_adam

lib_LTLIBRARIES = libadam_adam.la

libadam_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(F_ROUTINES)
libadam_adam_la_FCFLAGS = $(AM_FCFLAGS) -I../messys

include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = ADAM_ERR ADAM_DEFNS adam_defns.h

F_ROUTINES = \
	adm_acknow.f \
        adm_getreply.f \
        adm_getreplyt.f \
        adm_path.f \
        adm_prcnam.f \
        adm_receive.f \
        adm_reply.f \
        adm_send.f \
        adm_sendonly.f \
        adm_sendt.f \
        adm_trigger.f

## BUILT_SOURCES = DDMSG @BUILT_SOURCES@
