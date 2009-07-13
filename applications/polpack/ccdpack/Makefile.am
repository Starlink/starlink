## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libccdpack.la

libccdpack_la_SOURCES = $(F_ROUTINES) \
                        $(PRIVATE_INCLUDES) 

PRIVATE_INCLUDES = CCD1_MEMCM CCD1_PAR

F_ROUTINES = ccd1_asfio.f ccd1_mfree.f ccd1_rmcom.f ccg1_is4d.f \
ccd1_skysr.f ccd1_cmprr.f ccd1_orvar.f ccg1_cenr.f ccd1_qsrtr.f \
ccd1_qntlr.f ccd1_hvcon.f ccg1_cmltr.f ccd1_iscom.f ccd1_rdlin.f 



