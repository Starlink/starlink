## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = mag_link mag_link_adam create_tape_dev

bin_MONOLITHS = tapecreate

tapecreate_SOURCES = \
	tapecreate.f

# There is only one task and that is the same as the monolith name
# no variable tapecreate_TASKS -- tapecreate is an ATASK

# Needs the mag libraries
tapecreate_LDADD = $(LDADD) `$(srcdir)/mag_link_adam`

lib_LTLIBRARIES = libmag.la libmag_adam.la

libmag_la_SOURCES = $(F_ROUTINES) $(C_ROUTINES) \
	$(include_MESSAGES) $(PRIVATE_INCLUDES) $(BLOCKDATA)

libmag_adam_la_SOURCES = $(F_ROUTINES_ADAM) \
	$(include_MESSAGES) $(PRIVATE_INCLUDES) $(BLOCKDATA_ADAM)

include_MESSAGES = MAG_ERR 
noinst_MESSAGES = MIO_ERR mio_err.h

PRIVATE_INCLUDES = MAGGO_SCL MAGIO_CMN MAGPA_SCL MAGSCL_PAR MAG_SYS \
                   MIOBUF_CMN MIOFIL_CMN MIO_SYS ioc_incl.h \
		   $(noinst_MESSAGES)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@


F_ROUTINES = mag1_code.f mag1_dev.f mag1_end.f mag1_ernam.f mag1_err.f \
mag1_errtp.f mag1_fxpos.f mag1_gettd.f mag1_init.f mag_close.f	       \
mag_jeov.f mag_jump.f mag_move.f mag_open.f mag_pos.f mag_read.f       \
mag_rew.f mag_set.f mag_skip.f mag_write.f mag_wtm.f mio1_chan.f       \
mio1_code.f mio1_devn.f mio1_gettd.f mio1_trln.f mio_activ.f	       \
mio_aloc.f mio_bread.f mio_bwrit.f mio_close.f mio_deact.f mio_deal.f  \
mio_dism.f mio_flush.f mio_mount.f mio_open.f mio_read.f mio_rew.f     \
mio_setrb.f mio_skipb.f mio_skipf.f mio_skipv.f mio_start.f mio_stop.f \
mio_write.f mio_wtm.f

F_ROUTINES_ADAM = mag1_chktd.f mag1_chktp.f mag1_cktds.f mag1_cntds.f \
mag1_cptds.f mag1_crtds.f mag1_fndtp.f mag1_gettp.f mag1_gtmpl.f      \
mag1_rdtds.f mag1_wrtds.f mag_activ.f mag_aloc.f mag_annul.f	      \
mag_assoc.f mag_cancl.f mag_deact.f mag_deal.f mag_dism.f mag_mount.f \
mag_start.f mag_stop.f

BLOCKDATA = mag1_blk.f mio1_blk.f

BLOCKDATA_ADAM = mag_blk.f

C_ROUTINES = ioc.c


BUILT_SOURCES = $(include_MESSAGES) $(noinst_MESSAGES)


# magtest is ADAM monolith

TESTS = mag_test ioctest magtest

check_PROGRAMS = mag_test ioctest
check_MONOLITHS = magtest

ioctest_SOURCES = ioctest.f
ioctest_LDADD = libmag.la `err_link` `psx_link`

mag_test_SOURCES = mag_test.f
mag_test_LDADD = `$(srcdir)/mag_link_adam`

# no variable magtest_TASKS -- magtest is an ATASK
magtest_SOURCES = magtest.f
magtest_LDADD = `$(srcdir)/mag_link_adam` `prm_link`

dist_starnews_DATA = mag.news
dist_pkgdata_DATA = LICENCE

EXTRA_DIST = mag_err.msg mio_err.msg

CLEANFILES = magtest fac_340_err magtest.ifc tapecreate \
             tapecreate.ifc MIO_ERR mio_err.h

