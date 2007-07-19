noinst_LTLIBRARIES = libgslinterpolation.la 

check_PROGRAMS = test

pkginclude_HEADERS = gsl_interp.h gsl_spline.h

libgslinterpolation_la_SOURCES = accel.c akima.c bsearch.c cspline.c interp.c linear.c integ_eval.h spline.c poly.c

noinst_HEADERS = bsearch.h

INCLUDES= -I$(top_builddir)

TESTS = $(check_PROGRAMS)

test_LDADD = libgslinterpolation.la ../poly/libgslpoly.la ../linalg/libgsllinalg.la ../permutation/libgslpermutation.la ../blas/libgslblas.la ../matrix/libgslmatrix.la ../vector/libgslvector.la ../block/libgslblock.la ../complex/libgslcomplex.la ../cblas/libgslcblas.la ../ieee-utils/libgslieeeutils.la  ../err/libgslerr.la ../test/libgsltest.la ../sys/libgslsys.la ../utils/libutils.la

test_SOURCES = test.c

