noinst_LTLIBRARIES = libgsleigen.la 

check_PROGRAMS = test

pkginclude_HEADERS = gsl_eigen.h
libgsleigen_la_SOURCES =  jacobi.c symm.c symmv.c nonsymm.c nonsymmv.c herm.c hermv.c sort.c francis.c schur.c
INCLUDES= -I$(top_builddir)

noinst_HEADERS =  qrstep.c schur.h

TESTS = $(check_PROGRAMS)

test_LDADD = libgsleigen.la  ../test/libgsltest.la ../linalg/libgsllinalg.la ../permutation/libgslpermutation.la ../blas/libgslblas.la ../cblas/libgslcblas.la ../matrix/libgslmatrix.la ../vector/libgslvector.la ../block/libgslblock.la  ../complex/libgslcomplex.la ../ieee-utils/libgslieeeutils.la ../sys/libgslsys.la ../err/libgslerr.la ../utils/libutils.la

test_SOURCES = test.c


