dnl    Process this file with autoconf to produce a configure script

AC_INIT(ssn70-doc-dtd2sgml)

AC_CONFIG_SRCDIR(dtd2sgml.pl)

AC_PATH_PROG(PERL, perl)
AC_PATH_PROGS(JADE, [openjade jade], jade)

AC_ARG_VAR(STARLINK_SGML_DIR)
test -n "$STARLINK_SGML_DIR" || STARLINK_SGML_DIR=`cd ../..; pwd`

AC_ARG_WITH(perlsgml,
            AS_HELP_STRING([--with-perlsgml],
                           [location of the perlSGML distribution (path to dir which contains lib/SGML/DTD.pm)]),
            PERLSGMLDIR=$withval/lib,
            PERLSGMLDIR=./lib)
AC_CHECK_FILE([$PERLSGMLDIR/SGML/DTD.pm],,[PERLSGMLDIR=/UNKNOWN])
AC_SUBST(PERLSGMLDIR)

AC_CONFIG_FILES(Makefile)
AC_OUTPUT
