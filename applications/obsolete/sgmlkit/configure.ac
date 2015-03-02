dnl    Process this file with autoconf to produce a configure script

AC_INIT(sgmlkit, 0.7-13, starlink@jiscmail.ac.uk)
AC_CONFIG_AUX_DIR([build-aux])

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

STAR_DEFAULTS

AC_PATH_PROG(PERL, perl)
AC_PATH_PROG(PAX, pax)
AC_PATH_PROGS(JADE, [openjade jade], NOJADE)
AC_PATH_PROGS(SGMLNORM, [osgmlnorm sgmlnorm], NOSGMLNORM)
AC_PATH_PROG(DVI2BITMAP, dvi2bitmap, dvi2bitmap)
AC_PATH_PROG(PYTHON, python, python)
AC_PROG_LN_S

# We need dvi2bitmap in order to build the sgmlkit documentation
dnl In obsolete form dvi2bitmap is not available in the Starlink tree
dnl STAR_DECLARE_DEPENDENCIES(sourceset, dvi2bitmap)

STAR_PREDIST_SOURCES(doc/dtd2sgml/CATALOG)

dnl FIXME: This is VERY dodgy, especially if I want to remove @PREDIST@
dnl and reimplement that functionality using an automake conditional.
dnl $PREDIST is null if we're in the predist state.
if test -z "$PREDIST"; then
# The documentation requires Earl Hood's perlSGML library.  See
# http://search.cpan.org/~ehood/perlSGML.1997Sep18/
# This is imported into this repository in directory
# applications/sgmlkit/thirdparty/perlSGML (ie, subdirectory of here).
# If there should ever be a reason to change this, then you can override it
# with the --with-perlsgml option
# Unpack this, and give its location with the --with-perlsgml option
AC_ARG_WITH(perlsgml,
            AS_HELP_STRING([--with-perlsgml],
                           [location of the perlSGML distribution (path to dir w
hich contains lib/SGML/DTD.pm)]),
            PERLSGMLDIR=$withval/lib,
            PERLSGMLDIR=thirdparty/perlSGML/lib)
AC_CHECK_FILE([$PERLSGMLDIR/SGML/DTD.pm],,[PERLSGMLDIR=UNKNOWN])
if test "$PERLSGMLDIR" = UNKNOWN; then
    AC_MSG_NOTICE([Can't find perlSGML, which is required for building SSN/70.  Get it from http://search.cpan.org/~ehood/perlSGML.1997Sep18/ and point to it using --with-perlsgml])
else
    # make PERLSGMLDIR absolute
    PERLSGMLDIR=`cd $PERLSGMLDIR; pwd`
fi

else # in post-distribution state
    PERLSGMLDIR=
fi
AC_SUBST(PERLSGMLDIR)

# Documentation is in SSN/70, but this is not built in the default way
STAR_XML_DOCUMENTATION(ssn70,
                       [doc/ssn70/ssn70.htx_tar doc/ssn70/ssn70.texml_tar])

STAR_SUPPRESS_BUILD_IF([test $JADE = NOJADE -o $SGMLNORM = NOSGMLNORM],
    [The sgmlkit component requires both Jade and sgmlnorm.  All I could
     find were $JADE for Jade, and $SGMLNORM for sgmlnorm.
     Your system may have a way to install them as a package,
     in which case you should re-run configure and make.])

AC_CONFIG_FILES([Makefile component.xml \
                 dsssl/sl/common/slparams.dsl dsssl/sl/lib/Makefile])
AC_CONFIG_FILES(lib/img-eqlist.pl, [chmod +x lib/img-eqlist.pl])
# bin scripts are postprocessed from *.x at build time -- see Makefile.am
AC_CONFIG_FILES([bin/sgml2docs.x:bin/sgml2docs.in]
                [bin/sgml2hlp.x:bin/sgml2hlp.in])

AC_OUTPUT
