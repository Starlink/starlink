dnl    Process this file with autoconf to produce a configure script

AC_INIT(ssn78, 0.1, ussc@star.rl.ac.uk)

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

AC_CONFIG_SRCDIR(ssn78.xml)

STAR_DEFAULTS

AC_PATH_PROGS(JADE, [jade openjade], jade)
AC_PATH_PROGS(SGMLNORM, [sgmlnorm osgmlnorm], sgmlnorm)
AC_PATH_PROGS(METAPOST, [mp mpost])

AC_ARG_WITH(sgmldir,
            AS_HELP_STRING([--with-sgmldir],
                           [location of the SGML Kit tools]),
            STARLINK_SGML_DIR=$withval,
            STARLINK_SGML_DIR=$STARLINK/lib)
AC_SUBST(STARLINK_SGML_DIR)

dnl  The default here isn't terribly reliable, since it relies on
dnl  $STARLINK having the correct value, and star2html not having changed
dnl  its mind about where to install things.
AC_ARG_WITH(pstoimg,
            AS_HELP_STRING([--with-pstoimg],
                           [location of pstoimg (installed as part of star2html)]),
            PSTOIMG=$withval,
            PSTOIMG=$STARLINK/share/star2html-l2hsys/current)
AC_SUBST(PSTOIMG)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
