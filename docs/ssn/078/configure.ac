dnl    Process this file with autoconf to produce a configure script

AC_INIT(ssn78, 0.1, ussc@star.rl.ac.uk)

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

AC_CONFIG_SRCDIR(ssn78.xml)

STAR_DEFAULTS

AC_PATH_PROGS(JADE, [jade openjade], jade)
AC_PATH_PROGS(SGMLNORM, [sgmlnorm osgmlnorm], sgmlnorm)

AC_ARG_WITH(sgmldir,
            AS_HELP_STRING([--with-sgmldir],
                           [Location of the SGML Kit tools]),
            STARLINK_SGML_DIR=$withval,
            STARLINK_SGML_DIR=$STARLINK/lib)
AC_SUBST(STARLINK_SGML_DIR)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
