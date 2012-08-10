dnl    Process this file with autoconf to produce a configure script

AC_INIT(ssn78, 0.1, starlink@jiscmail.ac.uk)

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

AC_CONFIG_SRCDIR(ssn78.xml)

STAR_DEFAULTS(docs-only)

STAR_XML_DOCUMENTATION(ssn78)
LATEX2DVI='export TEXINPUTS=..:; $$LATEX "\\batchmode\\input $$1" && makeindex $$1 && $$LATEX "\\batchmode\\input $$1"'
AC_PATH_PROGS(METAPOST, [mp mpost])

dnl  The default here isn't terribly reliable, since it relies on
dnl  $STARLINK having the correct value, and star2html not having changed
dnl  its mind about where to install things.  This is not a model test.
AC_ARG_WITH(pstoimg,
            AS_HELP_STRING([--with-pstoimg],
                           [location of pstoimg (installed as part of star2html)]),
            PSTOIMG=$withval,
            PSTOIMG=$STARLINK/share/star2html-l2hsys/current/pstoimg)
if test -f "$PSTOIMG" -a -x "$PSTOIMG"; then
    : OK
else
    AC_MSG_WARN([pstoimg application $PSTOIMG does not exist is not executable])
    PSTOIMG=NOPSTOIMG
fi
AC_SUBST(PSTOIMG)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
