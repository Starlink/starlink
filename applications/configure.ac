dnl    Process this file with autoconf to produce a configure script

dnl    This is not a package, so the first two arguments are fake.
AC_INIT(star-applications, 1.0, starlink@jiscmail.ac.uk)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Following sanity-check isn't terribly helpful...
AC_CONFIG_SRCDIR(bootstrap)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_MAKE_SET

dnl    The following list must be kept up-to-date by hand.
dnl    The list is available as the substitution variable @subdirs@.
dnl    (must be kept on one line)
AC_CONFIG_SUBDIRS([astrom atools ccdpack coco convert cupid cursa datacube daophot dipso docfind echomop esp extractor extreme fluxes figaro findcoords gaia generic hdstools hdstrace htx icl ifd kappa messgen news observe pamela period photom pongo pisa polmap polpack psmerge rv smurf specx spt sst star2html startcl starx surf tsp xdisplay latexsupport])

AC_CONFIG_FILES(Makefile)

AC_OUTPUT
