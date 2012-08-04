dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(extreme, 0.1-1, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([do-xxx])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)

dnl    Declare dependencies.
STAR_DECLARE_DEPENDENCIES(sourceset, sst)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
STAR_CHECK_PROGS(perl)
STAR_CHECK_PROGS(prolat, sst)

dnl    Ideally only code produced by flex should be distributed.
dnl    Not having a lex is serious, so stop (don't use AM_PROG_LEX test
dnl    that uses the missing script when lex isn't found).
AC_PROG_LEX
if test "$LEX" = ":"; then
  AC_MSG_ERROR([Failed to locate lex or flex])
fi

if test "$LEX" != flex; then
   AC_MSG_WARN([Do NOT distribute source code created by the '$LEX' command.])
   AC_MSG_WARN([Only code produced by the flex command is recommended])
   AC_MSG_WARN([for distribution.])
fi

dnl    Work out what size a pointer is.  Note, this isn't written in a 
dnl    foolproof manner, but it will probably work.
dnl    I think the use of ac_cv_sizeof_* variables is totally unsafe
dnl    here (got from looking through generated configure scripts) - 
dnl    but how else to do it???
AC_CHECK_SIZEOF(int *)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_MSG_CHECKING([type for BIG_INT])
if test "$ac_cv_sizeof_int_p" = "8" -a "$ac_cv_sizeof_long" = "8"
then
   big_int=long
else
   if test "$ac_cv_sizeof_int_p" = "4" -a "$ac_cv_sizeof_int" = "4"
   then
      big_int=int
   else
      AC_MSG_ERROR(cannot determine C type for BIG_INT)
   fi
fi
AC_MSG_RESULT($big_int)
AC_SUBST(big_int)

dnl   Work out the name of a big-enough FORTRAN 77 integer type.
dnl   Note this doesn't need to match the size of a pointer, it's just
dnl   an integer type which is going to be big enough to store values
dnl   which might be encountered on this platform (such as pixel counts
dnl   in an array).  It does need to be the same for all routines
dnl   linked together though, so all packages must use the same type.
dnl   We set it to "INTEGER * 8" for systems with a 64-bit pointer type
dnl   and "INTEGER    " for systems with a 32-bit pointer type.
dnl   If other possibilities emerge, write a new condition here.
AC_MSG_CHECKING([type for large FORTRAN integer])
if test "$ac_cv_sizeof_int_p" = "8"
then
   integer8="INTEGER * 8"
else
   if test "$ac_cv_sizeof_int_p" = "4"
   then
       integer8="INTEGER    "
   else
       AC_MSG_ERROR(cannot determine large FORTRAN integer type)
   fi
fi
AC_MSG_RESULT($integer8)
AC_SUBST(integer8)


STAR_LATEX_DOCUMENTATION(ssn73)

AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_FILES([extreme.h EXT_DEC EXT_PAR extreme.csh])
AC_CONFIG_FILES(cmp-xxx, [chmod +x cmp-xxx]) 
AC_CONFIG_FILES(extmakefile, [chmod +x extmakefile])
AC_CONFIG_FILES(extmk, [chmod +x extmk])
AC_CONFIG_FILES(mkdoc, [chmod +x mkdoc])

dnl    This is the bit that does the actual work
AC_OUTPUT
