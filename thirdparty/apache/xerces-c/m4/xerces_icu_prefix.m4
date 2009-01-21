dnl @synopsis XERCES_ICU_PREFIX
dnl
dnl Determines the prefix for icu
dnl
dnl @category C
dnl @author James Berry
dnl @version 2005-05-23
dnl @license AllPermissive
dnl
dnl $Id: xerces_icu_prefix.m4 467290 2006-10-24 09:23:14Z amassari $

AC_DEFUN([XERCES_ICU_PREFIX],
	[
	AC_ARG_WITH([icu],
		[AS_HELP_STRING([--with-icu[[[[=DIR]]]]],[Specify location of icu])],
		[with_icu=m4_if($with_icu, [yes], [], $with_icu)],
		[with_icu=])

	# Determine if icu is available
	AC_CACHE_CHECK([for icu], [xerces_cv_icu_prefix],
	[	
		xerces_cv_icu_prefix=
		if test x"$with_icu" != x"no"; then
			search_list="$with_icu /usr/local /usr"
			for i in $search_list; do			
				if test -r $i/include/unicode/ucnv.h; then
					xerces_cv_icu_prefix=$i
					break
				fi
			done
		fi
	])
	
	AC_SUBST([ICU_PREFIX], [$xerces_cv_icu_prefix])
	]
)

