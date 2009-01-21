dnl @synopsis XERCES_CURL_PREFIX
dnl
dnl Determines the prefix for libcurl
dnl
dnl @category C
dnl @author James Berry
dnl @version 2005-05-23
dnl @license AllPermissive
dnl
dnl $Id: xerces_curl_prefix.m4 467290 2006-10-24 09:23:14Z amassari $

AC_DEFUN([XERCES_CURL_PREFIX],
	[
	AC_ARG_WITH([curl],
		[AS_HELP_STRING([--with-curl[[[[=DIR]]]]],[Specify location of libcurl])],
		[with_curl=m4_if($with_curl, [yes], [], $with_curl)],
		[with_curl=])

	# Determine if curl is available
	AC_CACHE_CHECK([for libcurl], [xerces_cv_curl_prefix],
	[	
		xerces_cv_curl_prefix=
		if test x"$with_curl" != x"no"; then
			search_list="$with_curl /usr/local /usr"
			for i in $search_list; do
				if test -r "$i/include/curl/easy.h" -a -r "$i/include/curl/multi.h" ; then
					xerces_cv_curl_prefix=$i
					break
				fi
			done
		fi
	])

	AC_SUBST([CURL_PREFIX], [$xerces_cv_curl_prefix])
	]
)

