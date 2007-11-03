/*
 * tkPort.h --
 *
 *	This header file handles porting issues that occur because of
 *	differences between systems.  It reads in platform specific
 *	portability files.
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkPort.h,v 1.3 2002/08/31 06:12:26 das Exp $
 */

#ifndef _TKPORT
#define _TKPORT

#if defined(__WIN32__) || defined(_WIN32)
#define NEED_REAL_STDIO
#endif

#ifndef _Lang
#include "Lang.h"
#endif

#if defined(__WIN32__) || defined(_WIN32)
#   include "tkWinPort.h"
#   ifndef strcasecmp
#       define strcasecmp(a,b) stricmp(a,b)
#   endif
#   ifdef __CYGWIN__
#       undef strcasecmp
#   endif
#else
#   if defined(MAC_TCL)
#	include "tkMacPort.h"
#   elif defined(MAC_OSX_TK)
#	include "../macosx/tkMacOSXPort.h"
#   else
#       ifdef __PM__
#           include "tkOS2Port.h"
#       else
#           include "tkUnixPort.h"
#       endif
#   endif
#endif

#endif /* _TKPORT */
