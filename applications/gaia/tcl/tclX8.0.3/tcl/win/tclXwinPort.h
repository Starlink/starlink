/*
 * tclXwinPort.h
 *
 * Portability include file for MS Windows systems.
 *-----------------------------------------------------------------------------
 * Copyright 1996-1997 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tclXwinPort.h,v 8.2 1997/04/17 05:00:01 markd Exp $
 *-----------------------------------------------------------------------------
 */

#ifndef TCLXWINPORT_H
#define TCLXWINPORT_H

#include "tclWinPort.h"

#include <direct.h>
#include <assert.h>

/*
 * Types needed for fstat, but are not directly supported (we emulate).  If
 * defined before tclWinPort.h is include, it will define the access macros.
 */
#define S_IFIFO  _S_IFIFO               /* pipe */
#define S_IFSOCK 0140000                /* socket */

/*
 * OS feature definitons.
 */
#define NO_CATGETS
#define NO_FCHMOD
#define NO_FCHOWN
#define NO_FSYNC
#define NO_RANDOM  /* uses compat */
#define NO_SIGACTION
#define NO_SYS_SELECT_H
#define NO_TRUNCATE    /* FIX: Are we sure there is no way to truncate???*/
#define RETSIGTYPE void

#include <math.h>
#include <limits.h>

#ifndef MAXDOUBLE
#    define MAXDOUBLE HUGE_VAL
#endif
/*
 * Define a macro to call wait pid.  We don't use Tcl_WaitPid on Unix because
 * it delays signals.
 */
#define TCLX_WAITPID(pid, status, options) Tcl_WaitPid (pid, status, options)

#define bcopy(from, to, length)    memmove((to), (from), (length))

/*
 * Compaibility functions.
 */
extern long
random (void);

extern void
srandom (unsigned int x);

extern int
getopt (int           nargc,
        char * const *nargv,
        const char   *ostr);

#endif


