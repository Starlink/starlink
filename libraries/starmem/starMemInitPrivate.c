
#if HAVE_CONFIG_H
# include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Private includes */
#include "mem.h"
#include "mem1.h"



/*
*  Name:
*     starMemInitPrivate

*  Purpose:
*     Starlink memory management private initialisation routine

*  Invocation:
*     void starMemInitPrivate( int gc_initialised );

*  Description:
*     This function is called from the starMemInit public macro
*     to initialise the memory management routine. This function
*     controls whether or not garbage collection is available. If it
*     is not called, garbage collection is disabled (mainly because
*     the GC initialize routine must be called for the GC to work
*     portably). The particular malloc implementation is set by this
*     routine GC from the STARMEM_MALLOC environment
*     variable. See NOTES for more information on this variable.

*  Parameters:
*     gc_initialised = int (Given)
*        If true, GC_INIT has been called (and so GC may be enabled).
*        If false, GC_INIT has not been called.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     08-FEB-2006 (TIMJ):
*        Original version.
*     23-FEB-2006 (TIMJ):
*        Add DL
*     25-FEB-2006 (TIMJ):
*        Add STARMEM_PRINT_INFO
*     26-APR-2006 (TIMJ):
*        Check for libgc *and* gc.h

*  Notes:
*     - This function is private and should only be called from the
*       starMemInit macro.
*     - The STARMEM_MALLOC environment variable can be used to control
*       the malloc behaviour. It can have the following values:
*       AST: Use AST for malloc/free. This can be dangerous and will only
*           be enabled via special builds of the library. Use with care.
*       DL: Use Doug Lea's malloc (dlmalloc).
*           http://g.oswego.edu/dl/html/malloc.html
*       GC: Garbage Collection must be used. An error occurs if that is
*           not available. Since EMS is unavailable, the error will
*           be printed to stderr and the system will fallback to SYSTEM.
*       SYSTEM: Use normal system malloc/free. This is useful for valgrind
*           usage (valgrind does not work with GC).
*       If the variable is unset, GC is used if available else SYSTEM is
*       used.
*     - Returns without action if the function has already been called.
*     - From testing, the DL malloc is the fastest. The GC malloc is
*       generally slower and also has real problems if used for large
*       repetitive mallocs (where large can be of order 30 kB). This makes
*       it unsuitable for general starlink applications but can be useful
*       for smaller applications.
*     - Currently, the SYSTEM malloc is the default if STARMEM_MALLOC
*       environment variable is not defined.
*     - If the STARMEM_PRINT_INFO environment variable is defined
*       the selected malloc will be printed to stdout.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*/

void
starMemInitPrivate( int gc_initialised ) {
  char * starenv;   /* STARMEM_MALLOC environment values */

  /* return immediately if we have been initialised already */
  if ( STARMEM_INITIALISED ) return;

#if STARMEM_DEBUG
  /* see if STARMEM_PRINT_MALLOC is defined */
  if ( getenv( "STARMEM_PRINT_MALLOC" ) ) {
    STARMEM_PRINT_MALLOC = 1;
  }
#endif

  /* See if STARMEM_PRINT_INFO is defined */
  if ( getenv( "STARMEM_PRINT_INFO" ) ) {
    STARMEM_PRINT_INFO = 1;
  }

  /* Read the STARMEM_MALLOC environment variable */
  starenv = getenv( "STARMEM_MALLOC" );

  if (STARMEM_PRINT_INFO)
    printf("Attempting to use malloc '%s'\n", starenv);

  /* Indicate that we are initialised and default to "SYSTEM" malloc */
  STARMEM_MALLOC = STARMEM__SYSTEM;
  STARMEM_INITIALISED = 1;

  if (starenv == NULL) {
    /* default behaviour */
    if (STARMEM_PRINT_INFO)
      printf("Default behaviour for malloc\n");

    return;
  } else if (strncmp(starenv, "SYS", 3) == 0) {
    /* use system version */
    STARMEM_MALLOC = STARMEM__SYSTEM;

  } else if (strncmp(starenv, "AST", 3) == 0) {
    /* use AST version */
    STARMEM_MALLOC = STARMEM__AST;

  } else if (strncmp(starenv, "DL", 2) == 0) {
    /* use system version */
    STARMEM_MALLOC = STARMEM__DL;

  } else if (strncmp(starenv, "GC", 2) == 0 ) {
    /* Garbage Collector mandatory */
#if HAVE_LIBGC && HAVE_GC_H
    if (gc_initialised) {
      STARMEM_MALLOC = STARMEM__GC;
    } else {
      fprintf(stderr, "Garbage collection requested but GC has not been initialised in main program. Please call starMemInit()\n");
    }
#endif

    if (STARMEM_MALLOC != STARMEM__GC) {
      /* we have not got GC but we needed it - we can not use ems */
      fprintf(stderr, "Garbage Collection requested but Garbage Collection not available. Falling back to system malloc");
      STARMEM_MALLOC = STARMEM__SYSTEM;

    }

  } else {
    /* unknown option */
    fprintf(stderr, "Unknown malloc method requested ('%s'). Using system malloc.\n", starenv);
    STARMEM_MALLOC = STARMEM__SYSTEM;

  }

  if (STARMEM_PRINT_INFO)
    printf("Selected malloc %d\n", STARMEM_MALLOC );

  return;
}
