
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
*     portably). GC can be disabled using the STARMEM_MALLOC environment
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

*  Notes:
*     - This function is private and should only be called from the
*       starMemInit macro.
*     - The STARMEM_MALLOC environment variable can be used to control
*       the malloc behaviour. It can have the following values:
*       GC: Garbage Collection must be used. An error occurs if that is
*           not available. Since EMS is unavailable, the error will
*           be printed to stderr and the system will fallback to SYSTEM.
*       SYSTEM: Use normal system malloc/free. This is useful for valgrind
*           usage (valgrind does not work with GC).
*       If the variable is unset, GC is used if available else SYSTEM is
*       used.
*     - Returns without action if the function has already been called.

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

  /* Read the STARMEM_MALLOC environment variable */
  starenv = getenv( "STARMEM_MALLOC" );

  if (starenv == NULL) {
    /* default behaviour */
#if HAVE_GC_H
    if (gc_initialised) {
      STARMEM_USE_GC = 1;
    } else {
      /* did not initialise GC so use SYSTEM */
      STARMEM_USE_GC = 0;
    }
#else
    STARMEM_USE_GC = 0;
#endif
    STARMEM_INITIALISED = 1;
  } else if (strncmp(starenv, "SYSTEM", 6) == 0) {
    /* use system version */
    STARMEM_USE_GC = 0;
    STARMEM_INITIALISED = 0;

  } else if (strncmp(starenv, "GC", 2) == 0 ) {
    /* Garbage Collector mandatory */
#if HAVE_GC_H    
    if (gc_initialised) {
      STARMEM_USE_GC = 1;
      STARMEM_INITIALISED = 1;
    } else {
      fprintf(stderr, "Garbage collection requested but GC has not been initialised in main program. Please call starMemInit()\n");
      STARMEM_INITIALISED = 1;
      STARMEM_USE_GC = 0;
    }
#endif

    if (!STARMEM_INITIALISED) {
      /* we have not got GC but we needed it - we can not use ems */
      fprintf(stderr, "Garbage Collection requested but Garbage Collection not available. Falling back to system malloc");
      STARMEM_USE_GC = 0;
      STARMEM_INITIALISED = 1;
    }

  } else {
    /* unknown option */
    fprintf(stderr, "Unknown malloc method requested ('%s'). Using system malloc.\n", starenv);
    STARMEM_USE_GC = 0;
    STARMEM_INITIALISED = 1;
  }

}
