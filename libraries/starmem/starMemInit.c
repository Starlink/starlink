
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

/* Prototype for fortran interface */
void starmem_init_( void );
void starMemInitFort( void );

/*
*  Name:
*     starMemInit

*  Purpose:
*     Starlink memory management initialisation routine

*  Invocation:
*     void starMemInit();

*  Description:
*     This function should be called from your main program
*     to initialise the memory management routine. This function
*     controls whether or not garbage collection is available. If it
*     is not called, garbage collection is disabled (mainly because
*     the GC initialize routine must be called for the GC to work
*     portably). GC can be disabled using the STARMEM_MALLOC environment
*     variable. See NOTES for more information on this variable.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     09-FEB-2006 (TIMJ):
*        Original version.
*     26-APR-2006 (TIMJ):
*        Check for libgc *and* gc.h

*  Notes:
*     - Call this from your main program. It is not part of the shared
*       library but will be included in the main program link using
*       starmem_link.
*     - The STARMEM_MALLOC environment variable can be used to control
*       the malloc behaviour. It can have the following values:
*       GC: Garbage Collection must be used. An error occurs if that is
*           not available. Since EMS is unavailable, the error will
*           be printed to stderr and the system will fallback to SYSTEM.
*       SYSTEM: Use normal system malloc/free. This is useful for valgrind
*           usage (valgrind does not work with GC).
*       If the variable is unset, GC is used if available else SYSTEM is
*       used.
*     - Can safely be called multiple times.
*     - In most cases this routine is implemented as a macro.
*     - A Fortran interface is provided as STARMEM_INIT()
*     - Fortran programs using STARMEM_INIT() must link explicitly
*       against $(STARLINK)/lib/starMemInit.o

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

void
starMemInitFort() {

  /* return immediately if we have been initialised already */
  if ( starMemIsInitialised() ) return;

#if HAVE_LIBGC && HAVE_GC_H
  GC_INIT();
  starMemInitPrivate( 1 );
#else
  starMemInitPrivate( 0 );
#endif

}

void
starmem_init_() {
  starMemInitFort();
}
