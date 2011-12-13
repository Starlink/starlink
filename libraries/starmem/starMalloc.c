#if HAVE_CONFIG_H
# include <config.h>
#endif

/* System includes */
#include <stdlib.h>

/* Private includes */
#include "mem.h"
#include "mem1.h"


/*
*  Name:
*     starMalloc

*  Purpose:
*     Starlink memory allocator

*  Invocation:
*     void * starMalloc( size_t size );

*  Description:
*     This function allocates memory using the memory management scheme
*     selected with a call to starMemInit(). Its interface is deliberately
*     intended to match the ANSI-C standard and so can be a drop in
*     replacement for system malloc. Depending on which allocator
*     is chosen, the memory returned by this routine may or may have been
*     initialised.

*  Parameters:
*     size = size_t (Given)
*        Number of bytes to allocate.

*  Returned Value:
*     starMalloc = void * (Returned)
*        Pointer to allocated memory. NULL if the memory could not be obtained.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     09-FEB-2006 (TIMJ):
*        Original version.
*     23-FEB-2006 (TIMJ):
*        Use switch to select malloc
*     25-FEB-2006 (TIMJ):
*        Force initialisation first time through.
*     26-APR-2006 (TIMJ):
*        Check for libgc *and* gc.h

*  Notes:
*     - The Garbage Collector malloc is only available if starMemInit() has
*       been invoked from the main program (not library) before this call.
*     - If you know that the memory allocated by this routine will never
*       contain pointers (e.g. it's for an atomic chunk of memory such as a
*       string or an image) there may be a performance gain in using
*       starMallocAtomic instead of starMalloc.
*     - This memory must be freed either by starFree() or starFreeForce()
*       and never the system free().

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

void * starMalloc( size_t size ) {
  void * tmp = NULL;
  static const size_t THRESHOLD = 1024 * 100; /* Bytes */
#if USE_AST_MALLOC
  int ast_status = 0;
  int *old_ast_status = NULL;
#endif

  /* Force initialisation - only needed when allocating not freeing
     since free is too late. Note that we have clearly not run any
     initialisation macros at this point so pass in false. If this
     is overhead is too high, remove it and force a call to
     starMemInit */
  if ( ! STARMEM_INITIALISED ) starMemInitPrivate(0);

  /* Decide which to use */
  switch ( STARMEM_MALLOC ) {

  case STARMEM__SYSTEM:
    tmp = malloc( size );
    break;

  case STARMEM__AST:
#if USE_AST_MALLOC
    old_ast_status = astWatch( &ast_status );
    tmp = astMalloc( size );
    astWatch( old_ast_status );
#else
    starMemFatalAST;
#endif
    break;

  case STARMEM__DL:
    tmp = dlmalloc( size );
    break;

  case STARMEM__GC:
#if HAVE_LIBGC && HAVE_GC_H
    if ( size < THRESHOLD ) {
      tmp = GC_MALLOC( size );
    } else {
      tmp = GC_MALLOC_IGNORE_OFF_PAGE( size );
    }
#else
    starMemFatalGC;
#endif
    break;

  default:
    starMemFatalNone;
  }

#if STARMEM_DEBUG
  if (STARMEM_PRINT_MALLOC)
    printf(__FILE__": Allocated %lu bytes into pointer %p\n",
	   (unsigned long)size, tmp );
#endif

  /* No GC so use system */
  return tmp;
}
