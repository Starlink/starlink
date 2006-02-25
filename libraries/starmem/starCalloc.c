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
*     starCalloc

*  Purpose:
*     Starlik memory allocator (initialising all memory)

*  Invocation:
*     void * starCalloc( size_t nmemb, size_t size );

*  Description:
*     This function allocates memory using the memory management scheme
*     selected with a call to starMemInit(). Its interface is deliberately
*     intended to match the ANSI-C standard and so can be a drop in
*     replacement for system calloc(). The memory will be initialised.

*  Parameters:
*     nmemb = size_t (Given)
*        Number of array elements to allocate.
*     size = size_t (Given)
*        Number of bytes to allocate per array element

*  Returned Value:
*     starCalloc = void * (Returned)
*        Pointer to allocated memory. NULL if the memory could not be obtained.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     09-FEB-2006 (TIMJ):
*        Original version.

*  Notes:
*     - The Garbage Collector malloc is only available if starMemInit() has
*       been invoked from the main program (not library) before this call.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*/

void * starCalloc( size_t nmemb, size_t size ) {
  void * tmp = NULL;

  switch (STARMEM_MALLOC) {

  case STARMEM__SYSTEM:
    tmp = calloc( nmemb, size );
    break;

  case STARMEM__DL:
    tmp = dlcalloc( nmemb, size );
    break;

  case STARMEM__GC:
#if HAVE_LIBGC
    /* Call normal malloc since we know the GC initialises memory */
    tmp = GC_MALLOC( nmemb * size );
#else
    starMemFatalGC;
#endif
    break;

  default:
    starMemFatalNone;

  }

#if STARMEM_DEBUG
  if (STARMEM_PRINT_MALLOC)
    printf(__FILE__": Allocated %lu elements of size %lu bytes into pointer %p\n",
	   (unsigned long)nmemb, (unsigned long)size, tmp );
#endif

  return tmp;
}
