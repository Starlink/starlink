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
*     starFree

*  Purpose:
*     Really free memory allocated by starMalloc

*  Invocation:
*     void starFreeForce( void * ptr );

*  Description:
*     This function indicates to the memory allocator that memory should
*     be freed now. It may not be efficient to call this routine routinely
*     but can be useful to free large data arrays that are not intended
*     to be reused.

*  Parameters:
*     ptr = void * (Given)
*        Pointer to memory to be freed.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     09-FEB-2006 (TIMJ):
*        Original version.
*     26-APR-2006 (TIMJ):
*        Check for libgc *and* gc.h

*  Notes:
*     - An attempt will be made to free this memory regardless of
*       Garbage Collector or malloc scheme in use.
*     - Use starFree if the memory has no real need to be freed
*       immediately.
*     - This memory should have been allocated by starMalloc or related
*       routines, not the system malloc.

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

void starFreeForce( void * ptr ) {
#if USE_AST_MALLOC
  int ast_status = 0;
  int *old_ast_status = NULL;
#endif

#if STARMEM_DEBUG
  if (STARMEM_PRINT_MALLOC)
    printf(__FILE__": Free pointer %p\n", ptr );
#endif

  switch ( STARMEM_MALLOC ) {

  case STARMEM__SYSTEM:
    free( ptr );
    break;

  case STARMEM__AST:
#if USE_AST_MALLOC
    old_ast_status = astWatch( &ast_status );
    astFree( ptr );
    astWatch( old_ast_status );
#else
    starMemFatalAST;
#endif
    break;

  case STARMEM__DL:
    dlfree( ptr );
    break;

  case STARMEM__GC:
#if HAVE_LIBGC && HAVE_GC_H
    GC_FREE( ptr );
#else
    starMemFatalGC;
#endif
    break;

  default:
    starMemFatalNone;
  }

  return;
}
