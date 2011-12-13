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
*     Free memory allocated by starMalloc

*  Invocation:
*     void starFree( void * ptr );

*  Description:
*     This function indicates to the memory allocator that memory can
*     be freed. Whether memory is actually freed is up to the particular
*     memory allocator in use. It should be used for small reusable
*     objects.

*  Parameters:
*     ptr = void * (Given)
*        Pointer to memory to be freed.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     09-FEB-2006 (TIMJ):
*        Original version.

*  Notes:
*     - Depending on malloc scheme in use, the memory may not be
*       freed immediately.
*     - Use starFreeForce if the memory really must be freed now.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

void starFree( void * ptr ) {
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
    /* Nothing to do if garbage collector selected */
    break;

  default:
    starMemFatalNone;
  }

  return;
}
