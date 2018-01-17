#include "sae_par.h"
#include "ary1.h"
#include "ary_ast.h"
#include "star/hds.h"

char *ary1Get0C( const HDSLoc *loc, int *status ){
/*
*+
*  Name:
*     ary1Get0C

*  Purpose:
*     Get the value of a scalar HDS object as a null-terminated character
*     string.

*  Synopsis:
*     char *ary1Get0C( const HDSLoc *loc, int *status )

*  Description:
*     The value of the supplied HDS scalar object is obtained as a
*     string, and returned as a null-terminated character string
*     stored in dynamically allocated memory.

*  Parameters:
*     loc
*        The HDS locator for the object.
*     status
*        The global status.

*  Returned function value:
*     A pointer to newly allocated dynamic memory holding the required
*     null-terminated string. This memory should be freed using astFree
*     when no longer needed.

*  Notes:
*     -  A NULL pointer is returned if an error has already occurred, or
*     if an error occurs during this function.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     22-JUN-2017 (DSB):
*        Original version.

*-
*/

/* Local Variables: */
   char *result;     /* The returned value */
   size_t clen;      /* Length of character string, excluding null */

/* Initialise */
   result = NULL;

/* Check the inherited STATUS value. */
   if( *status != SAI__OK ) return result;

/* Determine the length of the character string. */
   datClen( loc, &clen, status );

/* Allocate memory, including room for a terminating null. */
   clen++;
   result = astMalloc( clen*sizeof(*result) );

/* Read the string into the memory. */
   datGet0C( loc, result, clen, status );

/* If an error has occurred, attempt to free the memory. */
   if( *status != SAI__OK ) result = astFree( result );

/* Retun the result. */
   return result;
}
