#include <string.h>
#include "star/hds.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf_ast.h"

char *ndf1Hmp0C( HDSLoc *loc, int *status ){
/*
*+
*  Name:
*     ndf1Hmp0C

*  Purpose:
*     Get a scalar null-terminated string from an HDS locator.

*  Synopsis:
*     char *ndf1Hmp0C( HDSLoc *loc, int *status )

*  Description:
*     This function returns a pointer to dynamically allocated memory
*     holding a null-terminated copy of a string read from an HDS locator.
*     It is needed because datMap0C (etc) does not null-terminate the
*     returned string. The returned pointer should be freed using astFree
*     when no longer needed.

*  Parameters:
*     loc
*        The locator.
*     *status
*        The global status.

*  Returned Value:
*     Pointer to the memory holding the null-terminated copy of the string
*     held in the HDS locator, or NULL if an error occurs.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     1-FEB-2019 (DSB):
*        Original version.

*-
*/

/* Local Variables: */
   char *result = NULL;
   hdsdim dimv[ DAT__MXDIM ];
   size_t len;
   unsigned char *pntr;

/* Check inherited global status. */
   if( *status != SAI__OK ) return result;

/* Get a pointer to the fixed length string */
   datMapC( loc, "READ", 0, dimv, &pntr, status );

/* Get the length of the string */
   datClen( loc, &len, status );

/* Copy the text into a dynamically allocated buffer, including an extra
   space for a terminating null character. */
   result = astMalloc( len + 1 );
   if( result ) {
      memcpy( result, pntr, len );
      result[ len ] = 0;
   }

   return result;
}

