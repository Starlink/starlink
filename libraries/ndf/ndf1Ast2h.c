#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "star/util.h"

void ndf1Ast2h( char *data[], size_t data_length, int iline,
                const char *line, int *status ){
/*
*+
*  Name:
*     ndf1Ast2h

*  Purpose:
*     Copy AST_ data to an HDS object.

*  Synopsis:
*     void ndf1Ast2h( char *data[], size_t data_length, int iline,
*                     const char *line, int *status )

*  Description:
*     This function copies a line of text representing AST_ data into a
*     specified element of a 1-dimensional character array. It is intended
*     for use when writing AST_ data to an HDS object (i.e an HDS _CHAR
*     array).

*  Parameters:
*     data
*        Pointer to a null terminated string holding the character array
*        into which the text is to be copied.
*     data_length
*        The length of the supplied 'data' array. This should include
*        room for the terminating null.
*     iline
*        The index of the element in "data" which is to receive the text
*        (the contents of other elements are returned unchanged).
*     line
*        Pointer to a null terminated string holding the line of text to be
*        inserted.
*     *status
*        The global status.

*  Notes:
*     This function departs from the conventional parameter order so as to
*     accommodate the case where the "data" parameter is a mapped HDS
*     character array.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Copy the line of text. */
   star_strlcpy( data[ iline - 1 ], line, data_length );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Ast2H", status );

}

