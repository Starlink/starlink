#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "star/util.h"

void ndf1H2ast( const char *data[], int iline, char *line,
                size_t line_length, int *status ){
/*
*+
*  Name:
*     ndf1H2ast

*  Purpose:
*     Copy AST_ data from an HDS object.

*  Synopsis:
*     void ndf1H2ast( const char *data[], int iline, char *line,
*                     size_t line_length, int *status )

*  Description:
*     This function copies a line of text representing AST_ data from a
*     specified element of a 1-dimensional character array. It is intended
*     for use when reading AST_ data from an HDS object (i.e an HDS _CHAR
*     array).

*  Parameters:
*     data
*        Pointer to a null terminated string holding the character array
*        from which the text is to be copied.
*     iline
*        The index of the element in "data" which is to provide the text
*        (the contents of other elements are ignored).
*     line
*        Pointer to an array in which to return a null terminated string
*        holding the line of text obtained.
*     line_length
*        The length of the supplied 'line' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

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

/* Extract the required line of text. */
   star_strlcpy( line, data[ iline - 1 ], line_length );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1H2AST", status );

}

