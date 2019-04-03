#include <stdlib.h>
#include <star/util.h>
#include <ndf1_types.h>

void ndf1Ellip( char *str, size_t str_length ){
/*
*+
*  Name:
*     ndf1Ellip

*  Purpose:
*     Add an ellipsis to the end of a character string.

*  Synopsis:
*     void ndf1Ellip( char *str, size_t str_length )

*  Description:
*     This function adds an ellipsis "..." to the end of a character
*     string, to indicate that it has been truncated.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string to be
*        modified.
*     str_length
*        The length of the supplied 'str' array. This should include
*        room for the terminating null.

*  Notes:
*     This function does not perform error checking.

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

/* Append the ellipsis. */
   star_strlcpy( str + NDF_MAX( 1, str_length - 3 ) - 1, "...",
                 str_length - NDF_MAX( 1, str_length - 3 ) + 1 );

}

