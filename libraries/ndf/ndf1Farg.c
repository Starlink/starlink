#include <stdlib.h>

void ndf1Farg( int index, char *value, size_t value_length ){
/*
*+
*  Name:
*     ndf1Farg

*  Purpose:
*     Get a command-line parameter from the Fortran function GETARG.

*  Synopsis:
*     void ndf1Farg( int index, char *value, size_t value_length )

*  Description:
*     Gets the "index"-th command-line parameter from Fortran using the
*     non-standard intrinsic GETARG. We use this rather than calling GETARG
*     directly from the C function ndf_gtarg.c as namespace mangling means
*     that Fortran intrinsics are not always available from C.

*  Parameters:
*     index
*        The index of the parameter to return.
*     value
*        Pointer to an array in which to return a null terminated string
*        holding the value of the "index"-th parameter.
*     value_length
*        The length of the supplied 'value' array. This should include
*        room for the terminating null.

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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/


   getarg( index, value );
}

