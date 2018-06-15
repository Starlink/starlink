#include <ctype.h>
#include "chr.h"

int chrIsalm( char cvalue ){
/*
*+
*  Name:
*     chrIsalm

*  Purpose:
*     Return whether a character is alphanumeric.

*  Synopsis:
*     int chrIsalm( char cvalue )

*  Description:
*     Determine whether a character is alphanumeric, i.e. A - Z, a - z, 0 -
*     9 or _. Note that this function treats the underscore character as an
*     alphanumeric character.

*  Parameters:
*     cvalue
*        The character to be tested.

*  Returned Value:
*     Returns non-zero if the given character is alphanumeric,

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
*     DSB: David S. Berry (EAO)

*  History:
*     15-MAY-2018 (DSB):
*        Original version, based on equivalent Fortran function.

*-
*/

   return ( isalnum( cvalue ) || cvalue == '_' );
}

