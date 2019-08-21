#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

char **ndf1Freewords( int len, char **strings ){
/*
*+
*  Name:
*     ndf1Freewords

*  Purpose:
*     Free a list of substrings created by ndf1Findwords.

*  Synopsis:
*     char **ndf1Freewords( int len, char **strings )

*  Description:
*     This function frees resources used by a list of strings
*     created by ndf1Findwords.

*  Parameters:
*     len
*        The number of strings in the list.
*     string
*        The pointer returned by ndf1Findwords.

*  Returned value:
*     Always returns NULL.

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
   int i;
   if( strings ) {
      for( i = 0; i < len; i++ ) strings[ i ] = astFree( strings[ i ] );
      strings = astFree( strings );
   }

   return NULL;
}

