#include <stdio.h>
#include <string.h>
#include "sae_par.h"
#include "chr.h"

void chrCtor( const char *string, float *rvalue, int *status ){
/*
*+
*  Name:
*     chrCtor

*  Purpose:
*     Read a float value from a string.

*  Synopsis:
*     void chrCtor( const char *string, float *rvalue, int *status )

*  Description:
*     Read a float value from the given character string.

*  Parameters:
*     string
*        Pointer to a null terminated string holding the string from which
*        a float value is to be read.
*     *rvalue
*        Returned holding the resulting float value.
*     *status
*        The status value: if this value is not SAI__OK on input, the
*        function returns without action; if the function does not complete
*        successfully, "status" is returned set to SAI__ERROR.

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

/* Local Variables: */
   int len;              /* Length of string */
   int nc;               /* Number of characters read by sscanf */

/* Check the inherited status. */
   if( *status != SAI__OK || !string ) return;

/* Trap any commas in the given string or blank string. */
   if( strchr( string, ',' ) || chrLen( string ) == 0  ){
      *status = SAI__ERROR;

/* If there are no commas or spaces in the string... */
   } else {

/* Read the value into a float. */
      len = (int) strlen( string );
      nc = 0;
      if( ( 1 != sscanf( string, "%g %n", rvalue, &nc )) || (nc < len ) ){
         *status = SAI__ERROR;
      }
   }

/* Check the returned status value and set the returned INTEGER value
   on error. */
   if( *status == SAI__ERROR ) *rvalue = 0;
}


