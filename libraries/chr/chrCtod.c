#include <stdio.h>
#include <string.h>
#include "sae_par.h"
#include "chr.h"

void chrCtod( const char *string, double *dvalue, int *status ){
/*
*+
*  Name:
*     chrCtod

*  Purpose:
*     Read a double value from a string.

*  Synopsis:
*     void chrCtod( const char *string, double *dvalue, int *status )

*  Description:
*     Read a double value from the given character string.

*  Parameters:
*     string
*        Pointer to a null terminated string holding the string from which
*        a double value is to be read.
*     *dvalue
*        Returned holding the resulting double value.
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
   if( *status != SAI__OK ) return;

/* Trap any commas in the given string or blank string. */
   if( strchr( string, ',' ) || chrLen( string ) == 0 ){
      *status = SAI__ERROR;

/* If there are no commas or spaces in the string... */
   } else {

/* Read the value into a double. */
      len = (int) strlen( string );
      nc = 0;
      if( ( 1 != sscanf( string, "%lg %n", dvalue, &nc )) || (nc < len ) ){
         *status = SAI__ERROR;
      }
   }

/* Check the returned status value and set the returned INTEGER value
   on error. */
   if( *status == SAI__ERROR ) *dvalue = 0;
}

