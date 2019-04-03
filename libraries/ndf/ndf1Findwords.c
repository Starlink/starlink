#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

char **ndf1Findwords( const char *text, int *el, int *status ){
/*
*+
*  Name:
*     ndf1Findwords

*  Purpose:
*     Split a string into a list of comma-separated substrings.

*  Synopsis:
*     char **ndf1Findwords( const char *text, int *el, int *status )

*  Description:
*     This function splits the supplied string up using commas as
*     delimiters, and returns the resulting substrings in an array, with
*     leading and trailing spaces removed.

*  Parameters:
*     text
*        The null terminated stext string to split.
*     *el
*        Returned holding the number of substrings returned.
*     *status
*        The global status.

*  Returned value:
*     Pointer to an array that has "*el" elements. Each element holds a
*     pointer to a null terminated string. The returned resources should
*     be freed using ndf1Freewords when no longer needed.

*  Notes:
*     -  NULL is returned if an error occurs.

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

/* Local Variables: */
   char **result;
   int i;

/* Check inherited global status. */
   if( *status != SAI__OK ) return NULL;

/* Split the strings. */
   result = astChrSplitC( text, ',', el );

/* Remove leading and trailing spaces from each returned substring. */
   for( i = 0; i < *el; i++ ) {
      astRemoveLeadingBlanks( result[ i ] );
      astChrTrunc( result[ i ] );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Findwords", status );

/* Return the result. */
   return result;
}

