#include <stdlib.h>
#include <string.h>
#include "ndf1.h"
#include "sae_par.h"
#include "prm_par.h"

void ndf1Rdtun( const char *name, int dflt, int *value, int *status ){
/*
*+
*  Name:
*     ndf1Rdtun

*  Purpose:
*     Read a tuning parameter value from an environment variable.

*  Synopsis:
*     void ndf1Rdtun( const char *name, int dflt, int *value, int *status )

*  Description:
*     This function reads an NDF_ system tuning parameter from an
*     environment variable and attempts to convert it to an integer. If it
*     succeeds, this integer value is returned.  Otherwise, a default value
*     is returned.

*  Parameters:
*     name
*        Pointer to a null terminated string holding the name of the
*        environment variable.
*     dflt
*        The default value to be returned if no environment variable value
*        can be obtained.
*     *value
*        The returned value.
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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char cval[ VAL__SZI + 1 ];      /* Environment variable value */
   char *endptr;         /* Pointer to first non-digit */
   int def;              /* Environment variable defined? */
   size_t lval;          /* Length of environment variable value */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Attempt to translate the environment variable. */
   ndf1Gtenv( name, &def, cval, sizeof( cval ), &lval, status );

/* If a translation was found, attempt to convert it to an integer
   value. */
   if( *status == SAI__OK ) {
      if( def ) {
         *value = strtol( cval, &endptr, 10 );

/* If not successful, then use the default value. */
         if( cval[0] == 0 || endptr[0] != 0 ) *value = dflt;

/* Also use the default if the environment variable had no translation. */
      } else {
         *value = dflt;
      }
   }

}

