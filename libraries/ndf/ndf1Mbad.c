#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Mbad( int badok, int n, const int ndfs[], const char *comp,
               int check, int *bad, int *status ){
/*
*+
*  Name:
*     ndf1Mbad

*  Purpose:
*     Merge NDF bad pixel flag values.

*  Synopsis:
*     void ndf1Mbad( int badok, int n, const int ndfs[], const char *comp,
*                    int check, int *bad, int *status )

*  Description:
*     This function obtains the bad pixel flag values for an array
*     component of a sequence of NDFs and returns the logical "or" of the
*     result. If the result is non-zero but the "badok" parameter is set to
*     zero (indicating that the application cannot process bad values) then
*     an error is reported to this effect and "status" set.

*  Parameters:
*     badok
*        Whether the application can handle arrays containing bad values.
*     n
*        Number of NDFs whose bad pixel flags are to be matched.
*     ndfs
*        Array of identifiers for the NDFs. The supplied "ndfs" array
*        should have at least "n" elements.
*     comp
*        Pointer to a null terminated string holding the name of the array
*        component whose bad pixel flag is to be used: "DATA", "QUALITY" or
*        "VARIANCE".
*     check
*        Whether to make an explicit check, if necessary. to ensure that
*        bad pixels are actually present.
*     *bad
*        Returned holding the value of the combined bad pixel flag.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied in
*     which case the function will take all the specified components into
*     consideration when calculating the combined bad pixel flag value.
*     -  The effective value of the bad pixel flag used by this function
*     for each NDF component is the same as would be returned by the ndfBad
*     function.
*     -  If the function detects the presence of bad pixels which the
*     application cannot support, then a "status" value of NDF__BADNS (bad
*     pixels not supported) will be returned, as defined in the include
*     file "ndf_err.h". The value of the "bad" parameter will be set to
*     non-zero under these circumstances.

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   int i;                /* Loop counter for NDFs */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise the result. */
   *bad = 0;

/* Loop to consider each NDF in turn. */
   for( i = 0; i < n; i++ ){

/* Import the NDF identifier. */
      ndf1Impid( ndfs[ i ], &acb, status );

/* If the result is not yet set to non-zero, then determine the bad pixel
   flag value for this NDF. */
      if( !*bad ) {
         ndf1Bad( acb, comp, check, bad, status );
         if( *status == SAI__OK ) {

/* If a non-zero result was obtained, but the application cannot handle
   bad pixels, then report an error. */
            if( *bad && ( !badok ) ) {
               *status = NDF__BADNS;
               ndf1Amsg( "NDF", acb );

/* Ensure that the error message indicates the appropriate degree of
   certainty about whether bad pixel values are present. */
               if( check ) {
                  errRep( " ", "The NDF structure ^NDF contains \"bad\" "
                          "pixel values which cannot be handled correctly "
                          "by this application.", status );
               } else {
                  errRep( " ", "The NDF structure ^NDF may contain \"bad\" "
                          "pixel values which cannot be handled correctly "
                          "by this application.", status );
               }
            }
         }
      }

/* Quit considering NDFs once an error occurs. */
      if( *status != SAI__OK ) break;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Mbad", status );

}

