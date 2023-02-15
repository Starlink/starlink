/*
*+
*  Name:
*     smf_get_nsamp

*  Purpose:
*     Get a configuration parameter value, in units of time slices.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     double smf_get_nsamp( AstKeyMap *keymap, const char *name,
*                           const smfData *data, dim_t *nsamp, int *status )

*  Arguments:
*     keymap = AstKeyMap * (Given)
*        Pointer to the KeyMap. If NULL, then a value of one is returned
*        for "*nsamp" and the function value.
*     name = const char * (Given)
*        The name of the configuration parameter.
*     data = const smfData * (Given)
*        If not NULL, the step-time in the supplied data is used to
*        convert between seconds and time-samples. If NULL, then a
*        time step of 0.005 seconds (ie. 200 Hz sample rate) is assumed.
*     nsamp = dim_t * (Returned)
*        The value of the requested configuration parameter, in units of
*        time-samples. If an error occurs, the value on entry is left
*        unchanged.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     The raw floating-point value read from the KeyMap (may be
*     negative), or 1.0 if no KeyMap was supplied. Zero is returned if an
*     error occurs.

*  Description:
*     This function gets a value for a named configuration parameter from
*     a supplied KeyMap. If the value is positive, it is assumed to be in
*     units of time slices, and the nearest integer value is returned in
*     "*nsamp". If the value is negative, its absolute value is assumed to
*     be in units of seconds and is converted to time slices using the sample
*     rate associated with the supplied smfData. The nearest integer value
*     is then returned in "*nsamp".

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-FEB-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

double smf_get_nsamp( AstKeyMap *keymap, const char *name,
                      const smfData *data, dim_t *nsamp, int *status ){


/* Local Variables */
   double dval = 0.0;
   double result = 0.0;
   double steptime;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* If no KeyMap was supplied, return 1.0 */
   if( !keymap ) {
      result = 1.0;
      *nsamp = 1;

/* Otherwise, read the value from the KeyMap as a floating point value. */
   } else if( astMapGet0D( keymap, name, &result ) ) {

/* If the value in the KeyMap is negative, we assume it is in units of
   seconds. */
      if( result < 0.0 ) {

/* Set the default step time corresponding to 200 Hz sample rate. */
         steptime = 0.005;

/* Override this with the steptime from the supplied smfData, if any. */
         if( data ) {
            if( data->hdr ) {
               steptime = data->hdr->steptime;
               if( steptime <= 0.0 && *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRepf( " ", "smf_get_nsamp: The steptime value in the "
                           "supplied smfData (%g) is negative or zero.",
                           status, steptime );
               }
            } else if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRepf( " ", "smf_get_nsamp: The supplied smfData has no "
                        "hdr component.", status );
            }
         }

/* Do the conversion from (negative) seconds, to (positive) time slices. */
         if( steptime > 0.0 ) dval = -result/steptime;

/* If the value in the KeyMap is positive, we assume it is in units of
   time slices. */
      } else {
         dval = result;
      }

/* Return the nearest integer number of times slices. */
      if( *status == SAI__OK ) *nsamp = (dim_t) ( dval + 0.5 );
   }

/* Return the raw value read from the KeyMap. */
   return result;
}

