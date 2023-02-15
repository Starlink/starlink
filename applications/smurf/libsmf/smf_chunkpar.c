/*
*+
*  Name:
*     smf_chunkpar

*  Purpose:
*     Get a scalar parameter value to use with a specified chunk.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     result = smf_chunkpar( smfData *data, const char *par, const char *label,
*                            AstKeyMap *keymap, dim_t contchunk, int *status )

*  Arguments:
*     data = smfData * (Given)
*        The smfData holding the current chunk.
*     par = const char * (Given)
*        The parameter name.
*     label = const char * (Given)
*        A descriptive name of the parameter quantity for use in error
*        messages (e.g. "weight" "scale factor", etc).
*     keymap = AstKeyMap * (Given)
*        The KeyMap holding the configuration parameters.
*     contchunk = dim_t (Given)
*        The zero-based index of the current chunk.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     The parameter value.

*  Description:
*     Gets a value for a named config parameter that has a separate value
*     for each chunk of SCUBA-2 time series data. The returned value is
*     determined by the specified parameter in the supplied KeyMap. If
*     this is a vector of doubles, then the value of the element with
*     index "contchunk" is returned. If the vector does not contain
*     enough elements, then the value of the last element is returned. if
*     the named parameter is a text string, then it is assumed to be
*     the name of a FITS keyword that has a numerical value within the
*     header of the supplied smfData. An error is reported if neither of
*     these cases applies.

*  Authors:
*     David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     10-APR-2018 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory.
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
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Max number of chunks. */
#define MAXCHUNKS 200

double smf_chunkpar( smfData *data, const char *par, const char *label,
                     AstKeyMap *keymap, dim_t contchunk, int *status ){

/* Local Variables: */
   const char *cval = NULL;
   double result = 0.0;
   double values[ MAXCHUNKS ];
   int nval;
   AstKeyMap *maths = NULL;

/* Initialise the returned value to a safe value. */
   result = 1.0;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* Attempt to get the value of the named config parameter as vector
   of doubles. */
   if( astMapGet1D( keymap, par, MAXCHUNKS, &nval, values ) ){

/* Get the required element value, using the last element if the vector
   is too short. */
      if( contchunk < (dim_t) nval ) {
         result = values[ contchunk ];
      } else {
         result = values[ nval - 1 ];
      }
      msgOutiff( MSG__VERB, "", "Using a %s of %g for chunk %d "
                 "specified by config parameter %s", status, label,
                 result, (int) contchunk, par );

/* If the named parameter is not a vector of numerical values,
   annull the error and try to access it first as a text string. */
   } else if( *status == AST__MPGER ) {
      errAnnul( status );

/* We need a FITS header. */
      if( !data->hdr ) {
         *status = SAI__ERROR;
         errRepf( "", "SMF_CHUNKPAR: Supplied smfData has no FITS "
                  "header (programming error).", status );

/* Attempt to get the value as a text string. */
      } else if( astMapGet0C( keymap, par, &cval ) ){

/* If successful, try to create a MathMap that treats the text string as an
   algebraic function of a selection of FITS keywords, and evaluate this
   function using the FITS keyword values in the suppleid header. */
         maths = smf_fits_maths( NULL, cval, data->hdr, &result, status );

/* Store the "maths" object in the KeyMap. Since it is itself an AST
   object, it will be annulled when the KeyMap is annulled. */
         int oldval = astGetI( keymap, "MapLocked" );
         astSetI( keymap, "MapLocked", 0 );
         astMapPut0A( keymap, par, maths, NULL );
         astSetI( keymap, "MapLocked", oldval );

/* Now that we have saved a pointer to the "maths" object in the KeyMap,
   we can annul our local copy of the pointer without the object being
   destroyed. */
         maths = astAnnul( maths );

/* If the parameter value could not be read as a text string, annull
   the error and attempt to get it as an AST Object. */
      } else if( *status == AST__MPGER ) {
         errAnnul( status );
         if( astMapGet0A( keymap, par, &maths ) ){

/* If successful, use the "maths" object to evaluate the FITS expression. */
            (void) smf_fits_maths( maths, NULL, data->hdr, &result, status );

/* Annul the local pointer. */
            maths = astAnnul( maths );
         }
      }

/* Tell the user what value we are using. */
      msgOutiff( MSG__VERB, "", "Using a %s of %g for chunk %d read from "
                 "FITS header expression '%s'", status, label, result,
                 (int) contchunk, cval );
   }

   return result;
}

