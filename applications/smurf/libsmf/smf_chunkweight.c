/*
*+
*  Name:
*     smf_chunkweight

*  Purpose:
*     Get the weight to use for a specified chunk.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     result = smf_chunkweight( smfData *data, AstKeyMap *keymap,
*                               int contchunk, int *status )

*  Arguments:
*     data = smfData * (Given)
*        The smfData holding the current chunk.
*     keymap = AstKeyMap * (Given)
*        The KeyMap holding the configuration parameters.
*     contchunk = int (Given)
*        The zero-based index of the current chunk.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     The weight, or 1.0 if an error occurs.

*  Description:
*     Return the weight to use for the current chunk of SCUBA-2 time
*     series data. This is the relative weight to assign to the map
*     created from the current chunk, when combining the map with maps
*     from other chunks to create the final returned map. The weight is
*     determined from the CHUNKWEIGHT value in the supplied KeyMap. If
*     this is a vector of doubles, then the value of the element with
*     index "contchunk" is returned. If the vector does not contain
*     enough elements, then the value of the last element is returned. if
*     the CHUNKWEIGHT parameter is a text string, then it is assumed to be
*     the name of a FITS keyword that has a numerical value within the
*     header of the supplied smfData. An error is reported if neither of
*     these cases applies.

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     7-OCT-2013 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
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
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Max number of chunks. */
#define MAXCHUNKS 100

double smf_chunkweight( smfData *data, AstKeyMap *keymap, int contchunk,
                        int *status ){

/* Local Variables: */
   const char *cval = NULL;
   double result = 0.0;
   double values[ MAXCHUNKS ];
   int nval;
   AstKeyMap *maths = NULL;

/* Initialise the returned weight to a safe value. */
   result = 1.0;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* Attempt to get the value of the CHUNKWEIGHT config parameter as vector
   of doubles. */
   if( astMapGet1D( keymap, "CHUNKWEIGHT", MAXCHUNKS, &nval, values ) ){

/* Get the required element value, using the last element if the vector
   is too short. */
      if( contchunk < nval ) {
         result = values[ contchunk ];
      } else {
         result = values[ nval - 1 ];
      }
      msgOutiff( MSG__VERB, "", "smf_chunkweight: Using a weight of %g "
                 "for chunk %d specified by config parameter CHUNKWEIGHT",
                 status, result, contchunk );

/* If the CHUNKWEIGHT parameter is not a vector of numerical values,
   annull the error and try to access it first as a text string. */
   } else if( *status == AST__MPGER ) {
      errAnnul( status );

/* We need a FITS header. */
      if( !data->hdr ) {
         *status = SAI__ERROR;
         errRepf( "", "SMF_CHUNKWEIGHT: Supplied smfData has no FITS "
                  "header (programming error).", status );

/* Attempt to get the value as a text string. */
      } else if( astMapGet0C( keymap, "CHUNKWEIGHT", &cval ) ){

/* If successful, try to create a MathMap that treats the text string as an
   algebraic function of a selection of FITS keywords, and evaluate this
   function using the FITS keyword values in the suppleid header. */
         maths = smf_fits_maths( NULL, cval, data->hdr, &result, status );

/* Store the "maths" object in the KeyMap. Since it is itself an AST
   object, it will be annulled when the KeyMap is annulled. */
         int oldval = astGetI( keymap, "MapLocked" );
         astSetI( keymap, "MapLocked", 0 );
         astMapPut0A( keymap, "CHUNKWEIGHT", maths, NULL );
         astSetI( keymap, "MapLocked", oldval );

/* Now that we have saved a pointer to the "maths" object in the KeyMap,
   we can annul our local copy of the pointer without the object being
   destroyed. */
         maths = astAnnul( maths );

/* If the CHUNKWEIGHT value could not be read as a text string, annull
   the error and attempt to get it as an AST Object. */
      } else if( *status == AST__MPGER ) {
         errAnnul( status );
         if( astMapGet0A( keymap, "CHUNKWEIGHT", &maths ) ){

/* If successful, use the "maths" object to evaluate the FITS expression. */
            (void) smf_fits_maths( maths, NULL, data->hdr, &result, status );

/* Annul the local pointer. */
            maths = astAnnul( maths );
         }
      }

/* Tell the user what value we are using. */
      msgOutiff( MSG__VERB, "", "smf_chunkweight: Using a weight of %g "
                 "for chunk %d read from FITS header expression '%s'",
                 status, result, contchunk, cval );
   }

/* Give a context message if anything went wrong. */
   if( *status != SAI__OK ) {
      errRep( "", "Failed to get the weight for the current chunk using "
              "configuration parameter CHUNKWEIGHT.", status );
   }

   return result;
}

