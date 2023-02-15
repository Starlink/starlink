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
*                               dim_t contchunk, int *status )

*  Arguments:
*     data = smfData * (Given)
*        The smfData holding the current chunk.
*     keymap = AstKeyMap * (Given)
*        The KeyMap holding the configuration parameters.
*     contchunk = dim_t (Given)
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
*     10-APR-2018 (DSB):
*        Most of the code factored out to file smf_chunkpar.c, so that it
*        can be re-used for accessing the chunk calibration factor.

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
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

/* SMURF includes */
#include "libsmf/smf.h"

double smf_chunkweight( smfData *data, AstKeyMap *keymap, dim_t contchunk,
                        int *status ){

/* Local Variables: */
   double result;

/* Initialise the returned weight to a safe value. */
   result = 1.0;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* Get the weight value. */
   result = smf_chunkpar( data, "CHUNKWEIGHT", "weight", keymap, contchunk,
                          status );

/* Give a context message if anything went wrong. */
   if( *status != SAI__OK ) {
      result = 1.0;
      errRep( "", "Failed to get the weight for the current chunk using "
              "configuration parameter CHUNKWEIGHT.", status );
   }

   return result;
}

