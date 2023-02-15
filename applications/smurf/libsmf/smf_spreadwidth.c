/*
*+
*  Name:
*     smf_spreadwidth

*  Purpose:
*     Determines the width of the output volume into which each input pixel
*     value is spread by astRebinSeq.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_spreadwidth( int spread, const double params[], int *status )

*  Arguments:
*     spread = int (Given)
*        Specifies the scheme to be used for dividing each input data value
*        up amongst the corresponding output pixels. See docs for astRebinSeq
*        (SUN/211) for the allowed values.
*     params = const double[] (Given)
*        An optional pointer to an array of double which should contain any
*        additional parameter values required by the pixel spreading scheme.
*        See docs for astRebinSeq (SUN/211) for further information. If no
*        additional parameters are required, this array is not used and a
*        NULL pointer may be given.
*     status = int * (Given and Returned)
*        Pointer to an int holding the inherited status value.

*  Returned Value:
*     The diameter of the spherical volume in the output into which each input
*     pixel is spread, in output pixels. This is always an odd value. One is
*     returned if an error occurs.

*  Description:
*     This function returns the diameter (in output pixels) of the spherical
*     volume in the output array into which each input pixel is spread
*     when astRebinSeq uses the supplied spreading scheme and parameter values.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     21-MAY-2008 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

int smf_spreadwidth( int spread, const double params[], int *status ){

/* Local Variables */
   AstUnitMap *umap = NULL;
   float *w = NULL;
   float *work = NULL;
   float val;
   int result;
   int lbin;
   int lbout;
   int ubin;
   int ubout;

/* Initialise the result. */
   result = 1;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Re-bin a single non-zero pixel value using the supplied spreading
   scheme, and then determining the width of the resulting non-zero pixel
   values. */
   umap = astUnitMap( 1, " " );
   lbin = 0;
   ubin = 0;
   val = 1.0;

   lbout = -1000;
   ubout = 1000;
   work = astMalloc( sizeof( float )*( ubout - lbout + 1 ) );
   if( work ) {
      astRebinF( umap, 0.0, 1, &lbin, &ubin, &val, NULL, spread, params, 0,
                 0.0, 0, VAL__BADR, 1, &lbout, &ubout, &lbin, &ubin, work,
                 NULL );

      w = work + 1001;
      while( *w != VAL__BADR && *w != 0.0 ) w++;
      result = (int)( ( w  - ( work + 1001 ) )*2 + 1 );

      umap = astAnnul( umap );
      work = astFree( work );
   }

/* Return the result. */
   return result;
}
