#include "ast.h"
#include "atl.h"
#include "mers.h"
#include "sae_par.h"
#include <math.h>

void atlPairAxes( AstFrameSet *from, AstFrameSet *to, double *p,
                  const char *domainlist, int *axes, int *status ) {
/*
*+
*  Name:
*     atlPairAxes

*  Purpose:
*     Find corresponding axes in a pair of Frames or FrameSets.

*  Invocation:
*     void atlPairAxes( AstFrameSet *from, AstFrameSet *to, double *p,
*                       const char *domainlist, int *axes, int *status )

*  Description:
*     For each axis in the base Frame of "From", this function finds the
*     index of the most closely aligned axis in the base Frame of "to",
*     (or the current Frame of "from" if "to" is not supplied), and
*     returns these indices.

*  Arguments:
*     from
*        An AST pointer to the first FrameSet.
*     to
*        An AST pointer to the second FrameSet. An error is reported if
*        it is not possible to align the two FrameSets using astConvert.
*        If NULL, then the returned axis indices are the indices of the
*        corresponding current axes in "from".
*     p
*        The axis values of a point within the base Frame of "from" at
*        which the pairing is to be determined. None of the supplied axis
*        values should be zero.
*     domainlist
*        A list of domain names that define the prefered alignment
*        Frames. This list is used by astConvert to align the two
*        FrameSets. Only used if "to" is not NULL.
*     axes
*        The length of this array should be equal to the number of base
*        Frame axes in "from". Each returned value will be the one-based
*        index of the corresponding axis in the base Frame of "to" (or
*        the current Frame of "from" if "to" is not supplied), or zero
*        if no corresponding axis can be found.
*     status
*        The global status.

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     8-NOV-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}
*-
*/

/* Local Variables: */
   AstFrameSet *fs;
   AstMapping *map;
   double *inpos;
   double *outpos;
   double *p1;
   double delta;
   double delta_max;
   double v0;
   double v1;
   int *jaxes;
   int *old_status;
   int i;
   int ibase_from;
   int ibase_to;
   int imax;
   int j;
   int jmax;
   int nfrom;
   int nto;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Make AST use the supplied status variable. */
   old_status = astWatch( status );

/* If a "to" FrameSet has been supplied, get the mapping from base Frame
   in "from" to base Frame in "to". */
   if( to ) {

/* Temporaily invert the two FrameSets so that astConvert will give us a
   Mapping between the two original base Frames. */
      astInvert( to );
      astInvert( from );

/* Note the indices of the new base Frames since astConvert will change
   them. */
      ibase_from = astGetI( from, "Base" );
      ibase_to = astGetI( to, "Base" );

/* Get a FrameSet that goes from the original base Frame in "from" (now
   the current Frame), to the original base Frame in "to" (now the current
   Frame). */
      fs = astConvert( from, to, domainlist );

/* Reset the original base Frames. */
      astSetI( from, "Base", ibase_from );
      astSetI( to, "Base", ibase_to );

/* Re-invert the two FrameSets to get them back to their original states. */
      astInvert( to );
      astInvert( from );

/* Report an error if the conversion is not possible. */
      if( !fs ){
         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( "", "atlPairAxes: the two data sets cannot be aligned "
                    "since they have no common WCS.", status );
         }

/* Otherwise, get the Mapping from the base Frame of "to" to the base
   Frame of "from". */
      } else {
         map = astGetMapping( fs, AST__BASE, AST__CURRENT );
         fs = astAnnul( fs );
      }

/* If no "to" FrameSet was supplied, use the Mapping from the base Frame
   of "from" to the current Frame of "from". */
   } else {
      map = astGetMapping( from, AST__BASE, AST__CURRENT );
   }


/* Get the number base Frame axes in "from", and the number of axes in
   the destination Frame (base Frame of "to" or current Frame of "from"). */
   nfrom = astGetI( map, "Nin" );
   nto = astGetI( map, "Nout" );

/* Initalise the returned indices. */
   for( i = 0; i < nfrom; i++ ) axes[ i ] = 0;

/* Allocate memory to hold a set of position in the base Frame of "from"
   - one for each base Frame axis and one more. */
   inpos = astMalloc( ( nfrom + 1 )*nfrom*sizeof( *inpos ) );

/* Allocate memory to hold the corresponding positions in the destination
   Frame. */
   outpos = astMalloc( ( nfrom + 1 )*nto*sizeof( *outpos ) );

/* Allocate an array to hold, for each destination Frame axis, the
   one-based index of the corresponding "from" axis, initialising it to
   zero. */
   jaxes = astCalloc( nto, sizeof( *jaxes ) );

/* Check we can use the pointers safely. */
   if( *status == SAI__OK ) {

/* Set up the input positions. The first is at the supplied position in
   the base Frame of "from". The rest are all displaced from this
   position by a small angle along a single axis. */
      p1 = inpos;
      for( i = 0; i < nfrom; i++ ) {

         if( p[ i ] == AST__BAD || p[ i ] == 0.0 ) {
            *status = SAI__ERROR;
            errRepf( "", "atlPairAxes: bad value (%g) supplied for axis "
                     "%d of the reference position.", status, p[ i ], i + 1 );
            break;
         }

         for( j = -1; j < nfrom; j++ ) {
            *(p1++) = ( j == i ) ?  p[ i ]*1.001 : p[ i ];
         }

      }

/* Transform them into the destination Frame. */
      astTranN( map, nfrom + 1, nfrom, nfrom + 1, inpos, 1, nto, nfrom + 1,
                outpos );

/* We find the the pair of axes, selected form all remaining unpaired axes,
   that are most closely aligned with each other. We do this until we can
   find no more pairs. */
      while( *status == SAI__OK ) {

/* Initialise the largest projection found so far. */
         delta_max = -1.0;

/* Loop round each axis in the base frame of "from". */
         for( i = 0; i < nfrom; i++ ) {

/* Skip if this "from" axis already has a pair. */
            if( axes[ i ] > 0 ) continue;

/* Loop round each axis in the destination frame. */
            for( j = 0; j < nto; j++ ) {

/* Skip if this destination axis already has a pair. */
               if( jaxes[ j ] > 0 ) continue;

/* Get the projection on the current "from" base Frame axis, of the
   displacement from the transformed reference point to the transformed
   displaced point. */
               v0 = outpos[ ( nfrom + 1 )*j ];
               v1 = outpos[ ( nfrom + 1 )*j + 1 + i ];
               if( v0 == AST__BAD || v1 == AST__BAD ) {
                  if( *status == SAI__OK ) {
                     *status = SAI__ERROR;
                     errRepf( "", "atlPairAxes: supplied reference position is "
                              "bad in the destination Frame.", status );
                  }
                  break;
               }
               delta = fabs( v1 - v0 );

/* If this destination axis has the largest delta found so far, record
   it. */
               if( delta > delta_max ) {
                  delta_max = delta;
                  imax = i;
                  jmax = j;
               }
            }
         }

/* If a pair was found, record it. */
         if( delta_max > 0.0 ) {
            axes[ imax ] = jmax + 1;
            jaxes[ jmax ] = imax + 1;

/* Otherwise leave the loop. */
         } else {
           break;
         }
      }
   }

/* Free resources. */
   jaxes = astFree( jaxes );
   inpos = astFree( inpos );
   outpos = astFree( outpos );
   map = astAnnul( map );

/* Make AST use its original status variable. */
   astWatch( old_status );
}

