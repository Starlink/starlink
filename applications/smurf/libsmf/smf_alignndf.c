/*
*+
*  Name:
*     smf_alignndf

*  Purpose:
*     Align the data array in a supplied 2D NDF with the output 2D map.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     double *smf_alignndf( int indf, AstFrameSet *outfset, int *lbnd_out,
*                           int *ubnd_out, int *status )

*  Arguments:
*     indf = int (Given)
*        Identifier for the NDF to be aligned with the output map.
*     outfset = AstFrameSet * (Given)
*        The FrameSet describing the WCS associated with the output map.
*     lbnd_out = int * (Given)
*        A 2-element array - the lower pixel index bounds of the output map.
*     ubnd_out = int * (Given)
*        A 2-element array - the upper pixel index bounds of the output map.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to a newly allocated array of doubles holding a 2-D array
*     with the same shape and size as the output map. It will hold the
*     resampled values read from the data array of the supplied 2D NDF. It
*     should be freed using astFree when no longer needed. NULL is returned
*     if an error occurs.

*  Description:
*     This function resamples the supplied NDF to create an 2D array of
*     doubles that is aligned with the output map on the sky. A SincSinc
*     interpolation kernel is used.

*  Authors:
*     David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     8-OCT-2015 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
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
#include "star/atl.h"
#include "ndf.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

double *smf_alignndf( int indf, AstFrameSet *outfset, int *lbnd_out,
                      int *ubnd_out, int *status ){

/* Local Variables: */
   AstFrameSet *fs;
   AstFrameSet *iwcs;
   AstFrameSet *iwcsin;
   AstFrameSet *iwcsout;
   double *indata;
   double *result = NULL;
   double params[2];
   int el;
   int glbnd_in[ 3 ];
   int glbnd_out[ 2 ];
   int gubnd_in[ 3 ];
   int gubnd_out[ 2 ];
   int iaxis;
   int lbnd_in[ 3 ];
   int ndim_in;
   int ubnd_in[ 3 ];

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Begin AST and NDF context. */
   astBegin;
   ndfBegin();

/* Get the pixel index bounds of the NDF. This reports an error if the
   NDF has more than 3 pixel axes. */
   ndfBound( indf, 3, lbnd_in, ubnd_in, &ndim_in, status );

/* Get the corresponding bounds in grid indices. */
   for( iaxis = 0; iaxis < ndim_in; iaxis++ ) {
      glbnd_in[ iaxis ] = 1;
      gubnd_in[ iaxis ] = ubnd_in[ iaxis ] - lbnd_in[ iaxis ] + 1;
   }

/* If it is 3D, check that axis 3 spans only one pixel. */
   if( ndim_in == 3 && gubnd_in[ 2 ] > 1 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      errRep( " ", "smf_alignndf: NDF '^N' is 3-dimensional - only "
              "2-dimensional NDFs may be used.", status );
   }

/* Get the WCS FrameSet from the NDF. */
   kpg1Gtwcs( indf, &iwcs, status );

/* Extract a 2D FrameSet from the potentially 3D FrameSet (may be 3D if
   the NDF has a 3rd pixel axis that spans only one pixel). */
   iwcsin = atlFrameSetSplit( iwcs, "SKY", NULL, NULL, status );
   if( !iwcsin &&  *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      errRep( " ", "smf_alignndf: could not extract the sky axes from "
              "the WCS information in NDF '^N'.", status );
   }

/* Extract a 2D FrameSet from the potentially 3D supplied FrameSet. */
   iwcsout = atlFrameSetSplit( outfset, "SKY", NULL, NULL, status );

/* We want the Mapping from NDF grid coordinates to output grid
   coordinates. Since grid coords will be the base Frame in each
   FrameSet, we need to invert both FrameSets prior to calling astConvert,
   so that grid coords becomes the current Frame. This is because
   astConvert find the Mapping between current Frames. */
   astInvert( iwcsin );
   astInvert( iwcsout );

/* Get the Mapping from 2D grid coords in the supplied NDF to 2D grid coords
   in the output map. */
   fs = astConvert( iwcsin, iwcsout, " " );

/* Report an error if alignment was not possible. */
   if( !fs && *status == SAI__OK ) {
      *status = SAI__ERROR;
      ndfMsg( "N", indf );
      errRep( " ", "smf_alignndf: could not align NDF '^N' with the "
              "output map.", status );
   }

/* Get the grid index bounds of the output map. */
   for( iaxis = 0; iaxis < 2; iaxis++ ) {
      glbnd_out[ iaxis ] = 1;
      gubnd_out[ iaxis ] = ubnd_out[ iaxis ] - lbnd_out[ iaxis ] + 1;
   }

/* Create the returned array. */
   result = astMalloc( gubnd_out[ 0 ]*gubnd_out[ 1 ]*sizeof( *result ) );

/* Map the Data array of the supplied NDF. */
   ndfMap( indf, "DATA", "_DOUBLE", "READ", (void **) &indata,
           &el, status );

/* Resample the NDF onto the output grid. */
   params[ 0 ] = 2.0;
   params[ 1 ] = 2.0;
   astResampleD( fs, 2, glbnd_in, gubnd_in, indata, NULL, AST__SINCSINC,
                 NULL, params, AST__USEBAD, 0.1, 100, VAL__BADD, 2,
                 glbnd_out, gubnd_out, glbnd_out, gubnd_out, result, NULL );

/* Free any returned array if an error occurred. */
   if( *status != SAI__OK ) result = astFree( result );

/* End the AST and NDF context. */
   ndfEnd( status );
   astEnd;

   return result;
}


