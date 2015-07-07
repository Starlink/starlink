/*
*+
*  Name:
*     smf_projbox

*  Purpose:
*     Find the minimal pixel bounding box of a set of NDFs within another
*     projection.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_projbox( Grp *igrp, AstFrameSet *refwcs,
*                       int lbnd[ NDF__MXDIM ], int ubnd[ NDF__MXDIM ],
*                       int dims[ NDF__MXDIM ], int *status )

*  Arguments:
*     igrp = Grp * (Given)
*        A group of NDF paths.
*     refwcs = AstFrameSet * (Given)
*        The WCS FrameSet for the required output projection. The current
*        Frame must have Domain "SKY", and the base Frame must have
*        Domain "PIXEL" abd be 2-dimensional.
*     lbnd = int * (Returned)
*        The lower pixel index bounds within the output projection that
*        just encloses all the input NDFs. May be NULL.
*     ubnd = int * (Returned)
*        The upper pixel index bounds within the output projection that
*        just encloses all the input NDFs. May be NULL.
*     dims = int * (Returned)
*        The dimensions of the boundiong box within the output projection
*        that just encloses all the input NDFs. May be NULL.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function finds the minimal pixel bounding box within the
*     output projection that just encloses the supplied NDFs.

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-SEP-2014 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013-2014 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "star/grp.h"
#include "star/atl.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Main entry */
void smf_projbox( Grp *igrp, AstFrameSet *refwcs, int lbnd[ NDF__MXDIM ],
                  int ubnd[ NDF__MXDIM ], int dims[ NDF__MXDIM ],
                  int *status ){

/* Local Variables: */
   AstFrameSet *fs;
   AstFrameSet *iwcs;
   const char *dom;
   double dlbnd_in[ 2 ];
   double dlbnd_out[ 2 ];
   double dubnd_in[ 2 ];
   double dubnd_out[ 2 ];
   int *pixaxes;
   int idim;
   int ilbnd[ NDF__MXDIM ];
   int index;
   int indf;
   int indim;
   int iubnd[ NDF__MXDIM ];
   int lbnd_box[ NDF__MXDIM ];
   int ndim = 0;
   int nndf;
   int spax1 = 0;
   int spax2 = 0;
   int ubnd_box[ NDF__MXDIM ];

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Check the current Frame of the reference FrameSet is SKY. */
   dom = astGetC( refwcs, "Domain" );
   if( *status == SAI__OK && strcmp( dom, "SKY" ) ) {
      *status = SAI__ERROR;
      errRepf( "", "smf_projbox: Current Frame of supplied reference "
               "FrameSet has domain '%s' - should be 'SKY'.", status, dom );
   }

/* Invert the supplied reference FrameSet so that the current Frame
   becomes PIXEL and the base Frame becomes SKY. */
   astInvert( refwcs );

/* Check the original base Frame (now the current Frame) of the reference
   FrameSet is PIXEL, and is 2D. */
   dom = astGetC( refwcs, "Domain" );
   if( *status == SAI__OK && strcmp( dom, "PIXEL" ) ) {
      *status = SAI__ERROR;
      errRepf( "", "smf_projbox: Base Frame of supplied reference "
               "FrameSet has domain '%s' - should be 'PIXEL'.", status, dom );
   }

   ndim = astGetI( refwcs, "Naxes" );
   if( *status == SAI__OK && ndim != 2 ) {
      *status = SAI__ERROR;
      errRepf( "", "smf_projbox: Base Frame of supplied reference "
               "FrameSet has %d axes - should be 2.", status, ndim );
   }

/* See how many NDFs there are. */
   nndf = grpGrpsz( igrp, status );

/* Loop round each NDF. */
   for( index = 0; index < nndf && *status == SAI__OK; index++ ) {

/* Start an AST context. */
      astBegin;

/* Get an NDF identifier for the current NDF. */
      ndgNdfas( igrp, index + 1, "Read", &indf, status );

/* Get the WCS FrameSet from the NDF. */
      kpg1Gtwcs( indf, &iwcs, status );

/* Make PIXEL coords the base Frame. The NDF library ensures that the
   PIXEL Frame is always Frame 2. */
      astSetI( iwcs, "Base", 2 );

/* Create a new FrameSet containing just the spatial axes from the NDFs
   WCS FrameSet. Note, the indices of the spatial pixel axes. */
      fs = atlFrameSetSplit( iwcs, "SKY", &pixaxes, NULL, status );

/* Check a pair of celestial axes was found. */
      if( fs ) {

/* Attempt to align the spatial NDF FrameSet with the supplied reference
   spatial FrameSet. We need to invert it first so that SKY coords
   becomes the base Frame, and PIXEL coords become the current Frame. */
         astInvert( fs );
         fs = astConvert( fs, refwcs, "SKY" );

/* Check the alignment was possible. */
         if( fs ) {

/* Replace the FrameSet pointer by a simplified Mapping pointer. */
            fs = astSimplify( astGetMapping( fs, AST__BASE, AST__CURRENT ) );

/* Get the pixel index bounds of the NDF. */
            ndfBound( indf, NDF__MXDIM, ilbnd, iubnd, &indim, status );

/* Check the current NDF has the same number of pixel axes as all previous
   NDFs. */
            if( index == 0 ) {
               ndim = indim;

            } else if( *status == SAI__OK ) {
               if( ndim != indim ) {
                  *status = SAI__ERROR;
                  ndfMsg( "N", indf );
                  errRep( "", "smf_projbox: NDF '^N' has a different "
                          "number of axes to the previous NDFs.", status );
               }
            }

/* Check the spatial axes in the current NDF have the same indices as all
   previous NDFs. */
            if( index == 0 ) {
               spax1 = pixaxes[ 0 ];
               spax2 = pixaxes[ 1 ];

            } else if( *status == SAI__OK ) {
               if( spax1 != pixaxes[ 0 ] || spax2 != pixaxes[ 1 ] ) {
                  *status = SAI__ERROR;
                  ndfMsg( "N", indf );
                  errRep( "", "smf_projbox: NDF '^N' has a different axis "
                          "order to the previous NDFs.", status );
               }
            }

/* Modify the NDF pixel bounds on the spatial axes so that they refer to
   the output projection. Bounds on other axes are left unchanged. */
            dlbnd_in[ 0 ] = ilbnd[ spax1 - 1 ] - 0.5;
            dubnd_in[ 0 ] = iubnd[ spax1 - 1 ] - 0.5;
            dlbnd_in[ 1 ] = ilbnd[ spax2 - 1 ] - 0.5;
            dubnd_in[ 1 ] = iubnd[ spax2 - 1 ] - 0.5;

            astMapBox( fs, dlbnd_in, dubnd_in, 1, spax1, dlbnd_out,
                       dubnd_out, NULL, NULL );
            astMapBox( fs, dlbnd_in, dubnd_in, 1, spax2, dlbnd_out + 1,
                       dubnd_out + 1, NULL, NULL );

            ilbnd[ spax1 - 1 ] = ceil( dlbnd_out[ 0 ] );
            iubnd[ spax1 - 1 ] = ceil( dubnd_out[ 0 ] );
            ilbnd[ spax2 - 1 ] = ceil( dlbnd_out[ 1 ] );
            iubnd[ spax2 - 1 ] = ceil( dubnd_out[ 1 ] );

/* Update the bounds of the total output bounding box. */
            if( index == 0 ) {
               for( idim = 0; idim < ndim; idim++ ) {
                  lbnd_box[ idim ] = ilbnd[ idim ];
                  ubnd_box[ idim ] = iubnd[ idim ];
               }
            } else {
               for( idim = 0; idim < ndim; idim++ ) {
                  if( lbnd_box[ idim ] > ilbnd[ idim ] ) lbnd_box[ idim ] = ilbnd[ idim ];
                  if( ubnd_box[ idim ] < iubnd[ idim ] ) ubnd_box[ idim ] = iubnd[ idim ];
               }
            }

/* Report an error if the NDF could not be aligned with the reference
   WCS. */
         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "N", indf );
            errRep( "", "smf_projbox: Cannot use the celestial axes "
                    "within the current Frame of NDF '^N'.", status );
         }

/* Report an error if a pair of celestial axes was not found in the NDF. */
      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "N", indf );
         errRep( "", "smf_projbox: Cannot find any usable celestial "
                 "axes within the current Frame of NDF '^N'.", status );
      }

/* Free resources. */
      pixaxes = astFree( pixaxes );

/* Annul the NDF identifier. */
      ndfAnnul( &indf, status );

/* End the AST context, thus annulling all AST objects created within the
   context. */
      astBegin;
   }

/* Invert the supplied reference FrameSet again so that it is left
   unchanged. */
   astInvert( refwcs );

/* Copy the required info to the returned arrays. */
   if( *status == SAI__OK ) {
      if( lbnd ) memcpy( lbnd, lbnd_box, ndim*sizeof( *lbnd ) );
      if( ubnd ) memcpy( ubnd, ubnd_box, ndim*sizeof( *ubnd ) );
      if( dims ) {
         for( idim = 0; idim < ndim; idim++ ) {
            dims[ idim ] = ubnd_box[ idim ] - lbnd_box[ idim ] + 1;
         }
      }
   }
}

