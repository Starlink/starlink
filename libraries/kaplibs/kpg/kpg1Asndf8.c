#include "sae_par.h"
#include "ast.h"
#include "kaplibs.h"

/* Use 8-byte NDF API */
#define NDF_I8 1
#include "ndf.h"

void kpg1Asndf8( int indf, int ndim, int *dim, hdsdim *lbnd, hdsdim *ubnd,
                 AstFrameSet **iwcs, int *status ){
/*
*  Name:
*     kpg1Asndf8

*  Purpose:
*     Create a FrameSet containing NDF-special Frames with given bounds.

*  Language:
*     C.

*  Invocation:
*     void kpg1Asndf8( int indf, int ndim, int *dim, hdsdim *lbnd,
*                      hdsdim *ubnd, AstFrameSet **iwcs, int *status )

*  Description:
*     This function is equivalent to kpg1Asndf except that arguments
*     "lbnd" and "ubnd" are of type "hdsdim *" instead of "int *". See
*     kpg1Asndf for more information.

*  Arguments:
*     indf
*        An NDF from which to propagate AXIS information. May be NDF__NOID,
*        in which case the AXIS Frame in the returned FrameSet will describe
*        the default AXIS coordinate system (i.e. pixel coords).
*     ndim
*        The number of pixel axes in the modified FrameSet.
*     dim
*        The indices within INDF corresponding to each of the required
*        ndim axes.
*     lbnd
*        The lower pixel index bounds in the modified FrameSet.
*     ubnd
*        The upper pixel index bounds in the modified FrameSet.
*     iwcs
*        Address at which to return a pointer to a new FrameSet holding
*        GRID, FRACTION, PIXEL and AXIS Frames describing the supplied
*        NDF bounds, plus AXIS information from the supplied NDF.
*     status
*        The inherited status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     4-OCT-2019 (DSB):
*        Original version, copied from kpg1Asndf and changed to use
*        hdsdim bounds and dimensions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

/* Local Variables: */
   int adim;          /* Number of dimensions of input NDF */
   hdsdim dims[ NDF__MXDIM ]; /* Dimension sizes of input NDF */
   int idim;          /* Axis index */
   int indf2;         /* Identifier for temporary NDF */
   int place;         /* Place holder for temporary NDF */

/* Initialise */
   *iwcs = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain the actual dimension of the input NDF. */
   ndfDim( indf, NDF__MXDIM, dims, &adim, status );
   adim = ( adim < ndim ) ? adim : ndim;

/* Create a place-holder for a temporary NDF. */
   ndfTemp( &place, status );

/* Create a new NDF with the required bounds. */
   ndfNew( "_REAL", ndim, lbnd, ubnd, &place, &indf2, status );

/* If an input NDF was supplied, copy the required AXIS structures to the
   new NDF. */
   if( indf != NDF__NOID ) {
      for( idim = 0; idim < adim; idim++ ) {
         kpg1Axcpy( indf, indf2, dim[ idim ], idim + 1, status );
      }
   }

/* Get the WCS FrameSet. */
   ndfGtwcs( indf2, iwcs, status );

/* Annul the temporary NDF. */
   ndfAnnul( &indf2, status );

}
