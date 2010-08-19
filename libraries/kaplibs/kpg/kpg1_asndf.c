#include "f77.h"
#include "kaplibs.h"
#include "ast.h"
#include "sae_par.h"

F77_SUBROUTINE(kpg1_asndf)( INTEGER(INDF), INTEGER(NDIM), INTEGER_ARRAY(DIM),
                            INTEGER_ARRAY(LBND), INTEGER_ARRAY(UBND),
                            INTEGER(IWCS), INTEGER(STATUS) ) {
/*
*+
*  Name:
*     KPG1_ASNDF

*  Purpose:
*     Create a FrameSet containing NDF-special Frames with given bounds.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_ASNDF( INDF, NDIM, DIM, LBND, UBND, IWCS, STATUS )

*  Description:
*     This function creates a FrameSet containing the NDF-special Frames,
*     GRID, PIXEL, FRACTION and AXIS, appropriate to an NDF with the
*     supplied dimensionality and pixel index bounds. Optionally, AXIS
*     information can be propagated from a supplied NDF.

*  Arguments:
*     INDF = INTEGER (Given)
*        An NDF from which to propagate AXIS information. May be NDF__NOID,
*        in which case the AXIS Frame in the returned FrameSet will describe
*        the default AXIS coordinate system (i.e. pixel coords).
*     NDIM = INTEGER (Given)
*        The number of pixel axes in the modified FrameSet.
*     DIM( NDIM ) = INTEGER (Given)
*        The indices within INDF corresponding to each of the required
*        NDIM axes.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds in the modified FrameSet.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds in the modified FrameSet.
*     IWCS = INTEGER (Returned)
*        Pointer to a new FrameSet holding GRID, FRACTION, PIXEL and AXIS
*        Frames describing the supplied NDF bounds, plus AXIS information
*        from the supplied NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     22-FEB-2010 (DSB):
*        Original version.
*     19-AUG-2010 (DSB):
*        Added DIM argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(DIM)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(IWCS)
   GENPTR_INTEGER(STATUS)

   AstFrameSet *iwcs;

   kpg1Asndf( *INDF, *NDIM, DIM, LBND, UBND, &iwcs, STATUS );
   F77_EXPORT_INTEGER( astP2I( iwcs ), *IWCS );

}
