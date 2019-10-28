#include "f77.h"
#include "kaplibs.h"
#include "ast.h"
#include "sae_par.h"

F77_SUBROUTINE(kpg1_asndf8)( INTEGER(INDF), INTEGER(NDIM), INTEGER_ARRAY(DIM),
                             INTEGER8_ARRAY(LBND), INTEGER8_ARRAY(UBND),
                             INTEGER(IWCS), INTEGER(STATUS) ) {
/*
*+
*  Name:
*     KPG1_ASNDF8

*  Purpose:
*     Creates a FrameSet containing NDF-special Frames with given bounds.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_ASNDF8( INDF, NDIM, DIM, LBND, UBND, IWCS, STATUS )

*  Description:
*     This routine is equivalent to KPG1_ASNDF except that arguments
*     LBND and UBND are INTEGER*8 instead of INTEGER. See KPG1_ASNDF
*     for more information.

*  Arguments:
*     INDF = INTEGER (Given)
*        An NDF from which to propagate AXIS information. May be NDF__NOID,
*        in which case the AXIS Frame in the returned FrameSet will describe
*        the default AXIS coordinate system (i.e. pixel co-ordinates).
*     NDIM = INTEGER (Given)
*        The number of pixel axes in the modified FrameSet.
*     DIM( NDIM ) = INTEGER (Given)
*        The indices within INDF corresponding to each of the required
*        NDIM axes.
*     LBND( NDIM ) = INTEGER*8 (Given)
*        The lower pixel index bounds in the modified FrameSet.
*     UBND( NDIM ) = INTEGER*8 (Given)
*        The upper pixel index bounds in the modified FrameSet.
*     IWCS = INTEGER (Returned)
*        Pointer to a new FrameSet holding GRID, FRACTION, PIXEL and AXIS
*        Frames describing the supplied NDF bounds, plus AXIS information
*        from the supplied NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     useful, but WITHOUT ANY WARRANTY; without even the implied
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
*        Original version, copied from KPG1_ASNDF and changed to use
*        INTEGER*8 bounds and dimensions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(DIM)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(IWCS)
   GENPTR_INTEGER(STATUS)

   AstFrameSet *iwcs;

   kpg1Asndf8( *INDF, *NDIM, DIM, LBND, UBND, &iwcs, STATUS );
   F77_EXPORT_INTEGER( astP2I( iwcs ), *IWCS );

}
