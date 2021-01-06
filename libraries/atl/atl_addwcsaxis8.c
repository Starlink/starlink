#include "atl.h"
#include "f77.h"
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

F77_SUBROUTINE(atl_addwcsaxis8)( INTEGER(WCS), INTEGER(MAP), INTEGER(FRM),
                                 INTEGER8_ARRAY(LBND), INTEGER8_ARRAY(UBND),
                                 INTEGER(STATUS) ) {
/*
*+
*  Name:
*     ATL_ADDWCSAXIS8

*  Purpose:
*     Add one or more axes to an NDFs WCS FrameSet.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL ATL_ADDWCSAXIS8( WCS, MAP, FRM, LBND, UBND, STATUS )

*  Description:
*     This routine adds one or more new axes to all the Frames in an NDF
*     WCS FrameSet. Frames that are known to be NDF-special (e.g. GRID,
*     AXIS, PIXEL and FRACTION) are expanded to include a number of extra
*     appropriate axes equal to the Nin attribute of the supplied Mapping.
*     all other Frames in the FrameSet are replaced by CmpFrames holding the
*     original Frame and the supplied Frame. These new axes are connected to
*     the new GRID axes using the supplied Mapping.

*  Arguments:
*     WCS = INTEGER (Given)
*        A pointer to a FrameSet that is to be used as the WCS FrameSet in
*        an NDF. This imposes the restriction that the base Frame must
*        have Domain GRID.
*     MAP = INTEGER (Given)
*        A pointer to a Mapping. The forward transformation should transform
*        the new GRID axes into the new WCS axes.
*     FRM = INTEGER (Given)
*        A pointer to a Frame defining the new WCS axes.
*     LBND() = INTEGER*8 (Given)
*        An array holding the lower pixel index bounds on the new axes.
*        The length of this array should beq aual to the Nin attribute of
*        the MAP Mapping.
*     UBND() = INTEGER*8 (Given)
*        An array holding the upper pixel index bounds on the new axes.
*        The length of this array should beq aual to the Nin attribute of
*        the MAP Mapping.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine is just a wrapper around the C function atlAddWcsAxis8.
*     - The new axes are appended to the end of the existing axes, so the
*     axis indices associated with the new axes will extend from "nold+1"
*     to "nold+nnew", where "nold" is the number of axes in the original
*     Frame, and "nnew" is the number of new axes.
*     - An error will be reported if the Nout attribute of "map" is
*     different to the Naxes attribute of "frm".

*  Copyright:
*     Copyright (C) 2021 East Asian Observatory
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
*     DSB: David S. Berry  (EAO)
*     {enter_new_authors_here}

*  History:
*     6-JAN-2021 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   GENPTR_INTEGER(WCS)
   GENPTR_INTEGER(MAP)
   GENPTR_INTEGER(FRM)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(STATUS)

   atlAddWcsAxis8( astI2P( *WCS ), astI2P( *MAP ), astI2P( *FRM ),
                   LBND, UBND, STATUS );

}

