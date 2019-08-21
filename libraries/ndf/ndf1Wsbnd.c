#include "sae_par.h"
#include "dat_par.h"
#include "ndf_ast.h"
#include "ndf1.h"

void ndf1Wsbnd( int ndim, const hdsdim lbnd[], const hdsdim ubnd[],
                NdfACB *acb, AstFrameSet **iwcs, int *status ){
/*
*+
*  Name:
*     ndf1Wsbnd

*  Purpose:
*     Obtain the effect of new pixel-index bounds on WCS information.

*  Synopsis:
*     void ndf1Wsbnd( int ndim, const hdsdim lbnd[], const hdsdim ubnd[],
*                     NdfACB *acb, AstFrameSet **iwcs, int *status )

*  Description:
*     This function obtains the new WCS information that should apply to an
*     NDF if its pixel-index bounds are changed in a specified way
*     (including possible changes to the number of dimensions).

*  Parameters:
*     ndim
*        New number of NDF dimensions.
*     lbnd
*        New lower pixel-index bounds. The supplied "lbnd" array should
*        have at least "ndim" elements.
*     ubnd
*        New upper pixel-index bounds. The supplied "ubnd" array should
*        have at least "ndim" elements.
*     acb
*        Pointer to the NDF entry in the ACB.
*     *iwcs
*        Returned holding the AST_ pointer to the new WCS information.
*     *status
*        The global status.

*  Notes:
*     - The NDF's WCS component need not previously exist.
*     - This function should be invoked prior to actually changing the
*     bounds of the NDF's axis component or main data array, upon whose
*     original shape it depends.
*     - If this function is called with "status" set, then a value of
*     AST__NULL will be returned for the "iwcs" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acbt;         /* Pointer to temporary ACB entry */

/* Initialise the returned AST_ pointer. */
   *iwcs = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Create a temporary section from the input NDF, with its shape
   determined by the new NDF bounds. This results in a new ACB entry. */
   ndf1Cut( acb, ndim, lbnd, ubnd, &acbt, status );

/* Read the NDF's WCS information via this section, which causes the
   effects of the new bounds to be imposed on it. */
   ndf1Rdwcs( acbt, iwcs, status );

/* Annul the temporary section's entry in the ACB. */
   ndf1Anl( &acbt, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Wsbnd", status );

}

