#include "star/hds.h"
#include "kaplibs.h"
#include "ast.h"
#include "sae_par.h"

void kpg1Ch2pm( HDSLoc *loc, AstPolyMap **polymap, int *status ){
/*
*  Name:
*     kpg1Ch2pm

*  Purpose:
*     Creates an AST PolyMap describing a Starlink POLYNOMIAL HDS structure.

*  Language:
*     C.

*  Invocation:
*     void kpg1Ch2pm( HDSLoc *loc, AstPolyMap **polymap, int *status )

*  Description:
*     This routine creates an AST PolyMap that implements the
*     polynomial transformation described by a supplied Starlink
*     POLYNOMIAL structure (see SGP/38).

*  Arguments:
*     loc
*        A locator for the HDS object into which the KeyMap contents
*        are to be copied.
*     polymap
*        The address at which to store the pointer to the returned PolyMap.
*        A NULL pointer will be stored at this address if an error occurs.
*     status
*        The inherited status.

*  Notes:
*     - The returned PolyMap has a defined forward transformation
*     (equivalent to the supplied POLYNOMIAL), but no inverse
*     transformation.
*     - Both CHEBYSHEV and SIMPLE variants of the POLYNOMIAL structure
*     are supported. But currently only 1- or 2- dimensional Chebyshev
*     polynomials can be handled. An error is reported for Chebyshev
*     polynomials of higher dimensionality.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-NOV-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/

/* Local Varianles: */
   double *coeffs_f;
   int ncoeff_f;
   int ndim;

/* Initialise */
   *polymap = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the coefficients from the supplied POLYNOMIAL structure. */
   coeffs_f = kpg1Chcof( 1, &loc, &ncoeff_f, &ndim, status );

/* Create the PolyMap. */
   *polymap = astPolyMap( ndim, 1, ncoeff_f, coeffs_f, 0, NULL, " " );

/* Free workspace. */
   coeffs_f = astFree( coeffs_f );

}
