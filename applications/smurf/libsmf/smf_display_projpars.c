/*
*+
*  Name:
*     smf_display_projpars

*  Purpose:
*     Display the projection parameters using msgOut.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_display_projpars( AstSkyFrame * skyframe, double par[ 7 ],
*                           int * status);

*  Arguments:

*  Description:

*  Authors:
*     David S Berry (JAC, UCLan)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-DEC-2006 (DSB):
*        Originally added as part of smf_cubegrid
*     3-JUN-2008 (TIMJ):
*        Factored out into separate function.
*     9-JUL-2009 (TIMJ):
*        Increase reported accuracy of base position.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2008-2009 Science & Technology Facilities Council.
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

#include "sae_par.h"
#include "ast.h"
#include "mers.h"

#include "smf.h"

void
smf_display_projpars( AstSkyFrame * skyframe, double par[ 7 ],
                      int * status) {
  int curdigits;           /* current value of digits() for frameset */
  const int ndigits = 9;   /* Number of digits to use to format position */

  if (*status != SAI__OK) return;

  msgBlank( status );
  msgOutif( MSG__NORM, " ", "   Projection parameters used:", status );
  msgSetd( "V", par[ 0 ] );
  msgOutif( MSG__NORM, " ", "      CRPIX1 = ^V", status );
  msgSetd( "V", par[ 1 ] );
  msgOutif( MSG__NORM, " ", "      CRPIX2 = ^V", status );
  msgSetd( "V", par[ 2 ]*AST__DR2D );
  curdigits = astGetI( skyframe, "digits(1)" );
  astSetI( skyframe, "digits(1)", ndigits );
  msgSetc( "V2", astFormat( skyframe, 1, par[ 2 ] ) );
  astSetI( skyframe, "digits(1)", curdigits );
  msgSetc( "S", astGetC( skyframe, "Symbol(1)" ) );
  msgOutif( MSG__NORM, " ", "      CRVAL1 = ^V ( ^S = ^V2 )", status );
  msgSetd( "V", par[ 3 ]*AST__DR2D );
  curdigits = astGetI( skyframe, "digits(2)" );
  astSetI( skyframe, "digits(2)", ndigits );
  msgSetc( "V2", astFormat( skyframe, 2, par[ 3 ] ) );
  astSetI( skyframe, "digits(2)", curdigits );
  msgSetc( "S", astGetC( skyframe, "Symbol(2)" ) );
  msgOutif( MSG__NORM, " ", "      CRVAL2 = ^V ( ^S = ^V2 )", status );
  msgSetd( "V", par[ 4 ]*AST__DR2D );
  msgSetd( "V2", 0.1*NINT(par[ 4 ]*AST__DR2D*36000.0) );
  msgOutif( MSG__NORM, " ", "      CDELT1 = ^V ( ^V2 arcsec )", status );
  msgSetd( "V", par[ 5 ]*AST__DR2D );
  msgSetd( "V2", 0.1*NINT(par[ 5 ]*AST__DR2D*36000.0) );
  msgOutif( MSG__NORM, " ", "      CDELT2 = ^V ( ^V2 arcsec )", status );
  msgSetd( "V", par[ 6 ]*AST__DR2D );
  msgOutif( MSG__NORM, " ", "      CROTA2 = ^V", status );

}
