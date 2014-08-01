/*
*+
*  Name:
*     palPrec

*  Purpose:
*     Form the matrix of precession between two epochs (IAU 2006)

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palPrec( double ep0, double ep1, double rmatp[3][3] )

*  Arguments:
*     ep0 = double (Given)
*        Beginning epoch
*     ep1 = double (Given)
*        Ending epoch
*     rmatp = double[3][3] (Returned)
*        Precession matrix

*  Description:
*     The IAU 2006 precession matrix from ep0 to ep1 is found and
*     returned. The matrix is in the sense  V(EP1)  =  RMATP * V(EP0).
*     The epochs are TDB (loosely TT) Julian epochs.
*
*     Though the matrix method itself is rigorous, the precession
*     angles are expressed through canonical polynomials which are
*     valid only for a limited time span of a few hundred years around
*     the current epoch.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-10 (DSB):
*        Initial version with documentation taken from Fortran SLA
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 1996 Rutherford Appleton Laboratory
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "pal1sofa.h"

void palPrec( double ep0, double ep1, double rmatp[3][3] ){

/* Local Variables: */
   double rmatq[3][3];
   double ep0_days;
   double ep1_days;

/* Convert supplied dates to days since J2000 */
   ep0_days = ( ep0 - 2000.0 )*ERFA_DJY;
   ep1_days = ( ep1 - 2000.0 )*ERFA_DJY;

/* If beginning epoch is J2000, just return the rotation matrix from
   J2000 to EP1. */
   if( ep0 == 2000.0 ) {
      eraPmat06( ERFA_DJ00, ep1_days, rmatp );

/* If end epoch is J2000, get the rotation matrix from J2000 to EP0 and
   then transpose it to get the rotation matrix from EP0 to J2000. */
   } else if( ep1 == 2000.0 ) {
      eraPmat06( ERFA_DJ00, ep0_days, rmatp );
      eraTr( rmatp, rmatp );

/* Otherwise. get the two matrices used above and multiply them
   together. */
   } else {
      eraPmat06( ERFA_DJ00, ep0_days, rmatp );
      eraTr( rmatp, rmatp );
      eraPmat06( ERFA_DJ00, ep1_days, rmatq );
      eraRxr( rmatp, rmatq, rmatp );
   }

}
