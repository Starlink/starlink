      SUBROUTINE sgs_1BNORM (X1,X2, Y1,Y2, X1N,X2N, Y1N,Y2N, JSTAT)
*+
*  Name:
*     BNORM

*  Purpose:
*     Order bounds for normal orientation and check for non-zero area.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     internal routine

*  Arguments:
*     X1 = REAL (Given)
*         Lower bound in x
*     Y1 = REAL (Given)
*         "     "    " y
*     X2 = REAL (Given)
*         Upper   "    " x
*     Y2 = REAL (Given)
*         "     "    " y
*     X1N = REAL (Returned)
*         Normalized lower bound in x
*     Y1N = REAL (Returned)
*         "       "     "    " y
*     X2N = REAL (Returned)
*         "     Upper   "    " x
*     Y2N = REAL (Returned)
*         "       "     "    " y
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
*         Status (0=OK) (if non-inherited)

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Errors:
*     Zero extent

*  Externals:
*     sgs_1HSTAT, sgs_1ERR

*-

      IMPLICIT NONE

      INCLUDE 'SGS_ERR'


      REAL X1,X2,Y1,Y2,X1N,X2N,Y1N,Y2N
      INTEGER JSTAT

      CHARACTER RNAME*5
      PARAMETER (RNAME='BNORM')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Order X bounds
      IF (X1.LT.X2) THEN
         X1N=X1
         X2N=X2
      ELSE
         X1N=X2
         X2N=X1
      END IF

*  Order Y bounds
      IF (Y1.LT.Y2) THEN
         Y1N=Y1
         Y2N=Y2
      ELSE
         Y1N=Y2
         Y2N=Y1
      END IF

*  Check for zero extent
      IF (X2N-X1N.LE.0.0 .OR. Y2N-Y1N.LE.0.0)
     :              CALL sgs_1ERR(SGS__ZEREX,RNAME,'Zero extent',JSTAT)

*  Exit
 9999 CONTINUE

      END
