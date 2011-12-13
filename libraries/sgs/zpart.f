      SUBROUTINE sgs_ZPART (NX,NY, IZONID, JSTAT)
*+
*  Name:
*     ZPART

*  Purpose:
*     Partition the current zone into NX by NY segments.  The current
*     zone is unchanged.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     NX = INTEGER (Given)
*         Number of zones in horizontal direction
*     NY = INTEGER (Given)
*         Number of zones in vertical direction
*     IZONID() = INTEGER (Returned)
*         Array of zone identifiers for new zones
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
*         Status (0=OK) (if non inherited)

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
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Errors:
*     Too many zones

*  Externals:
*     sgs_1HSTAT, sgs_ICURZ, sgs_IZONE, sgs_ZONE, sgs_SELZ, sgs_RELZ

*-

      IMPLICIT NONE

      INTEGER NX,NY,IZONID(NX*NY),JSTAT

      INTEGER I,J,II,IZ
      REAL X1,X2,Y1,Y2,XM,YM,XSTEP,YSTEP,X1S



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Get the limits and ID of the current zone
      CALL sgs_IZONE(X1,X2,Y1,Y2,XM,YM)
      CALL sgs_ICURZ(IZ)

*  Calculate step size and save start X
      XSTEP = (X2 - X1) / NX
      YSTEP = (Y2 - Y1) / NY
      X1S = X1

*  Create the zones
      DO 20 J = 1,NY
         X1 = X1S
         DO 10 I = 1,NX
            CALL sgs_ZONE(X1,MIN(X1+XSTEP,X2),Y1,MIN(Y1+YSTEP,Y2),
     :                                         IZONID(I+(J-1)*NX),JSTAT)
            IF (JSTAT.NE.0) GO TO 9000
            CALL sgs_SELZ(IZ,JSTAT)
            X1 = X1 + XSTEP
   10    CONTINUE
         Y1 = Y1 + YSTEP
   20 CONTINUE

*  Success
      GO TO 9999

 9000 CONTINUE

*  A zone create failed: delete all the ones created so far
      CALL sgs_SELZ(IZ,JSTAT)
      DO 30 II = 1,(J-1)*NX + I - 1
         CALL sgs_RELZ(IZONID(II))
   30 CONTINUE

 9999 CONTINUE

      END
