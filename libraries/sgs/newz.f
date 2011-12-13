      SUBROUTINE sgs_1NEWZ (IZONID, X1,X2, Y1,Y2)
*+
*  Name:
*     NEWZ

*  Purpose:
*     Finish setting up a new zone.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Description:
*     The viewport and a default window are stored in the zone table entry
*     for the given zone ID, and the new zone is selected.

*  Arguments:
*     IZONID = INTEGER (Given)
*         Zone identifier for new zone
*     X1,X2,Y1,Y2 = REAL (Given)
*         Zone viewport bounds (NDC)

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

*  Externals:
*     sgs_SELZ, sgs_1NORM

*  Written To Common:
*     ZTV           r()   zone table - viewport
*     ZTW           r()   zone table - window

*-

      IMPLICIT NONE

      INTEGER IZONID
      REAL X1,X2,Y1,Y2

      INCLUDE 'sgscom'


      INTEGER J
      REAL XN,YN



*  Viewport
      ZTV(1,IZONID)=X1
      ZTV(2,IZONID)=X2
      ZTV(3,IZONID)=Y1
      ZTV(4,IZONID)=Y2

*  Default window
      CALL sgs_1NORM(X2-X1,Y2-Y1,XN,YN)
      ZTW(1,IZONID)=0.0
      ZTW(2,IZONID)=1.0/YN
      ZTW(3,IZONID)=0.0
      ZTW(4,IZONID)=1.0/XN

*  Select the zone
      J=0
      CALL sgs_SELZ(IZONID,J)

      END
