      SUBROUTINE sgs_ZSIZE (XM,YM, POS, IZONID, JSTAT)
*+
*  Name:
*     ZSIZE

*  Purpose:
*     Create and select a zone of the specified size.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The new zone has a window whose world coordinate extent is (0,0) to
*     (X,Y), where X/Y is the true aspect ratio of the viewport and the
*     smaller of X or Y is unity.

*  Arguments:
*     XM,YM = REAL (Given)
*         Zone size in X and Y (metres)
*     POS = CHAR (Given)
*         Position code (see below)
*     IZONID = INTEGER (Returned)
*         Zone identifier for new zone
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
*         Status (0=OK) (if non-inherited)

*  Notes:
*     Position code;  the first two characters determine the position of
*     the new zone within the current zone as follows:
*
*     1st character = L (left),   C (centre) or R (right).
*     2nd character = B (bottom), C (centre) or T (top).
*
*     The new zone can be positioned in one corner of the current zone
*     by specifiying 'BL', 'BR', 'TL' or 'TR'.  The new zone can be
*     positioned centrally against one edge via 'BC', 'CR', 'TC' or 'CL'.
*     'CC' causes the new zone to be concentric with the current one.

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
*     Requested zone bigger than current

*  Externals:
*     sgs_IZONE, sgs_1ERR, sgs_1HSTAT, sgs_1PNZ

*  Read From Common:
*     ISZID        i      current zone ID
*     ZTV          i()    zone table - viewport

*-

      IMPLICIT NONE

      REAL XM,YM
      CHARACTER POS*(*)
      INTEGER IZONID,JSTAT

      INCLUDE 'SGS_ERR'


      REAL X1V,Y1V,X2V,Y2V,XV,YV,XVN,YVN
      REAL X1,X2,Y1,Y2,XCM,YCM

      INCLUDE 'sgscom'


      CHARACTER RNAME*5
      PARAMETER (RNAME='ZSIZE')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Find out how big the current zone is
      CALL sgs_IZONE(X1,X2,Y1,Y2,XCM,YCM)

*  Error if not big enough for requested size
      IF (XCM.LT.XM .OR. YCM.LT.YM) THEN
         CALL sgs_1ERR(SGS__ZONTB,RNAME,
     :       'Requested zone bigger than current',JSTAT)
         GO TO 9999
      END IF

*  Current viewport
      X1V=ZTV(1,ISZID)
      X2V=ZTV(2,ISZID)
      Y1V=ZTV(3,ISZID)
      Y2V=ZTV(4,ISZID)
      XV=X2V-X1V
      YV=Y2V-Y1V

*  NDC size of new viewport
      XVN=XV*XM/XCM
      YVN=YV*YM/YCM

*  Set up new zone
      CALL sgs_1PNZ(X1V,Y1V,XV,YV,XVN,YVN,POS,IZONID,JSTAT)

*  Exit
 9999 CONTINUE

      END
