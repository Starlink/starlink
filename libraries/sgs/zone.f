      SUBROUTINE sgs_ZONE (X1, X2, Y1, Y2, IZONID, JSTAT)
*+
*  Name:
*     ZONE

*  Purpose:
*     Create and select a zone of the specified extent.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The new zone has a window whose world coordinate extent is (0,0) to
*     (X,Y), where X/Y is the true aspect ratio of the viewport and the
*     smaller of X or Y is unity.

*  Arguments:
*     X1,X2,Y1,Y2 = REAL (Given)
*         Bounds of new zone in current zone's
*         world coordinates
*     IZONID = INTEGER (Returned)
*         Zone identifier for new zone
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
*         Status (0=OK) (if not inherited)

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
*     New zone not inside current
*     Too many zones

*  Externals:
*     sgs_1HSTAT, sgs_1BNORM, sgs_1ERR, sgs_1GETZ, sgs_1NEWZ

*  Read From Common:
*     ISZID         i      current zone ID
*     ZTW           i()    zone table - window
*     ZTV           i()     "     "   - viewport
*     IZTW          i()     "     "   - workstation ID

*-

      IMPLICIT NONE

      REAL X1,X2,Y1,Y2
      INTEGER IZONID,JSTAT

      INCLUDE 'SGS_ERR'

      INCLUDE 'sgscom'


      INTEGER IWKID
      REAL X1V,Y1V,X2V,Y2V,X1VN,Y1VN,X2VN,Y2VN
      REAL X1CUR,X2CUR,Y1CUR,Y2CUR,X1N,X2N,Y1N,Y2N

      CHARACTER RNAME*4
      PARAMETER (RNAME='ZONE')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 8888

*  Normalise bounds of new zone
      CALL sgs_1BNORM(X1,X2,Y1,Y2,X1N,X2N,Y1N,Y2N,JSTAT)
      IF (JSTAT.NE.0) GO TO 8888

*  Extent of current window
      X1CUR=ZTW(1,ISZID)
      X2CUR=ZTW(2,ISZID)
      Y1CUR=ZTW(3,ISZID)
      Y2CUR=ZTW(4,ISZID)

*  Check that new zone lies within current zone
      IF (X1N.LT.X1CUR .OR. X2N.GT.X2CUR .OR.
     :    Y1N.LT.Y1CUR .OR. Y2N.GT.Y2CUR) THEN
         CALL sgs_1ERR(SGS__ZNOUT,RNAME,'New zone not inside current',
     :                                                           JSTAT)
         GO TO 8888
      END IF

*  Allocate a zone table entry
      IWKID = ABS(IZTW(ISZID))
      CALL sgs_1GETZ(IWKID,IZONID)
      IF (IZONID.EQ.0) THEN
         CALL sgs_1ERR(SGS__ZNZEX,RNAME,'Too many zones',JSTAT)
         GO TO 8888
      END IF

*  Current viewport
      X1V=ZTV(1,ISZID)
      X2V=ZTV(2,ISZID)
      Y1V=ZTV(3,ISZID)
      Y2V=ZTV(4,ISZID)

*  New NDC limits
      X1VN = X1V + ((X1N-X1CUR) / (X2CUR-X1CUR)) * (X2V-X1V)
      X2VN = X1V + ((X2N-X1CUR) / (X2CUR-X1CUR)) * (X2V-X1V)
      Y1VN = Y1V + ((Y1N-Y1CUR) / (Y2CUR-Y1CUR)) * (Y2V-Y1V)
      Y2VN = Y1V + ((Y2N-Y1CUR) / (Y2CUR-Y1CUR)) * (Y2V-Y1V)

*  Set up zone with appropriate viewport
      CALL sgs_1NEWZ(IZONID,X1VN,X2VN,Y1VN,Y2VN)

*  Exit
      GO TO 9999
 8888 CONTINUE
      CALL sgs_1ERR(SGS__UCRNZ,RNAME,'Unable to create new zone',JSTAT)
 9999 CONTINUE

      END
