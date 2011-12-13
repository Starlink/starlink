      SUBROUTINE sgs_TPZ (IZIN, XIN,YIN, IZOUT, XOUT,YOUT, JSTAT)
*+
*  Name:
*     TPZ

*  Purpose:
*     Convert position in one zone to position in another.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     IZIN = INTEGER (Given)
*         Zone of input position
*     XIN = REAL (Given)
*         X position in zone IZIN
*     YIN = REAL (Given)
*         Y    "     "    "   "
*     IZOUT = INTEGER (Given)
*         Zone of output position
*     XOUT = REAL (Returned)
*         X position in zone IZOUT
*     YOUT = REAL (Returned)
*         Y    "     "    "   "
*     JSTAT = INTEGER (Returned)
*         Status (0=OK)

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

*  Constants From Sgscom:
*     MXZ         i      Maximum number of zones allowed

*  Errors:
*     Invalid zone ID

*  Externals:
*     sgs_1ERR, sgs_1HSTAT

*  Read From Common:
*     IZTW        i()    zone Table - workstation ID
*     ZTV         r()      "    "   - viewport
*     ZTW         r()      "    "   - window

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'


      INTEGER IZIN
      REAL XIN,YIN
      INTEGER IZOUT
      REAL XOUT,YOUT
      INTEGER JSTAT

      REAL XNDC,YNDC

      CHARACTER*3 RNAME
      PARAMETER (RNAME='TPZ')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Check that zones actually exist
      IF (IZIN.LE.0 .OR. IZIN.GT.MXZ .OR.
     :    IZOUT.LE.0 .OR. IZOUT.GT.MXZ) THEN
         CALL sgs_1ERR(SGS__INVZN,RNAME,'Invalid zone ID',JSTAT)
         GO TO 9999
      END IF

      IF (IZTW(IZIN).EQ.0 .OR. IZTW(IZOUT).EQ.0) THEN
         CALL sgs_1ERR(SGS__INVZN,RNAME,'Invalid zone ID',JSTAT)
         GO TO 9999
      END IF

*  Convert input position to NDC (note the order of evaluation to avoid
*  rounding problems)
      XNDC = ZTV(1,IZIN) + ((XIN - ZTW(1,IZIN)) /
     :                      (ZTW(2,IZIN) - ZTW(1,IZIN))) *
     :                      (ZTV(2,IZIN) - ZTV(1,IZIN))

      YNDC = ZTV(3,IZIN) + ((YIN - ZTW(3,IZIN)) /
     :                      (ZTW(4,IZIN) - ZTW(3,IZIN))) *
     :                      (ZTV(4,IZIN) - ZTV(3,IZIN))

*  Convert back to WC in new zone
      XOUT = ZTW(1,IZOUT) + ((XNDC - ZTV(1,IZOUT)) /
     :                       (ZTV(2,IZOUT) - ZTV(1,IZOUT))) *
     :                       (ZTW(2,IZOUT) - ZTW(1,IZOUT))

      YOUT = ZTW(3,IZOUT) + ((YNDC - ZTV(3,IZOUT)) /
     :                       (ZTV(4,IZOUT) - ZTV(3,IZOUT))) *
     :                       (ZTW(4,IZOUT) - ZTW(3,IZOUT))

      JSTAT = 0

 9999 CONTINUE

      END
