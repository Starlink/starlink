      SUBROUTINE sgs_IZONE (X1,X2, Y1,Y2, XM,YM)
*+
*  Name:
*     IZONE

*  Purpose:
*     Inquire world coordinate bounds and device coordinate size of the
*     current zone.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     X1 = REAL (Returned)
*         X coordinate of lower left corner
*     Y1 = REAL (Returned)
*         Y      "     "    "    "     "
*     X2 = REAL (Returned)
*         X      "     "  upper right  "
*     Y2 = REAL (Returned)
*         Y      "     "    "     "    "
*     XM = REAL (Returned)
*         X size in metres
*     YM = REAL (Returned)
*         Y   "   "   "

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
*     Error returned by GKS inquiry

*  Externals:
*     GQWKT, sgs_1ERR

*  Read From Common:
*     IZTW    i()   Zone table - SGS workstation ID
*     ISZID   i     Current zone ID
*     IWTID   i()   Workstation table - GKS workstation ID
*     ZTW     i()   Zone table - window
*     ZTV     i()   Zone table - viewport
*     XRES    r()   Workstation description table - X resolution
*     YRES    r()        "           "        "   - Y      "

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'


      REAL X1,X2,Y1,Y2,XM,YM

      REAL SMPN,XV1,XV2,YV1,YV2,DXWW,DYWW
      REAL RWINDO(4),CWINDO(4),RVIEWP(4),CVIEWP(4)
      INTEGER ITUS,IWKID,IERR,JSTAT
      CHARACTER*5 RNAME
      PARAMETER (RNAME='IZONE')



*  Workstation ID
      IWKID = ABS(IZTW(ISZID))

*  Window
      X1=ZTW(1,ISZID)
      X2=ZTW(2,ISZID)
      Y1=ZTW(3,ISZID)
      Y2=ZTW(4,ISZID)

*  Viewport
      XV1=ZTV(1,ISZID)
      XV2=ZTV(2,ISZID)
      YV1=ZTV(3,ISZID)
      YV2=ZTV(4,ISZID)

*  Workstation transformation
      CALL GQWKT(IWTID(IWKID),IERR,ITUS,RWINDO,CWINDO,RVIEWP,CVIEWP)
      IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQWKT',JSTAT)
         XM = 0.0
         YM = 0.0
         GO TO 9999
      END IF

*  NDC units per metre
      DXWW = XRES(IWKID)/(CWINDO(2)-CWINDO(1))
      DYWW = YRES(IWKID)/(CWINDO(4)-CWINDO(3))

      SMPN = MIN((CVIEWP(2)-CVIEWP(1))*(DXWW),
     :            (CVIEWP(4)-CVIEWP(3))*(DYWW))

*  Zone size in metres
      XM=SMPN*(XV2-XV1)
      YM=SMPN*(YV2-YV1)

 9999 CONTINUE

      END
