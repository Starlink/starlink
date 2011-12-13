      SUBROUTINE sgs_IDUN (XW,YW)
*+
*  Name:
*     IDUN

*  Purpose:
*     Estimate plotting resolution of current zone.

*  Language:
*     Starlink Fortran 77

*  Description:
*     For metafiles and segment storage, zero XW & YW are returned.

*  Arguments:
*     XW = REAL (Returned)
*         Size of x resolution elements in world coordinates
*     YW = REAL (Returned)
*         "   " y     "          "    "    "       "

*  Notes:
*     XW and YW are returned as zero if either the workstation has not
*     got a display surface or an error occurs.

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

*  Constants From GKS_PAR:
*     GWISS     i      workstation category - workstation-independent
*     segment store
*     GMO       i           "          "    - metafile output
*     GMI       i           "          "    - metafile input

*  Errors:
*     Error returned by GKS inquiry

*  Externals:
*     GQDSP, GQWKT, GQNT, sgs_1ERR

*  Read From Common:
*     IZTW      i()    zone table - SGS workstation ID
*     ISZID     i      current zone ID
*     IWTTY     i()    workstation table - workstation type
*     IWTCA     i()    workstation table - category
*     IWTID     i()    workstation table - GKS workstation ID

*-

      IMPLICIT NONE

      REAL XW,YW

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER IWKID,JSTAT
      INTEGER IERR,ITUS,IUNIT,IXP,IYP
      REAL XNDW,YNDW,XD,YD,XPD,YPD,WINDO(4),VIEWP(4)
      REAL RWINDO(4),CWINDO(4),RVIEWP(4),CVIEWP(4)
      REAL XN,YN,XDND,YDND
      CHARACTER*4 RNAME
      PARAMETER (RNAME='IDUN')



*  Workstation ID
      IWKID = ABS(IZTW(ISZID))
      IF (IWTCA(IWKID).EQ.GWISS.OR.IWTCA(IWKID).EQ.GMO
     :                         .OR.IWTCA(IWKID).EQ.GMI) GO TO 9000

*  Find size of workstation
      CALL GQDSP(IWTTY(IWKID),IERR,IUNIT,XD,YD,IXP,IYP)
      IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQDSP',JSTAT)
         GO TO 9000
      END IF

*  Calculate resolution
      XPD=XD/REAL(IXP)
      YPD=YD/REAL(IYP)

*  Express device units in normalised device units
      CALL GQWKT(IWTID(IWKID),IERR,ITUS,RWINDO,CWINDO,RVIEWP,CVIEWP)
      IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQWKT',JSTAT)
         GO TO 9000
      END IF
      XDND=ABS((CVIEWP(2)-CVIEWP(1))/(CWINDO(2)-CWINDO(1)))
      YDND=ABS((CVIEWP(4)-CVIEWP(3))/(CWINDO(4)-CWINDO(3)))

*  Express normalised device units in world units
      CALL GQNT(1,IERR,WINDO,VIEWP)
      IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQNT',JSTAT)
         GO TO 9000
      END IF
      XNDW=(VIEWP(2)-VIEWP(1))/(WINDO(2)-WINDO(1))
      YNDW=(VIEWP(4)-VIEWP(3))/(WINDO(4)-WINDO(3))

*  Express plotting units in NDC
      XN=XPD/XDND
      YN=YPD/YDND

*  Express plotting units in world units
      XW=XN/XNDW
      YW=YN/YNDW
      GO TO 9999

*  Not a real workstation or some sort of error
 9000 CONTINUE
      XW = 0.0
      YW = 0.0

 9999 CONTINUE

      END
