      SUBROUTINE sgs_BZNDC (X1,X2,Y1,Y2, POS, JSTAT)
*+
*  Name:
*     BZNDC

*  Purpose:
*     Modify the size and position on the display surface of a base zone.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The following conditions must be satisfied:-
*       (i)   The current zone is a base zone
*       (ii)  There are no other zones on the workstation
*       (iii) The display suface is empty

*  Arguments:
*     X1 = REAL (Given)
*         New NDC x coordinate of bottom left corner of zone
*     Y1 = REAL (Given)
*         "   "  y     "       "   "     "     "    "   "
*     X2 = REAL (Given)
*         "   "  x     "       "  top  right   "    "   "
*     Y2 = REAL (Given)
*         "   "  y     "       "   "     "     "    "   "
*     POS = CHAR*2 (Given)
*         Position of zone on display surface: the two
*         characters determine the position of the zone
*         within the display surface as follows:-
*         1st character = B (bottom), C (centre), or T (top).
*         2nd character = L (left),   C (centre), or R (right).
*     JSTAT = INTEGER (Given & Returned)
*         Status (if in inherited status mode)
*         Status (0=OK) (if non-inherited)

*  Notes:
*     This routine contains a workaround for a bug in the RAL GKS.

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
*     GEMPTY   i     display surface is empty
*     GOTHU    i     device units are "other"

*  Constants From Sgscom:
*     MXZ      i     maximum number of zones allowed = size of zone table

*  Errors:
*     Current zone is not a base zone
*     Scale factor invalid
*     Other zones exist on workstation
*     Display surface is not empty
*     Other zones exist on workstation

*  Externals:
*     sgs_1HSTAT, sgs_1ERR, sgs_1GKERR, sgs_1NORM, sgs_1UPCAS, sgs_SELZ,
*     GQWKDU, GSWKWN, GQDSP, GSWKVP

*  Read From Common:
*     IZTW     i()   zone table - SGS workstation ID
*     ISZID    i     current zone ID
*     IWTID    i()   workstation table - GKS workstation ID
*     IWTTY    i()        "        "   - workstation type

*  Written To Common:
*     ZTW      r()   zone table - window
*     ZTV      r()   zone table - viewport

*-

      IMPLICIT NONE

      REAL X1,X2,Y1,Y2
      CHARACTER*2 POS
      INTEGER JSTAT

      CHARACTER RNAME*5,LPOS*2
      PARAMETER (RNAME='BZNDC')
      INTEGER I,IDUM1,IDUM2,IDUM3,IEMPTY,IERR,IUNIT,IWKIDS
      REAL XN,YN,WVX1,WVX2,WVY1,WVY2,GAP,ZASP,WKASP,XM,YM
      REAL DUMMY1,DUMMY2

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Current SGS workstation ID
      IWKIDS = ABS(IZTW(ISZID))

*  Check that current zone is a base zone
      IF (IZTW(ISZID).GE.0) THEN
         CALL sgs_1ERR(SGS__ZONNB,RNAME,
     :                         'Current zone is not a base zone', JSTAT)
         GO TO 9999
      END IF

*  Check that no other zones exist on this SGS workstation
      DO 10 I = 1,MXZ
         IF (ABS(IZTW(I)).EQ.IWKIDS .AND. ISZID.NE.I) THEN
            CALL sgs_1ERR(SGS__OZEWK,RNAME,
     :                        'Other zones exist on workstation', JSTAT)
            GO TO 9999
         END IF
   10 CONTINUE

*  Check that no other SGS workstations exist on this GKS workstation
      DO 20 I = 1,MXWK
         IF (IWTID(I).EQ.IWTID(IWKIDS) .AND. IWKIDS.NE.I) THEN
            CALL sgs_1ERR(SGS__OZEWK,RNAME,
     :      'Other zones exist on workstation', JSTAT)
            GO TO 9999
         END IF
   20 CONTINUE

*  Check that display surface is empty
      CALL GQWKDU(IWTID(IWKIDS),IERR,IDUM1,IDUM2,IEMPTY,IDUM3)
      IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQWKDU',
     :                                                            JSTAT)
         GO TO 9999
      END IF
      IF (IEMPTY.NE.GEMPTY) THEN
         CALL sgs_1ERR(SGS__SURFN,RNAME,'Display surface is not empty',
     :                                                            JSTAT)
         GO TO 9999
      END IF

*  Adjust workstation window
      CALL GSWKWN(IWTID(IWKIDS),X1,X2,Y1,Y2)

*  Check for GKS error
      CALL sgs_1GKERR(RNAME,JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Load new zone limits into zone table
      ZTV(1,ISZID) = X1
      ZTV(2,ISZID) = X2
      ZTV(3,ISZID) = Y1
      ZTV(4,ISZID) = Y2

*  Set up default window for new zones
      CALL sgs_1NORM(X2-X1,Y2-Y1,XN,YN)
      ZTW(1,ISZID)=0.0
      ZTW(2,ISZID)=1.0/YN
      ZTW(3,ISZID)=0.0
      ZTW(4,ISZID)=1.0/XN

*  Set workstation viewport to same aspect ratio and positioned
*  according to the POS argument

*  Convert position to uppercase
      CALL sgs_1UPCAS(POS(:2),LPOS)

*  Get maximum display surface
      CALL GQDSP(IWTTY(IWKIDS),IERR,IUNIT,XM,YM,DUMMY1,DUMMY2)
      IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQDSP',JSTAT)
         GO TO 9999
      END IF

*   Workround for RAL GKS bug
      IF (IUNIT.EQ.GOTHU) THEN
         XM = XM - 1
         YM = YM - 1
      END IF

*  Workstation aspect ratio
      WKASP = XM/YM

*  Zone aspect ratio
      ZASP = (X2-X1)/(Y2-Y1)

*  Calculate workstation viewport limits
      IF (WKASP.GT.ZASP) THEN
         WVY1 = 0.0
         WVY2 = YM
         GAP = XM -  (YM * ZASP)
         IF (LPOS(2:2).EQ.'L') THEN
            WVX1 = 0.0
            WVX2 = XM - GAP
         ELSE IF (LPOS(2:2).EQ.'R') THEN
            WVX1 = GAP
            WVX2 = XM
         ELSE
            WVX1 = GAP/2.0
            WVX2 = XM - GAP/2.0
         END IF
      ELSE
         WVX1 = 0.0
         WVX2 = XM
         GAP = YM - (XM / ZASP)
         IF (LPOS(1:1).EQ.'B') THEN
            WVY1 = 0.0
            WVY2 = YM - GAP
         ELSE IF (LPOS(1:1).EQ.'T') THEN
            WVY1 = GAP
            WVY2 = YM
         ELSE
            WVY1 = GAP/2.0
            WVY2 = YM - GAP/2.0
         END IF
      END IF

*  Set workstation viewport
      CALL GSWKVP(IWTID(IWKIDS),WVX1,WVX2,WVY1,WVY2)

*  Check for GKS error
      CALL sgs_1GKERR(RNAME,JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Reselect zone to set new window and viewport
      CALL sgs_SELZ(ISZID,JSTAT)

 9999 CONTINUE

      END
