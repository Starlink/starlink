      SUBROUTINE sgs_BZNDC (X1,X2,Y1,Y2, POS, JSTAT)
*+
*   - - - - - -
*    B Z N D C
*   - - - - - -
*
*   Modify the size and position on the display surface of a base zone.
*
*   The following conditions must be satisfied:-
*         (i)   The current zone is a base zone
*         (ii)  There are no other zones on the workstation
*         (iii) The display suface is empty
*
*   This routine contains a workaround for a bug in the RAL GKS.
*
*   Given:
*      X1      r     new NDC x coordinate of bottom left corner of zone
*      Y1      r      "   "  y     "       "   "     "     "    "   "
*      X2      r      "   "  x     "       "  top  right   "    "   "
*      Y2      r      "   "  y     "       "   "     "     "    "   "
*      POS     c*2   position of zone on display surface: the two
*                    characters determine the position of the zone
*                    within the display surface as follows:-
*
*                  1st character = B (bottom), C (centre), or T (top).
*                  2nd character = L (left),   C (centre), or R (right).
*
*     JSTAT    i     status (if in inherited status mode)
*
*   Returned:
*     JSTAT    i     status (0=OK)
*
*   Read from COMMON:
*     IZTW     i()   zone table - SGS workstation ID
*     ISZID    i     current zone ID
*     IWTID    i()   workstation table - GKS workstation ID
*     IWTTY    i()        "        "   - workstation type
*
*   Written to COMMON:
*     ZTW      r()   zone table - window
*     ZTV      r()   zone table - viewport
*
*   Constants from SGSCOM:
*     MXZ      i     maximum number of zones allowed = size of zone table
*   Constants from GKS_PAR:
*     GEMPTY   i     display surface is empty
*     GOTHU    i     device units are "other"
*
*   Externals:
*     sgs_1HSTAT, sgs_1ERR, sgs_1GKERR, sgs_1NORM, sgs_1UPCAS, sgs_SELZ,
*     GQWKDU, GSWKWN, GQDSP, GSWKVP
*
*   Errors:
*     Current zone is not a base zone
*     Scale factor invalid
*     Other zones exist on workstation
*     Display surface is not empty
*     Other zones exist on workstation
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
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
