      SUBROUTINE sgs_IDUN (XW,YW)
*+
*   - - - - -
*    I D U N
*   - - - - -
*
*   Estimate plotting resolution of current zone.
*
*   For metafiles and segment storage, zero XW & YW are returned.
*
*   Returned:
*      XW        r      size of x resolution elements in world coordinates
*      YW        r        "   " y     "          "    "    "       "
*
*      XW and YW are returned as zero if either the workstation has not
*      got a display surface or an error occurs.
*
*   Read from COMMON:
*      IZTW      i()    zone table - SGS workstation ID
*      ISZID     i      current zone ID
*      IWTTY     i()    workstation table - workstation type
*      IWTCA     i()    workstation table - category
*      IWTID     i()    workstation table - GKS workstation ID
*
*   Constants from GKS_PAR:
*      GWISS     i      workstation category - workstation-independent
*                                              segment store
*      GMO       i           "          "    - metafile output
*      GMI       i           "          "    - metafile input
*
*   Externals:
*       GQDSP, GQWKT, GQNT, sgs_1ERR
*
*   Errors:
*      Error returned by GKS inquiry
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
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
