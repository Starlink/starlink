      SUBROUTINE sgs_IZONE (X1,X2, Y1,Y2, XM,YM)
*+
*   - - - - - -
*    I Z O N E
*   - - - - - -
*
*   Inquire world coordinate bounds and device coordinate size of the
*   current zone.
*
*   Returned:
*      X1      r     x coordinate of lower left corner
*      Y1      r     y      "     "    "    "     "
*      X2      r     x      "     "  upper right  "
*      Y2      r     y      "     "    "     "    "
*      XM      r     x size in metres
*      YM      r     y   "   "   "
*
*   Read from COMMON:
*      IZTW    i()   Zone table - SGS workstation ID
*      ISZID   i     Current zone ID
*      IWTID   i()   Workstation table - GKS workstation ID
*      ZTW     i()   Zone table - window
*      ZTV     i()   Zone table - viewport
*      XRES    r()   Workstation description table - X resolution
*      YRES    r()        "           "        "   - Y      "
*
*   Externals:
*      GQWKT, sgs_1ERR
*
*   Errors:
*      Error returned by GKS inquiry
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
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
