      SUBROUTINE sgs_ZONE (X1, X2, Y1, Y2, IZONID, JSTAT)
*+
*   - - - - -
*    Z O N E
*   - - - - -
*
*   Create and select a zone of the specified extent.
*
*   The new zone has a window whose world coordinate extent is (0,0) to
*   (X,Y), where X/Y is the true aspect ratio of the viewport and the
*   smaller of X or Y is unity.
*
*   Given:
*      X1,X2,Y1,Y2   r      bounds of new zone in current zone's
*                                                      world coordinates
*      JSTAT         i      inherited status (if option selected)
*
*   Returned:
*      IZONID        i      zone identifier for new zone
*      JSTAT         i      status (0=OK)
*
*   Read from COMMON:
*      ISZID         i      current zone ID
*      ZTW           i()    zone table - window
*      ZTV           i()     "     "   - viewport
*      IZTW          i()     "     "   - workstation ID
*
*   Externals:
*      sgs_1HSTAT, sgs_1BNORM, sgs_1ERR, sgs_1GETZ, sgs_1NEWZ
*
*   Errors:
*      New zone not inside current
*      Too many zones
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
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
