      SUBROUTINE sgs_TPZ (IZIN, XIN,YIN, IZOUT, XOUT,YOUT, JSTAT)
*+
*   - - - -
*    T P Z
*   - - - -
*
*   Convert position in one zone to position in another.
*
*   Given:
*      IZIN        i      zone of input position
*      XIN         r      x position in zone IZIN
*      YIN         r      y    "     "    "   "
*      IZOUT       i      zone of output position
*
*   Returned:
*      XOUT        r      x position in zone IZOUT
*      YOUT        r      y    "     "    "   "
*      JSTAT       i      status (0=OK)
*
*   Read from COMMON:
*      IZTW        i()    zone Table - workstation ID
*      ZTV         r()      "    "   - viewport
*      ZTW         r()      "    "   - window
*
*   Constants from SGSCOM:
*      MXZ         i      Maximum number of zones allowed
*
*   Externals:
*      sgs_1ERR, sgs_1HSTAT
*
*   Errors:
*      Invalid zone ID
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
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
