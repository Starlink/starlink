      SUBROUTINE sgs_1BNORM (X1,X2, Y1,Y2, X1N,X2N, Y1N,Y2N, JSTAT)
*+
*   - - - - - -
*    B N O R M    (internal routine)
*   - - - - - -
*
*   Order bounds for normal orientation and check for non-zero area.
*
*   Given:
*      X1       r      lower bound in x
*      Y1       r        "     "    " y
*      X2       r      upper   "    " x
*      Y2       r        "     "    " y
*      JSTAT    i      inherited status (if option selected)
*
*   Returned:
*      X1N      r      normalized lower bound in x
*      Y1N      r           "       "     "    " y
*      X2N      r           "     Upper   "    " x
*      Y2N      r           "       "     "    " y
*      JSTAT    i      status (0=OK)
*
*   Errors:
*      Zero extent
*
*   Externals:
*      sgs_1HSTAT, sgs_1ERR
*
*  P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      INCLUDE 'SGS_ERR'

      
      REAL X1,X2,Y1,Y2,X1N,X2N,Y1N,Y2N
      INTEGER JSTAT

      CHARACTER RNAME*5
      PARAMETER (RNAME='BNORM')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Order X bounds
      IF (X1.LT.X2) THEN
         X1N=X1
         X2N=X2
      ELSE
         X1N=X2
         X2N=X1
      END IF

*  Order Y bounds
      IF (Y1.LT.Y2) THEN
         Y1N=Y1
         Y2N=Y2
      ELSE
         Y1N=Y2
         Y2N=Y1
      END IF

*  Check for zero extent
      IF (X2N-X1N.LE.0.0 .OR. Y2N-Y1N.LE.0.0)
     :              CALL sgs_1ERR(SGS__ZEREX,RNAME,'Zero extent',JSTAT)

*  Exit
 9999 CONTINUE

      END
