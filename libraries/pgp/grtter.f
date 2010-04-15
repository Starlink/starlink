      SUBROUTINE GRTTER(STRING, SAME)
*+
*     - - - - - - - -
*       G R T T E R     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Test whether device is user's terminal
*
*   Given
*      STRING   c   The device name to be tested.
*
*   Returned
*      SAME     l   .TRUE. if STRING contains a valid name for the
*                    user's terminal; .FALSE. otherwise.
*
*   The supplied string is compared with the string returned by GNS_GTN,
*   therefore this routine will only work if the string origonally came from
*   calling GNS_GTN. Since GRTTER is only used by PGPLOT to test the string
*   obtained from GRQDEV (which uses GNS) this should be OK.
*
*   D.L.Terrett  Starlink  Apr 1991
*+
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'


      CHARACTER*(*) STRING
      LOGICAL SAME

      INTEGER ISTAT, LT
      CHARACTER*20 TERM
      LOGICAL CHR_SIMLR

*  Mark error stack
      CALL ERR_MARK

*  Get name of user' terminal
      ISTAT = SAI__OK
      CALL gns_GTN(TERM, LT, ISTAT)

*  Suppress error message and release stack
      IF (ISTAT.NE.SAI__OK) CALL ERR_ANNUL(ISTAT)
      CALL ERR_RLSE

*  Compare supplied string with terminal name returned by GNS
      IF (ISTAT.EQ.0 .AND. LT.GT.0) THEN
         SAME = CHR_SIMLR( TERM(:LT), STRING)
      ELSE
         SAME = .FALSE.
      END IF

      END
