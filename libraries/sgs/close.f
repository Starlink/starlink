      SUBROUTINE sgs_CLOSE
*+
*   - - - - - -
*    C L O S E
*   - - - - - -
*
*   Flush the buffers, close each open workstation (deactivating if
*   active), and close GKS.
*
*   Externals:
*      sgs_FLUSH, sgs_1GKSTM, gns_STOP
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      INTEGER JSTAT



      JSTAT = 0

*  Flush the buffers
      CALL sgs_FLUSH

* Terminate GKS
      CALL sgs_1GKSTM(JSTAT)

* Shut down the GNS database
      CALL gns_STOP('GKS',JSTAT)

      END
