      SUBROUTINE GRMSG (MESS)
*+
*   - - - - - - -
*     G R M S G     (GKS emulation of GRPCKG)
*   - - - - - - -
*
*   Outputs a message to the user
*
*   Given
*      MESS     c      Message text
*
*   D.L.Terrett  Starlink  Mar 1991
*+
      IMPLICIT NONE
      CHARACTER*(*) MESS
      INCLUDE 'SAE_PAR'

      INTEGER STATUS

      STATUS = SAI__OK
      CALL MSG_OUT('PGPLOT_MSG', MESS, STATUS)

*   This routine is called by PGPLOT just prior to using graphical input so
*   MSG_SYNC must be called to ensure that the message is delivered to the user
*   first.
      CALL MSG_SYNC(STATUS)

      END
