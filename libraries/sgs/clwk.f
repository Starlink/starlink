      SUBROUTINE sgs_1CLWK (IWKID)
*+
*   - - - - -
*    C L W K     (Internal routine)
*   - - - - -
*
*   De-activate (if active) and close a workstation.
*
*   Given:
*      IWKID    i        GKS Workstation ID
*
*   Externals:
*      GQWKS, GDAWK, GCLWK
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER IWKID

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'


      INTEGER IERR, ISTATE



*  Deactivate if active
      CALL GQWKS(IWKID,IERR,ISTATE)
      IF (IERR.EQ.0.AND.ISTATE.EQ.GACTIV) CALL GDAWK(IWKID)

*  Close
      CALL GCLWK(IWKID)

      END
