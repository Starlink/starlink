      SUBROUTINE sgs_1GKSIN (LUN, JSTAT)
*+
*   - - - - - -
*    G K S I N     (Internal routine)
*   - - - - - -
*
*   Initialise GKS.
*
*   If GKS is already open, it is left open.
*
*   Given:
*      LUN      i      logical unit for error messages
*      JSTAT    i      inherited status (if option selected)
*
*   Returned:
*      JSTAT    i      status: 0=OK
*
*   Constants from GKS_PAR:
*      GGKCL    i      operating state - closed
*
*    Externals:
*      sgs_1HSTAT, sgs_1GKERR, GQOPS, GOPKS
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER LUN,JSTAT

      INCLUDE 'GKS_PAR'


      INTEGER IOPSTA
      CHARACTER*5 RNAME
      PARAMETER (RNAME='GKSIN')



*  Handle incoming status
      CALL sgs_1HSTAT(JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Open GKS (unless already open)
      CALL GQOPS(IOPSTA)
      IF (IOPSTA.EQ.GGKCL) CALL GOPKS(LUN,-1)

*  Check for GKS errors
      CALL sgs_1GKERR(RNAME,JSTAT)

*  Exit
 9999 CONTINUE

      END
