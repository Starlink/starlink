      SUBROUTINE sgs_1GKSTM (JSTAT)
*+
*   - - - - - -
*    G K S T M      (Internal routine)
*   - - - - - -
*
*   Terminate GKS.
*
*   Deactivate and close all workstations, close GKS.
*
*   Returned:
*      JSTAT     i    status: 0=OK
*
*   Externals:
*      GQOPS, GCLSG, GQOPWK, GCLKS, sgs_1CLWK, sgs_1GKERR
*
*   Constants from GKS_PAR:
*      GCLKS     i    operating state - closed
*      GSGOP     i        "       "   - segment open
*      GGKOP     i        "       "   - at least one workstation open
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER JSTAT

      INCLUDE 'GKS_PAR'


      INTEGER IOPS,IWKID,IERR,NACT
      CHARACTER*5 RNAME
      PARAMETER (RNAME='GKSTM')



*  Test operating state of GKS
      CALL GQOPS(IOPS)
      IF (IOPS.EQ.GGKCL) GO TO 9999

*  Close segment (if open)
      IF (IOPS.EQ.GSGOP) CALL GCLSG

*  Close any open workstations
   10 CONTINUE
      IF (IOPS.EQ.GGKOP) GO TO 20
         CALL GQOPWK(1,IERR,NACT,IWKID)
         IF (IERR.EQ.0) CALL sgs_1CLWK(IWKID)
         CALL GQOPS(IOPS)
      GO TO 10
   20 CONTINUE

*  Close GKS
      CALL GCLKS

*  Check for GKS errors
      CALL sgs_1GKERR(RNAME,JSTAT)

 9999 CONTINUE

      END
