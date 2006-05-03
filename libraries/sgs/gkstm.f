      SUBROUTINE sgs_1GKSTM (JSTAT)
*+
*  Name:
*     GKSTM

*  Purpose:
*     Terminate GKS.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Description:
*     Deactivate and close all workstations, close GKS.

*  Arguments:
*     JSTAT = INTEGER (Returned)
*         Status: 0=OK

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GCLKS     i    operating state - closed
*     GSGOP     i        "       "   - segment open
*     GGKOP     i        "       "   - at least one workstation open

*  Externals:
*     GQOPS, GCLSG, GQOPWK, GCLKS, sgs_1CLWK, sgs_1GKERR

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
