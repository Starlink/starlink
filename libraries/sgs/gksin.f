      SUBROUTINE sgs_1GKSIN (LUN, JSTAT)
*+
*  Name:
*     GKSIN

*  Purpose:
*     Initialise GKS.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Description:
*     If GKS is already open, it is left open.

*  Arguments:
*     LUN = INTEGER (Given)
*         Logical unit for error messages
*     JSTAT = INTEGER (Given & Returned)
*         Inherited status (if option selected)
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
*     GGKCL    i      operating state - closed

*  Externals:
*     sgs_1HSTAT, sgs_1GKERR, GQOPS, GOPKS

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
