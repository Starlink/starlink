      SUBROUTINE sgs_1CLWK (IWKID)
*+
*  Name:
*     CLWK

*  Purpose:
*     De-activate (if active) and close a workstation.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Arguments:
*     IWKID = INTEGER (Given)
*         GKS Workstation ID

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

*  Externals:
*     GQWKS, GDAWK, GCLWK

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
