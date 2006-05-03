      SUBROUTINE sgs_INIT (LUN, JSTAT)
*+
*  Name:
*     INIT

*  Purpose:
*     Open SGS.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Open GKS (unless already open), Set default values in the SGS
*     common block.

*  Arguments:
*     LUN = INTEGER (Given)
*         Logical unit for error messages
*     JSTAT = INTEGER (Returned)
*         Status: 0=OK

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1992 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     sgs_ISTAT, sgs_1GKSIN, sgs_1SGSIN

*-

      IMPLICIT NONE

      INTEGER LUN,JSTAT



*  Initialise status handling mode if not already set
      CALL sgs_ISTAT(-1, JSTAT)

*  Initialise GKS
      CALL sgs_1GKSIN(LUN,JSTAT)

*  Initialise SGS
      CALL sgs_1SGSIN(JSTAT)

      END
