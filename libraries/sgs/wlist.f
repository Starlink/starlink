      SUBROUTINE sgs_WLIST (LU)
*+
*  Name:
*     WLIST

*  Purpose:
*     List all the SGS workstation names.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     LU = INTEGER (Given)
*         Fortran I/O stream number to which the
*         list will be output

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
*     sgs_WNAME, sgs_1WLST, gns_START

*-

      IMPLICIT NONE

      INTEGER LU

      EXTERNAL sgs_1WLST
      INTEGER JSTAT
      CHARACTER*5 RNAME
      PARAMETER (RNAME='WLIST')



*  Check that the workstation name database is accessible
      JSTAT = 0
      CALL gns_START('GKS',JSTAT)

      IF (JSTAT.EQ.0) THEN      
         CALL sgs_WNAME(sgs_1WLST,LU,JSTAT)
         WRITE (LU,'(1X)')
      END IF

      END
