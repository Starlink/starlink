      SUBROUTINE sgs_WLIST (LU)
*+
*   - - - - - -
*    W L I S T
*   - - - - - -
*
*   List all the SGS workstation names.
*
*   Given:
*      LU       i      Fortran I/O stream number to which the
*                      list will be output
*
*   Externals:
*      sgs_WNAME, sgs_1WLST, gns_START
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
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
