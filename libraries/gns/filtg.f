      LOGICAL FUNCTION gns_FILTG(ITYPE)
*++
*   Filters the list of available workstations rejecting those which are
*   not supported by GKS (unless GKS is closed in which case all
*   workstations are passed).
*
*   Inputs
*      ITYPE      i    Workstation type
*
*   Outputs
*      gns_FILTG  l    Include in list
*
*   Constants from GKS_PAR
*      GGKCL   i    GKS closed
*
*   Externals
*      GQWKCA
*+
      IMPLICIT NONE

      INCLUDE 'GKS_PAR'

      INTEGER IERR, ITYPE, ICAT, ISTATE

*  Check for GKS being open
      CALL GQOPS(ISTATE)
      IF (ISTATE.EQ.GGKCL) THEN
         gns_FILTG = .TRUE.
      ELSE

*  Make an inquiry about the workstation type and assume that an error
*  means that it doesn't exist.
         CALL GQWKCA(ITYPE,IERR,ICAT)
         IF (IERR.EQ.0) THEN
            gns_FILTG = .TRUE.
         ELSE
            gns_FILTG = .FALSE.
         END IF
      END IF
      END
