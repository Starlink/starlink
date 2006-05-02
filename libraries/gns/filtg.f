      LOGICAL FUNCTION gns_FILTG(ITYPE)
*+
*  Name:
*     GNS_FILTG

*  Purpose:
*     Filter list of available workstations

*  Language:
*     Starlink Fortran 77

*  Description:
*     Filters the list of available workstations rejecting those which are
*     not supported by GKS (unless GKS is closed in which case all
*     workstations are passed).

*  Arguments:
*     ITYPE = INTEGER (Given)
*         Workstation type

*  Returned Value:
*     gns_FILTG = LOGICAL (Returned)
*         Include in list

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GGKCL   i    GKS closed

*  Externals:
*     GQWKCA

*-
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
