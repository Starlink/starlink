      LOGICAL FUNCTION gns_FILTI(TYPE)
*++
*     Dummy IDI workstation filter routine; all workstation types are
*     accepted.
*
*   Inputs
*      TYPE      c    Workstation type
*
*   Outputs
*      gns_FILTI  l    Include in list
*
*+
      IMPLICIT NONE

      CHARACTER*(*) TYPE

      gns_FILTI = .TRUE.

      END
