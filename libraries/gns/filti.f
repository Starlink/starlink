      LOGICAL FUNCTION gns_FILTI(TYPE)
*+
*  Name:
*     GNS_FILTI

*  Purpose:
*     Dummy IDI workstation filter routine.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Dummy IDI workstation filter routine; all workstation types are
*     accepted.

*  Arguments:
*     TYPE = CHAR (Given)
*         Workstation type

*  Returned Value:
*     gns_FILTI = LOGICAL (Returned)
*         Include in list

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

      CHARACTER*(*) TYPE

      gns_FILTI = .TRUE.

      END
