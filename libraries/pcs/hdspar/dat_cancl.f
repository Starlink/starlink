      SUBROUTINE DAT_CANCL ( PARAM, STATUS )
*+
*  Name:
*     DAT_CANCL

*  Purpose:
*     Cancel association between a parameter and a data object.

*  Language:
*     Fortran 77

*  Invocation:
*     CALL DAT_CANCL ( PARAM, STATUS )

*  Description:
*     An existing association between the named parameter and a data
*     system object is cancelled, and the container file closed.
*     This routine will attempt to operate regardless of the given
*     STATUS value.
*
*     The parameter enters the CANCELLED state.

*  Arguments:
*     PARAM=CHARACTER*(*) (given)
*        Name of program parameter
*     STATUS=INTEGER (given and returned)
*        Global status

*  Algorithm:
*     The internal identifying number for the named parameter is
*     obtained, and used to call SUBPAR_CANCL which does the work.

*  Authors:
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1984 (BDK)
*        Original
*     21-MAR-1985 (BDK)
*        Make execute even if status bad
*     16-JUN-1998 (AJC)
*        Re-format prologue
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) PARAM

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NAMECODE               ! pointer to internal parameter
                                     ! storage

      INTEGER ISTAT                  ! temporary status

*.

      ISTAT = STATUS
      STATUS = SAI__OK

      CALL SUBPAR_FINDPAR ( PARAM, NAMECODE, STATUS )

      CALL SUBPAR_CANCL ( NAMECODE, STATUS )

      IF ( ISTAT .NE. SAI__OK ) STATUS = ISTAT

      END
