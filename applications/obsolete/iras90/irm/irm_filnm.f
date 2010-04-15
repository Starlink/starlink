      SUBROUTINE IRM_FILNM( INSTR, OUTSTR, STATUS )
*+
*  Name:
*     IRM_FILNM

*  Purpose:
*     Extract a file name from an NDG file specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_FILNM( INSTR, OUTSTR, STATUS )

*  Description:
*     The file name is considered to be everything occurring after the
*     final "]" (VMS) or "/" (UNIX) character. If neither of these two
*     characters are found in the string then the supplied string is
*     returned unchanged.

*  Arguments:
*     INSTR = CHARACTER * ( * ) (Given)
*        The full file specification (as retrieved from an NDG group).
*     OUTSTR = CHARACTER * ( * ) (Returned)
*        The file name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-NOV-1992 (DSB):
*        Original version.
*     2-AUG-1993 (DSB):
*        Name changed from TRACA0 to IRM_FILNM.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER INSTR*(*)

*  Arguments Returned:
      CHARACTER OUTSTR*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER C*1              ! Current test character.
      INTEGER I                  ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned string to equal the supplied string.
      OUTSTR = INSTR

*  Loop round each character in the supplied string, starting at the end
*  and working back to the start.
      DO I = LEN( INSTR ), 1, -1
         C = INSTR( I : I )

*  If this character is a "/" OR A "]" a file name has been found.
         IF( C .EQ. '/' .OR. C .EQ. ']' ) THEN
            OUTSTR = INSTR( I + 1 : )
            GO TO 999
         END IF

      END DO

 999  CONTINUE

      END
