      SUBROUTINE IRG1_BRACK( STRING, STATUS )
*+
*  Name:
*     IRG1_BRACK

*  Purpose:
*     Replace matching angle brackets by square brackets.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG1_BRACK( STRING, STATUS )

*  Description:
*     Matching pairs of angle brackets "< >" are replaced with
*     equivalent square brackets "[ ]". Un-matched angle brackets are
*     left alone.

*  Arguments:
*     STRING = CHARACTER (Given and Returned)
*        The string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given and Returned:
      CHARACTER STRING*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CL                 ! Position of next closing angle
                                 ! bracket.
      INTEGER OP                 ! Position of last opening angle
                                 ! bracket which preceeds current
                                 ! closing angle bracket.
      INTEGER START              ! Starting position of search for next
                                 ! closing angle bracket.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round until all pairs have been replaced.
      START = 1
      CL = INDEX( STRING, '>' )

      DO WHILE( CL .GE. START )

*  Find the last "<" character which preceeds the first ">" character.
         OP = CL - 1
 10      CONTINUE
         IF( OP .GT. 0 ) THEN
            IF( STRING( OP : OP ) .NE. '<' ) THEN
               OP  = OP - 1
               GO TO 10
            END IF
         END IF

*  If a "<" character was found, set it to "[" and the ">" character to
*  "]".
         IF( OP .GT. 0 ) THEN
            STRING( OP : OP ) = '['
            STRING( CL : CL ) = ']'
         END IF

*  Find the next ">" character.
         START = CL + 1
         CL = INDEX( STRING( START : ), '>' ) + START - 1

      END DO

      END
* $Id$
