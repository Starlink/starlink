      SUBROUTINE IRG1_SLICE( NAME, SLICE, START, STATUS )
*+
*  Name:
*     IRG1_SLICE

*  Purpose:
*     Find and remove any NDF slice specification from a name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG1_SLICE( NAME, SLICE, START, STATUS )

*  Description:
*     A slice specification is taken to be anything between the first
*     opening and the first closing parenthesis, so long as the closing
*     parentheis is the last character in the name.

*  Arguments:
*     NAME = CHARACTER (Given and Returtned)
*        The name to be checked. On exit, any NDF slice specification
*        contained in the name on entry, is removed.
*     SLICE = CHARACTER (Returned)
*        The slice specification including opening and closing
*        parenthesise. If the input name contains no slice
*        specification, then SLICE is returned blank.
*     START = INTEGER (Given)
*        The position (within the original name) of the opening
*        parethesis. If the name does not containa slice specification
*        then START is returned pointing to the first character beyond
*        the end of the name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-SEP-1991 (DSB):
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
      CHARACTER NAME*(*)

*  Arguments Returned:
      CHARACTER SLICE*(*)
      INTEGER START

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.
*  Local Variables:
      INTEGER NAMLEN             ! Used length of original name.
      INTEGER STOP               ! Position of the first closing
                                 ! parenthesis in the original name.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract and remove any NDF slice specification from the name (i.e.
*  any string occuring between opening and closing parenthesise at the
*  end of the name).
      SLICE = ' '
      NAMLEN = CHR_LEN( NAME )

      START = INDEX( NAME, '(' )

      IF( START .NE. 0 ) THEN
         STOP = INDEX( NAME( START: ), ')' )

         IF( STOP .NE. 0 ) THEN
            STOP = STOP + START - 1 

            IF( STOP .EQ. NAMLEN ) THEN
               SLICE = NAME( START:STOP )
               NAME( START: ) = ' '
            END IF

         END IF      

      END IF

      IF( SLICE .EQ. ' ' ) START = NAMLEN + 1

      END
* $Id$
