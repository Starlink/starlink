      SUBROUTINE KPG1_ASPTP( FRAME, NAX, POS, SYMBLS, SEP, TEXT, IAT, 
     :                       STATUS )
*+
*  Name:
*     KPG1_ASPTP

*  Purpose:
*     Put a formatted AST position into a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASPTP( FRAME, NAX, POS, SYMBLS, SEP, TEXT, IAT, STATUS )

*  Description:
*     This routine puts a formatted position into a text string starting
*     at a specified index within the string. Axis symbols may optionally
*     be included. The axis values are separated by a specified string.

*  Arguments:
*     FRAME = INTEGER (Given)
*        The Frame in which the position is defined.
*     NAX = INTEGER (Given)
*        The number of axes in the Frame.
*     POS( NAX ) = DOUBLE PRECISION (Given)
*        The position to format.
*     SYMBLS = LOGICAL (Given)
*        Are axis symbols to be included? If so, each axis value is
*        formatted with a "symbol=value" string.
*     SEP = CHARACTER * ( * ) (Given)
*        The separator for axis values. Trailing spaces are significant.
*     TEXT = CHARACTER * ( * ) (Given and Returned)
*        The text to hold the formatted values. 
*     IAT = INTEGER (Given and Returned)
*        On entry, the index of the last character before the point at
*        which the text is to be placed. On exit, the index of the last
*        character written by this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER FRAME
      INTEGER NAX
      DOUBLE PRECISION POS( NAX )
      LOGICAL SYMBLS
      CHARACTER SEP*(*)

*  Arguments Given and Returned:
      CHARACTER TEXT*(*)
      INTEGER IAT 

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER SYMBL*60           ! Axis symbol
      CHARACTER ATTR*20            ! Attribute name
      INTEGER I                    ! Axis index
      INTEGER JAT                  ! Character index
      INTEGER TLEN                 ! Length of character variable
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the declared length of the text.
      TLEN = LEN( TEXT )

*  Ensure the remainder of the string is blank.
      IF( IAT .LT. TLEN ) TEXT( IAT + 1 : ) = ' '
      
*  Loop round each axis.
      DO I = 1, NAX

*  If required, append the axis symbol to the string followed by an equals, 
*  but only if the symbol is not blank.
         IF( SYMBLS ) THEN
            ATTR = 'SYMBOL('
            JAT = 7
            CALL CHR_PUTI( I, ATTR, JAT )
            CALL CHR_APPND( ')', ATTR, JAT )
            SYMBL = AST_GETC( FRAME, ATTR( : JAT ), STATUS )
            IF( SYMBL .NE. ' ' ) THEN
               CALL KPG1_PGESC( SYMBL, STATUS )
               CALL CHR_APPND( SYMBL, TEXT, IAT )
               CALL CHR_APPND( ' =', TEXT, IAT )
               IAT = IAT + 1
            END IF
         END IF

*  Format the axis value.
         CALL CHR_APPND( AST_FORMAT( FRAME, I, POS( I ), STATUS ),
     :                   TEXT, IAT )

*  Append a separator unless this is the last axis.
         IF( I .NE. NAX ) THEN
            IF( IAT .LT. TLEN ) TEXT( IAT + 1 :  ) = SEP
            IAT = IAT + LEN( SEP )
         END IF

      END DO

      END
