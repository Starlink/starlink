      SUBROUTINE CTG1_LASTO( STRING, CVAL, IAT, STATUS )
*+
*  Name:
*     CTG1_LASTO

*  Purpose:
*     Locates the last occurence of CVAL in STRING.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG1_LASTO( STRING, CVAL, IAT, STATUS )

*  Description:
*     The routine locates the last occurence of the single character
*     CVAL in STRING. If an occurence is not located then IAT is
*     returned as 0.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        String to be searched for occurences of CVAL.
*     CVAL = CHARACTER * ( * ) * ( 1 ) (Given)
*        Character whose last occurence is to be located.
*     IAT = INTEGER (Returned)
*        Position  within STRING at which last occurence of CVAL is
*        located.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STRING
      CHARACTER * ( 1 ) CVAL

*  Arguments Returned:
      INTEGER IAT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER STRLEN             ! Length of STRING
      INTEGER WASAT              ! Previous position within STRING
      INTEGER NOWAT              ! Current position within STRING
      LOGICAL MORE               ! Flag for more loops required

*.

*  Initialise.
      IAT = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get lenght of string.
      STRLEN = LEN ( STRING )

*  Initialise string positions
      NOWAT = 0
      WASAT = 1

*  Loop flag.
      MORE = .TRUE.

*  Loop while occurences are still located and string length not
*  exceeded.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( MORE ) THEN
         NOWAT = INDEX( STRING( WASAT : ) , CVAL )
         IF ( NOWAT .EQ. 0 ) THEN

*  No more occurrences.
            MORE = .FALSE.
            IAT = WASAT - 1
         ELSE

*  More to do - increment position within STRING.
            WASAT = NOWAT + WASAT

*  If WASAT now exceeds the string length last occurrence was at end of
*  string.
            IF ( WASAT .GT. STRLEN ) THEN
               MORE = .FALSE.
               IAT = STRLEN
            END IF
         END IF
         GO TO 1
      END IF

      END
