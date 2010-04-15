      SUBROUTINE GTENV( NAME, ENV, TRANS, STATUS )
*+
*  Name:
*     GTENV

*  Purpose:
*     Get a variable value either from a supplied list of values or from
*     the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GTENV( NAME, ENV, TRANS, STATUS )

*  Description:
*     An attempt is made to translate the supplied NAME as an environemtn
*     variable. If this fails, the supplied list of variable names and
*     values is searched for a "name=value" pair refering to the supplied
*     NAME. IF one is found, the associated value is returned, otherwise
*     an error is reported.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The environment variable or logical name (VMS) to be translated.
*     ENV = CHARACTER * ( * ) (Given)
*        A comma separated list of "name=value" pairs to supplement the
*        values supplied in environment variables.
*     TRANS = CHARACTER * ( * ) (Returned)
*        The variable value, or blank if variable is not defined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAR-1997 (DSB):
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
      CHARACTER NAME*(*)
      CHARACTER ENV*(*)

*  Arguments Returned:
      CHARACTER TRANS*(*)

*  External References:
      INTEGER CHR_LEN            ! Used length of a string.
      LOGICAL CHR_EQUAL          ! Are two strings equal?

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER COMMA              ! Index of next comma
      INTEGER ENVLEN             ! Used length os supplied list
      INTEGER EQUALS             ! Index of next equals sign
      INTEGER F                  ! Index of first non-blank character
      INTEGER L                  ! Index of last non-blank character
      INTEGER LSTAT              ! Was string truncated?
      INTEGER NAMEND             ! Index of end of next variable name
      INTEGER NAMSTA             ! Index of start of next variable name
      INTEGER TEMP               ! Temporary storage
      INTEGER VALEND             ! Index of end of next variable value
      INTEGER VALSTA             ! Index of start of next variable value
      LOGICAL FOUND              ! Has variable been found?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Attempt to translate the supplied environment variable.
      CALL PSX_GETENV( NAME, TRANS, STATUS )

*  If it was not defined, annul the error message, and search the
*  supplied list.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )

*  Indicate that a value has not yet been found for the requested variable.
         FOUND = .FALSE.

*  Ignore the supplied list if it is blank.
         ENVLEN = CHR_LEN( ENV )
         IF( ENVLEN .GT. 0 ) THEN

*  Store the index of the first character in the next variable name in
*  the list.
            COMMA = 0

*  Loop until the list has been exhausted or the supplied variable has
*  been found.
            DO WHILE( .NOT. FOUND .AND. COMMA .LT. ENVLEN )

*  Store the index of the first character after the previously found comma.
               NAMSTA = COMMA + 1

*  Find the index of the next comma in the list (or use the end of the list
*  if there are no more comma).
               TEMP = INDEX( ENV( COMMA + 1 : ), ',' )
               IF( TEMP .EQ. 0 ) THEN
                  COMMA = ENVLEN + 1
               ELSE
                  COMMA = TEMP + COMMA
               END IF

*  Store the index of the last character in the variable value
               VALEND = COMMA - 1

*  Find the index of the equals sign.
               EQUALS = INDEX( ENV( NAMSTA : ), '=' )
               IF( EQUALS .GT. 0 ) THEN
                  EQUALS = EQUALS + NAMSTA - 1

*  Store the index of the name end and the index of the value start.
                  NAMEND = EQUALS - 1
                  VALSTA = EQUALS + 1

*  Find the indices of the first and last non-blank character in the name.
                  IF( NAMSTA .LE. NAMEND ) THEN
                     CALL CHR_FANDL( ENV( NAMSTA : NAMEND ), F, L )
                  ELSE
                     L = 0
                  END IF

*  If the name is not blank, compare it to the supplied name.
                  IF( L .GT. 0 ) THEN
                     F = NAMSTA + F - 1
                     L = NAMSTA + L - 1
                     IF( CHR_EQUAL( ENV( F : L ), NAME ) ) THEN

*  If it matches, set the flag indicating that we have found the variable.
                        FOUND = .TRUE.

*  Extract the value.
                        IF( VALSTA .LE. VALEND ) THEN
                           CALL CHR_COPY( ENV( VALSTA : VALEND ),
     :                                    .FALSE., TRANS, LSTAT )
                        ELSE
                           TRANS = ' '
                        END IF

                     END IF

                  END IF

               END IF

            END DO

         END IF

*  Report an error if the value has not been found.
         IF( .NOT. FOUND .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETC( 'VAR', name )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Environment variable ^VAR not defined.',
     :                   STATUS )
         END IF

      END IF

      END
