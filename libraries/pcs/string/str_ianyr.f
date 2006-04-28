      INTEGER FUNCTION STRING_IANYR ( STRING, CHOICE )
*+
*  Name:
*     STRING_IANYR

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     INTEGER FUNCTION

*  Invocation:
*     POSITION = STRING_IANYR ( STRING, CHOICE )

*  Description:
*     Finds the position of the last character in STRING which matches 
*     any of the characters in CHOICE.

*  Algorithm:
*     Each character of STRING is compared with the characters in CHOICE 
*     until a match is found, or STRING is exhausted, starting with the 
*     last character of STRING and working towards its start.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     10-APR-1984 (REVAD::BDK):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Result:
*     POSITION = INTEGER
*           The value returned is the position in STRING at which the 
*           rightmost character match occurs.
*           If no match found, then POSITION is set to zero.

*-

*  Type Definitions:
      IMPLICIT NONE
*  Arguments Given:
      CHARACTER*(*) STRING,          ! character string to be searched
     :              CHOICE           ! set of matching characters
*  Local Variables:
      INTEGER NUMSTRING,             ! number of characters in STRING
     :        NUMCHOICE,             ! number of characters in CHOICE
     :        POSITION,              ! current position in STRING
     :        MATCH                  ! position in CHOICE
      LOGICAL FOUND                  ! controller for search loop
*.

      NUMSTRING = LEN ( STRING )
      NUMCHOICE = LEN ( CHOICE )

      FOUND = .FALSE.
      POSITION = NUMSTRING

      DO WHILE ( (.NOT.FOUND) .AND. (POSITION.GE.1) )
         MATCH = INDEX ( CHOICE, STRING(POSITION:POSITION) )
         IF ( MATCH .NE. 0 ) THEN
            FOUND = .TRUE.
         ELSE
            POSITION = POSITION - 1
         ENDIF
      ENDDO

      IF ( FOUND ) THEN
         STRING_IANYR = POSITION
      ELSE
         STRING_IANYR = 0
      ENDIF

      END
