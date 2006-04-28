      INTEGER FUNCTION STRING_INANYL ( STRING, CHOICE )
*+
*  Name:
*     STRING_INANYL

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     INTEGER FUNCTION

*  Invocation:
*     POSITION = STRING_INANYL ( STRING, CHOICE )

*  Description:
*     Finds the position of the first character in STRING which fails to 
*     match any of the characters in CHOICE.

*  Algorithm:
*     Each character of STRING is compared with the characters in CHOICE 
*     until no match is found, or STRING is exhausted.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     31-MAR-1984 (REVAD::BDK):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Result:
*     POSITION = INTEGER
*           The value returned is the position in STRING at which the 
*           first character mis-match occurs.
*           If no mis-match found, then POSITION is set to zero.

*-

*  Type Definitions:
      IMPLICIT NONE
*  Arguments Given:
      CHARACTER*(*) STRING,          ! character string to be searched
     :              CHOICE           ! set of matching characters
*    External references :
*     <declarations for external function references>
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
      POSITION = 1

      DO WHILE ( (.NOT.FOUND) .AND. (POSITION.LE.NUMSTRING) )
         MATCH = INDEX ( CHOICE, STRING(POSITION:POSITION) )
         IF ( MATCH .EQ. 0 ) THEN
            FOUND = .TRUE.
         ELSE
            POSITION = POSITION + 1
         ENDIF
      ENDDO

      IF ( FOUND ) THEN
         STRING_INANYL = POSITION
      ELSE
         STRING_INANYL = 0
      ENDIF

      END
