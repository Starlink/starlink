*+  STR_IANYL
      INTEGER FUNCTION STR_IANYL ( STRING, CHOICE )
*    Description :
*     Finds the position of the first character in STRING which matches
*     any of the characters in CHOICE.
*    Invocation :
*     POSITION = STR_IANYL ( STRING, CHOICE )
*    Result :
*     POSITION = INTEGER
*           The value returned is the position in STRING at which the
*           first character match occurs.
*           If no match found, then POSITION is set to zero.
*    Method :
*     Each character of STRING is compared with the characters in CHOICE
*     until a match is found, or STRING is exhausted.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     31.03.1984: original version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      CHARACTER*(*) STRING,          ! character string to be searched
     :              CHOICE           ! set of matching characters
*    Local variables :
      INTEGER NUMSTRING,             ! number of characters in STRING
     :        NUMCHOICE,             ! number of characters in CHOICE
     :        POSITION,              ! current position in STRING
     :        MATCH                  ! position in CHOICE
      LOGICAL FOUND                  ! controller for search loop
*-

      NUMSTRING = LEN ( STRING )
      NUMCHOICE = LEN ( CHOICE )

      FOUND = .FALSE.
      POSITION = 1

      DO WHILE ( (.NOT.FOUND) .AND. (POSITION.LE.NUMSTRING) )
         MATCH = INDEX ( CHOICE, STRING(POSITION:POSITION) )
         IF ( MATCH .NE. 0 ) THEN
            FOUND = .TRUE.
         ELSE
            POSITION = POSITION + 1
         ENDIF
      ENDDO

      IF ( FOUND ) THEN
         STR_IANYL = POSITION
      ELSE
         STR_IANYL = 0
      ENDIF

      END
