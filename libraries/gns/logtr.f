      SUBROUTINE GNS_1LOGTR ( LNAME, EQNAM, LENEQ, STATUS )

*++
*   GNS_1LOGTR - Perform one logical translation of the given name
*
*   Description :
*     This routine translates the given environment variable into an 
*     equivalence name.
*     If the translation fails then a zero length string is returned.
*
*   Input arguments:
*     LNAME   c*(*)   Name to translate
*
*   Output arguments:
*     EQNAM   c*(*)   Equivalence name
*     LENEQ   i       Length of name
*     STATUS  i       Inherited status
*
*+
*     D L Terrett  15 Jan 1991

      IMPLICIT NONE

      CHARACTER *(*) LNAME
      CHARACTER *(*) EQNAM
      INTEGER LENEQ
      INTEGER STATUS

      INTEGER STRLEN
      CHARACTER*255 STRING, STRING1

*   Initialise the output arguments
      LENEQ = 0
      EQNAM = ' '
      IF ( STATUS .EQ. 0 ) THEN

*   Copy the input string into a local variable
         CALL GNS_1TRIM( LNAME, STRING, STRLEN )

*   Translate the name
         CALL GETENV(STRING(:STRLEN), STRING1)

*   Trim the resulting string
         CALL GNS_1TRIM( STRING1, EQNAM, LENEQ )

      ENDIF

      END

