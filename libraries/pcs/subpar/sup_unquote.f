*+  SUBPAR_UNQUOTE - Remove surrounding quotes and escaped quotes
      SUBROUTINE SUBPAR_UNQUOTE ( ENTRY, VALUE, STATUS )
*   Description :
*     Produces the 'value' of a character string formatted following 
*     ADAM syntax - ie. It is contained within quotes, and quotes can be
*     included as part of the string by doubling them. Single and double
*     quotes are equivalent.
*     If the given string does not start and end with a quote, it is
*     simply copied to the output string. 
*   Invocation :
*     CALL SUBPAR_UNQUOTE ( ENTRY, VALUE, STATUS )
*   Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           The value-string to be interpreted
*     VALUE=CHARACTER*(*) (returned)
*           The processed string
*     STATUS=INTEGER (returned)
*   Method :

*   Deficiencies :
*     <description of any deficiencies>
*   Bugs :
*     <description of any "bugs" which have not been fixed>
*   Authors :
*     A.J.Chipperfield (STARLINK)
*   History :
*     28-OCT-1997 (AJC):
*        Original
*   endhistory
*   Type Definitions :
      IMPLICIT NONE
*   Global constants :
      INCLUDE 'SAE_PAR'

*   Import :
      CHARACTER*(*) ENTRY       ! The value-string to be interpreted

*   Export :
      CHARACTER*(*) VALUE       ! The processed character string

*   Status :
      INTEGER STATUS

*   External references :
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*   Local variables :
      INTEGER ENTLEN            ! Used length of given string
      INTEGER I
      INTEGER J
      LOGICAL QUOTE             ! Quote encountered?
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      ENTLEN = CHR_LEN( ENTRY )

* Check if string starts and ends with quote
      IF ( ( ( ENTRY(1:1) .EQ. '''' ) .OR.
     :       ( ENTRY(1:1) .EQ. '"' ) ) .AND. 
     :     ( ( ENTRY(ENTLEN:ENTLEN) .EQ. '''' ) .OR.
     :       ( ENTRY(ENTLEN:ENTLEN) .EQ. '"' ) ) ) THEN
* It is a quoted string
         J = 1
         VALUE = ' '
         QUOTE = .FALSE.

         DO I = 2, ENTLEN
            IF ( ( ENTRY(I:I) .EQ. '''' ) .OR.
     :           ( ENTRY(I:I) .EQ. '"' ) ) THEN
               IF ( QUOTE ) THEN
                  QUOTE = .FALSE.
                  VALUE(J:J) = ENTRY(I:I)
                  J = J + 1
               ELSE
                  QUOTE = .TRUE.
               ENDIF
            ELSE
               VALUE(J:J) = ENTRY(I:I)
               J = J + 1
            ENDIF
            IF ( J .GT. LEN( VALUE ) ) GOTO 99
         ENDDO

      ELSE
*  Not a quoted string
         VALUE = ENTRY

      ENDIF

99    CONTINUE

      END
