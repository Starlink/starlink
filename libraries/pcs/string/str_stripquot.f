*+  STRING_STRIPQUOT - Remove surrounding quotes and escaped quotes
      SUBROUTINE STRING_STRIPQUOT ( ENTRY, VALUE, STATUS )
*    Description :
*     Produces the 'value' of a character string formatted following 
*     FORTRAN 77 syntax - ie. It is contained within single quotes, but 
*     the single quote character can be included as part of the string 
*     by doubling it.
*     If the given string contains no quotes, then it is simply copied 
*     to the output string.
*    Invocation :
*     CALL STRING_STRIPQUOT ( ENTRY, VALUE, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           The value-string to be interpreted
*     VALUE=CHARACTER*(*) (returned)
*           The processed string
*     STATUS=INTEGER (returned)
*    Method :
*     The quotes surrounding the input string are found. The characters 
*     between them are copied to the output string, pairs of quotes 
*     being contracted to single quotes.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     19.09.1984:  Original (REVAD::BDK)
*     21.11.1996:  Change CHAR(39) in PARAMETER statement to ''''
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      CHARACTER*(*) ENTRY       ! The value-string to be interpreted

*    Export :
      CHARACTER*(*) VALUE       ! The processed character string

*    Status :
      INTEGER STATUS

*    External references :
      INTEGER STRING_IANYR
      EXTERNAL STRING_IANYR

*    Local Constants :
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

*    Local variables :
      INTEGER FIRSTQ                     ! position of first quote in 
                                         ! ENTRY

      INTEGER LASTQ                      ! position of last quote in 
                                         ! ENTRY

      INTEGER I
      INTEGER J

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Find the surrounding quotes
*
      FIRSTQ = INDEX ( ENTRY, QUOTE )
      LASTQ = STRING_IANYR ( ENTRY, QUOTE )
*
*   Copy the characters handling 'escaped' quotes
*
      IF ( FIRSTQ .EQ. 0 ) THEN
*
*      No quotes in string
*
         VALUE = ENTRY

      ELSE IF ( LASTQ .LE. FIRSTQ+1 ) THEN

         VALUE = ' '

      ELSE

         VALUE = ' '
         J = FIRSTQ + 1
         I = 1

         DO WHILE ( ( J .LT. LASTQ ) .AND. ( I .LE. LEN(VALUE) ) )

            VALUE(I:I) = ENTRY(J:J)
            I = I + 1
            IF ( ENTRY(J:J+1) .EQ. QUOTE // QUOTE ) THEN
               J = J + 2
            ELSE
               J = J + 1
            ENDIF

         ENDDO

      ENDIF

      END
