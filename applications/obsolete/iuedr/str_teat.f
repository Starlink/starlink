      SUBROUTINE str_TEAT(CNTRL, LAST, FIRST, TOKEN)

*+
*
*   Name:
*      SUBROUTINE str_TEAT
*
*   Description:
*      Eat up a text token from the control string.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          21-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          15-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      The part of CNTRL from character positions FIRST to LAST
*      is searched for a suitable token.
*      Text is copied from CNTRL(FIRST:LAST) until before
*      a PERCENT character.
*      The PERCENT character can be escaped by duplication.
*      The FIRST index is incremented to point to the first character
*      position in CNTRL of the next token (FIRST>LAST is possible).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE PERCENT          ! ASCII "%"

      PARAMETER (PERCENT=37)

      INTEGER CODEDIT       ! edit descriptor for value
      INTEGER CODPOS        ! format token for position
      INTEGER CODTEXT       ! format token for straight text
      INTEGER CODWHITE      ! format token for white space
      INTEGER MAXTOK        ! maximum length of token string

      PARAMETER (CODEDIT=1, CODPOS=3, CODTEXT=0, CODWHITE=2,
     :           MAXTOK=256)

*   Import:
      BYTE CNTRL(256)       ! control string

      INTEGER LAST          ! last character position

*   Import/Export:
      INTEGER FIRST         ! first character in CNTRL

*   Export:
      BYTE TOKEN(MAXTOK)    ! token string

*   Local variables:
      INTEGER CPOS        ! character position in TOKEN

      CPOS = 0

      DO WHILE (FIRST.LE.LAST)

         IF (CNTRL(FIRST).EQ.PERCENT) THEN

            IF (FIRST.EQ.LAST) THEN
               GO TO 200
            ELSE IF (CNTRL(FIRST+1).NE.PERCENT) THEN
               GO TO 200
            ELSE
               CPOS = CPOS + 1
               TOKEN(CPOS) = CNTRL(FIRST)
               FIRST = FIRST + 2
            END IF

         ELSE
            CPOS = CPOS + 1
            TOKEN(CPOS) = CNTRL(FIRST)
            FIRST = FIRST + 1
         END IF
      END DO

 200  CONTINUE
      CALL str_TERM(CPOS, MAXTOK, TOKEN)

      END
