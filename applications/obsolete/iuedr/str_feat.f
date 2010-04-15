      SUBROUTINE str_FEAT(CNTRL, LAST, FIRST, TOKEN)

*+
*
*   Name:
*      SUBROUTINE str_FEAT
*
*   Description:
*      Eat up an edit token from the control string.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          21-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          20-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The part of CNTRL from character positions FIRST to LAST
*      is searched for a suitable token.
*      Text is copied from CNTRL(FIRST:LAST) up to and including
*      a LETTER character or the LAST character position.
*      The FIRST index is incremented to point to the first character
*      position in CNTRL of the next token (FIRST>LAST is possible).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER LETTER       ! LETTER index
      INTEGER MAXTOK       ! maximum length of token string

      PARAMETER (LETTER=1, MAXTOK=256)

*   Import:
      BYTE CNTRL(MAXTOK)   ! control string

      INTEGER LAST         ! last character position

*   Import/Export:
      INTEGER FIRST        ! first character in CNTRL

*   Export:
      BYTE TOKEN(MAXTOK)   ! token string

*   External refernces:
      BYTE str_LOWER       ! lower case of character

      INTEGER str_TYPE     ! lexical type of character

*   Local variables:
      INTEGER CPOS         ! character position in TOKEN

      CPOS = 0

      DO WHILE (FIRST.LE.LAST)
         CPOS = CPOS + 1
         TOKEN(CPOS) = CNTRL(FIRST)
         TOKEN(CPOS) = str_LOWER(TOKEN(CPOS))

         IF (str_TYPE(TOKEN(CPOS)).EQ.LETTER) THEN
            FIRST = FIRST + 1
            GO TO 200
         ELSE
            FIRST = FIRST + 1
         END IF
      END DO

 200  CONTINUE
      CALL str_TERM(CPOS, MAXTOK, TOKEN)

      END
