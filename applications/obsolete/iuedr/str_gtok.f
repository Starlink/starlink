      SUBROUTINE str_GTOK(CNTRL, LAST, FIRST, TYPE, TOKEN)

*+
*
*   Name:
*      SUBROUTINE str_GTOK
*
*   Description:
*      Find the next token in the control string.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          15-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      The part of CNTRL from character positions FIRST to LAST
*      is searched for a suitable token.
*      A token starting with PERCENT is of type EDIT and must
*      terminate on a LETTER or at the LAST character position.
*      Other tokens end before the next PERCENT or at the LAST
*      character position.
*      The PERCENT character can be escaped by duplication.
*      The FIRST index is incremented to point to the first character
*      position in CNTRL of the next token (FIRST>LAST is possible).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE LETP             ! ASCII "p"
      BYTE LETW             ! ASCII "w"
      BYTE PERCENT          ! ASCII "%"

      PARAMETER (LETP=112, LETW=119, PERCENT=37)

      INTEGER CODEDIT       ! edit descriptor for value
      INTEGER CODPOS        ! format token for position
      INTEGER CODTEXT       ! format token for straight text
      INTEGER CODWHITE      ! format token for white space
      INTEGER MAXTOK        ! maximum length of token string

      PARAMETER (CODEDIT=1, CODPOS=3, CODTEXT=0, CODWHITE=2,
     :           MAXTOK=256)

*   Import:
      BYTE CNTRL(MAXTOK)    ! cntrl string

      INTEGER LAST          ! last character position

*   Import/Export:
      INTEGER FIRST         ! first character in CNTRL

*   Export:
      INTEGER TYPE          ! token type index

      BYTE TOKEN(MAXTOK)    ! token string

      IF (FIRST.EQ.LAST) THEN
         CALL str_MOVE(CNTRL(FIRST), MAXTOK, TOKEN)
         FIRST = FIRST + 1
         TYPE = CODTEXT
      ELSE IF (CNTRL(FIRST).EQ.PERCENT .AND.
     :         CNTRL(FIRST+1).EQ.PERCENT) THEN
         CALL str_TEAT(CNTRL, LAST, FIRST, TOKEN)
         TYPE = CODTEXT
      ELSE IF (CNTRL(FIRST).EQ.PERCENT) THEN
         CALL str_FEAT(CNTRL, LAST, FIRST, TOKEN)

         IF (CNTRL(FIRST-1).EQ.LETW) THEN
            TYPE = CODWHITE
         ELSE IF (CNTRL(FIRST-1).EQ.LETP) THEN
            TYPE = CODPOS
         ELSE
            TYPE = CODEDIT
         END IF

      ELSE
         CALL str_TEAT(CNTRL, LAST, FIRST, TOKEN)
         TYPE = CODTEXT
      END IF

      END
