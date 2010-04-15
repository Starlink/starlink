      SUBROUTINE str_WRITI(CNTRL, IVALUE, MAXC, LINE, POS)

*+
*
*   Name:
*      SUBROUTINE str_WRITI
*
*   Description:
*      Encode integer value into parameter string under format control.
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
*      IVALUE is planted in the parameter string as specified by the
*      CNTRL format control string.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE LETI             ! ASCII "i"

      PARAMETER (LETI=105)

      INTEGER CODEDIT       ! edit descriptor for value
      INTEGER CODPOS        ! format token for position
      INTEGER CODWHITE      ! format token for white space
      INTEGER MAXTOK        ! maximum length of token string

      PARAMETER (CODEDIT=1, CODPOS=3, CODWHITE=2, MAXTOK=256)

*   Import:
      BYTE CNTRL(MAXTOK)    ! control string

      INTEGER IVALUE        ! integer value
      INTEGER MAXC          ! size of value

*   Import/Export:
      BYTE LINE(MAXC)       ! line to be modified

      INTEGER POS           ! character position

*   External references:
      INTEGER str_INDEX     ! index of character in string
      INTEGER str_LEN       ! string length

*   Local variables:
      LOGICAL FIXED         ! whether fixed point
      LOGICAL RIGHT         ! whether right justified

      BYTE EDIT             ! edit character
      BYTE FORMAT(MAXTOK)   ! format from CNTRL
      BYTE VALUE(MAXTOK)    ! value coded into string

      INTEGER FIELD         ! field size
      INTEGER FIRST         ! first position in CNTRL
      INTEGER LAST          ! last position in CNTRL
      INTEGER PREC          ! precision
      INTEGER TYPE          ! format type index

      FIRST = 1
      LAST = str_LEN(CNTRL)

      DO WHILE (FIRST.LE.LAST)
         CALL str_GTOK(CNTRL, LAST, FIRST, TYPE, FORMAT)

         IF (TYPE.EQ.CODPOS) THEN
            CALL str_WPOS(FORMAT, MAXC, LINE, POS)
         ELSE IF (TYPE.EQ.CODWHITE) THEN
            CALL str_WIT1(FORMAT, MAXTOK, VALUE)
            CALL str_ADD(VALUE, MAXC, LINE, POS)
         ELSE IF (TYPE.EQ.CODEDIT) THEN
            CALL str_DECF(FORMAT, RIGHT, FIELD, FIXED, PREC, EDIT)

            IF (str_INDEX('ioh\\', EDIT).EQ.0) EDIT = LETI

            CALL str_PWI(IVALUE, RIGHT, FIELD, EDIT, MAXTOK, VALUE)
            CALL str_ADD(VALUE, MAXC, LINE, POS)
         ELSE
            CALL str_ADD(FORMAT, MAXC, LINE, POS)
         END IF
      END DO

      END
