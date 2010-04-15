      SUBROUTINE str_JUST(VALUE, RIGHT, FIELD, MAXC, TOKEN)

*+
*
*   Name:
*      SUBROUTINE str_JUST
*
*   Description:
*      Copy value string into token string with justification.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          21-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          22-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The TOKEN string is blank filled to the larger size
*      of the VALUE string or FIELD.
*      The VALUE string is then copied into the TOKEN string.
*      If the VALUE string is shorter thasn FIELD then
*      if RIGHT is TRUE then it is copied right justified.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE VALUE(256)      ! value string

      LOGICAL RIGHT        ! whether right justified

      INTEGER FIELD        ! field size
      INTEGER MAXC         ! size of TOKEN

*   Export:
      BYTE TOKEN(MAXC)     ! string to receive value

*   External references:
      INTEGER STR_LEN      ! string length

*   Local variables:
      INTEGER FIRST        ! start position of VALUE in TOKEN
      INTEGER I            ! loop index
      INTEGER NCHAR        ! character counter

*   Blank fill TOKEN
      NCHAR = MIN(FIELD, MAXC - 1)

      DO 100 I = 1, NCHAR

         TOKEN(I) = 32

 100  CONTINUE

      CALL str_TERM(NCHAR, MAXC, TOKEN)

*   Plant VALUE in TOKEN
      NCHAR = str_LEN(VALUE)

      IF (NCHAR.LT.FIELD .AND. RIGHT) THEN

         FIRST = FIELD - NCHAR + 1

      ELSE

         FIRST = 1

      END IF

      CALL str_PLANT(VALUE, FIRST, MAXC, TOKEN)

      END
