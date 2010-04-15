      SUBROUTINE str_WIT1(FORMAT, MAXC, VALUE)

*+
*
*   Name:
*      SUBROUTINE str_WIT1
*
*   Description:
*      Encode white space into value as specified by format.
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
*      The FORMAT is interpretted in the context of the %W edit
*      descriptor. A VALUE string of white space (blanks) is
*      generated.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE FORMAT(256)     ! single format

      INTEGER MAXC         ! size of VALUE

*   Export:
      BYTE VALUE(MAXC)     ! value token

*   Local variables:
      LOGICAL RIGHT        ! whether right justified
      LOGICAL FIXED        ! whether fixed point

      BYTE EDIT            ! edit character

      INTEGER FIELD        ! field size
      INTEGER I            ! loop index
      INTEGER NCHAR        ! string length
      INTEGER PREC         ! precision

      CALL str_DECF(FORMAT, RIGHT, FIELD, FIXED, PREC, EDIT)
      NCHAR = MIN(FIELD, MAXC - 1)

      DO 100 I = 1, NCHAR

         VALUE(I) = 32

 100  CONTINUE

      CALL str_TERM(NCHAR, MAXC, VALUE)

      END
