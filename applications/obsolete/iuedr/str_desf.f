      SUBROUTINE str_DESF(FVALUE, CSIGN, SIGDEC, EXPON)

*+
*
*   Name:
*      SUBROUTINE str_DESF
*
*   Description:
*      Design number format.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          25-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          22-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      Determine the maximum number of significant decimal places
*      needed to represent number (when trailing zeros are removed).
*      Code the number in 1PE12.5 format and analyse the space required.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 FVALUE         ! floating point number to be formatted

*   Export:
      INTEGER CSIGN       ! number of sign characters
      INTEGER SIGDEC      ! format decimal places
      INTEGER EXPON       ! base 10 exponent

*   Local variables:
      BYTE STR(256)       ! temporary string to hold number

      INTEGER I           ! loop index
      INTEGER STATUS      ! status

      STATUS = 0
*   Code value
      CALL gen_FTOE(FVALUE, 12, 5, 256, STR)

*   Extract exponent
      CALL gen_STOI(STR(10), EXPON, STATUS)

      IF (STATUS.NE.0) EXPON = 0

*   Remove trailing zeros from float part
      I = 8

 100  CONTINUE

      IF (I.GT.3) THEN

         IF (STR(I).NE.48) GO TO 200
         I = I - 1
         GO TO 100

      END IF

 200  CONTINUE
      SIGDEC = I - 3

*   Sign character count
      IF (STR(1).EQ.43) THEN

         CSIGN = 1

      ELSE IF (STR(1).EQ.45) THEN

         CSIGN = 1

      ELSE

         CSIGN = 0

      END IF

      END
