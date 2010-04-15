      SUBROUTINE str_CLEAN(STR)

*+
*
*   Name:
*      SUBROUTINE str_CLEAN
*
*   Description:
*      Remove non-printable ASCII characters from string.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      10-SEP-81
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      Assume an ASCII collating sequence. Set all non-printables to
*      BLANK in place.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE BLANK          ! ASCII " "

      PARAMETER (BLANK=32)

      INTEGER ARB         ! arbitrary string length

      PARAMETER (ARB=100)

*   Import/Export:
      BYTE STR(ARB)       ! string to be cleaned

*   External references:
      INTEGER str_LEN     ! string length

*   Local variables:
      INTEGER I           ! loop index
      INTEGER NCHAR       ! character count

      NCHAR = str_LEN(STR)

      IF (NCHAR.GT.0) THEN

         DO I = 1, NCHAR

            IF (STR(I).LT.32 .OR. STR(I).GT.127) STR(I) = BLANK

         END DO
      END IF

      END
