      SUBROUTINE str_RMBLK(STR)


*+
*
*   Name:
*      SUBROUTINE str_RMBLK
*
*   Description:
*      Remove blanks from a string in situe.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      30-MAY-81
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      Remove all blanks from the string and place the result in place.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE BLANK          ! ASCII blank

      PARAMETER (BLANK=32)

      INTEGER ARB         ! arbitrary string length

      PARAMETER (ARB=100)

*   Import/Export:
      BYTE STR(ARB)       ! string

*   External references:
      INTEGER str_LEN     ! string length

*   Local variables:
      INTEGER I           ! loop index
      INTEGER J           ! loop index
      INTEGER NCHAR       ! character count

      NCHAR = str_LEN(STR)
      J = 1
      I = 1

      DO WHILE (I.LE.NCHAR)

         IF (STR(I).NE.BLANK) THEN
            STR(J) = STR(I)
            J = J + 1
         END IF

         I = I + 1
      END DO

      CALL str_TERM(J-1, ARB, STR)

      END
