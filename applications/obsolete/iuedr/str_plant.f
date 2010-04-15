      SUBROUTINE str_PLANT(STR1, START, SIZE2, STR2)

*+
*
*   Name:
*      SUBROUTINE str_PLANT
*
*   Description:
*      Copy the whole of one string into another.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      25-AUG-81
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      If the string is extended by the plant operation, then it is
*      terminated correctly.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER ARB          ! arbitrary string length
      PARAMETER (ARB=100)

*   Import:
      BYTE STR1(ARB)       ! sub-string to be planted

      INTEGER START        ! character position for plant
      INTEGER SIZE2        ! maximum size of string to be changed

*   Import/Export:
      BYTE STR2(SIZE2)     ! string to be changed

*   External references:
      INTEGER str_LEN      ! string length

      CALL str_COPY(STR1, 1, str_LEN(STR1), START, SIZE2, STR2)

      END
