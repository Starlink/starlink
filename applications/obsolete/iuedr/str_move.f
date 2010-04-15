      SUBROUTINE str_MOVE(STR1, SIZE2, STR2)

*+
*
*   Name:
*      SUBROUTINE str_MOVE
*
*   Description:
*      Copy a string.
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
*      Use str_TERM and str_PLANT.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER ARB          ! arbitrary string length
      PARAMETER (ARB=100)

*   Import:
      BYTE STR1(ARB)       ! input string
      INTEGER SIZE2        ! maximum size of output string

*   Export:
      BYTE STR2(SIZE2)     ! receptor string

      CALL str_TERM(0, SIZE2, STR2)
      CALL str_PLANT(STR1, 1, SIZE2, STR2)

      END
