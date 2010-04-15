      SUBROUTINE str_APPND(STR1, SIZE2, STR2)

*+
*
*   Name:
*      SUBROUTINE str_APPND
*
*   Description:
*      Append one string onto another.
*
*   Authors:
*      Jack Giddings
*
*   Routine History:
*      Jack Giddings      25-AUG-81
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      Find length of STR2 and copy STR1 onto end.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER ARB          ! arbitrary string length
      PARAMETER (ARB=100)

*   Import:
      BYTE STR1(ARB)       ! string to be appended

      INTEGER SIZE2        ! maximum size of final string

*   Import/Export:
      BYTE STR2(SIZE2)     ! string to be extended

*   External references:
      INTEGER str_LEN      ! string length

      CALL str_PLANT(STR1, str_LEN(STR2)+1, SIZE2, STR2)
      END
