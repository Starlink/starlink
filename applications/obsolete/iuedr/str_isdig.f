      LOGICAL FUNCTION str_ISDIG(C)

*+
*
*   Name:
*      LOGICAL FUNCTION str_ISDIG
*
*   Description:
*      Return whether the character fills in the set [0-9].
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      25-AUG-81
*         AT4 version.
*      Paul Rees          29-SEP-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE C                ! character to be typed

*   External references:
      INTEGER str_INDEX     ! index of character in string

      str_ISDIG = str_INDEX('0123456789\\', C).GT.0

      END
