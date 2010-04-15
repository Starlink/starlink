      LOGICAL FUNCTION str_ISALF(C)

*+
*
*   Name:
*      LOGICAL FUNCTION str_ISALF
*
*   Description:
*      Return whether the character falls in the set [A-Za-z_$%].
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

      str_ISALF = str_INDEX('abcdefghijklmnopqrstuvwxyz\\', C).GT.0

      IF (.NOT.str_ISALF) THEN
         str_ISALF = str_INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ\\', C).GT.0

         IF (.NOT.str_ISALF) str_ISALF = str_INDEX('_$%\\', C).GT.0

      END IF

      END
