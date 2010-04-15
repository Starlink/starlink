      INTEGER FUNCTION str_INDEX(STR, C)

*+
*
*   Name:
*      INTEGER FUNCTION str_INDEX
*
*   Description:
*      Find index of character in string.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      01-SEP-81
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      Look for character match in string, return zero if not found.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER ARB           ! arbitrary string length

      PARAMETER (ARB=100)

*   Import:
      BYTE STR(ARB)         ! string to be searched
      BYTE C                ! character to be looked for

*   External references:
      INTEGER str_LEN       ! string length

*   Local variables:
      INTEGER NCHAR         ! character count

      NCHAR = str_LEN(STR)
      str_INDEX = 1

      DO WHILE (str_INDEX.LE.NCHAR)

         IF (STR(str_INDEX).NE.C) THEN
            str_INDEX = str_INDEX + 1
         ELSE
            GO TO 100
         END IF
      END DO

      str_INDEX = 0
 100  CONTINUE

      END
