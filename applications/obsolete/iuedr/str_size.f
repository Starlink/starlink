      INTEGER FUNCTION str_SIZE(STR)

*+
*
*   Name:
*      INTEGER FUNCTION str_SIZE
*
*   Description:
*      Find size of string including terminator character(s).
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
*         Final conversion to SGP/16.
*
*   Method:
*      Count characters in string until an occurrence of EOS
*      or ESCEOS = (STRESC, STREOS).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE EOS             ! end of string
      BYTE STRESC          ! escape character (backslash)

      PARAMETER (EOS=0, STRESC=92)

      INTEGER ARB          ! arbitrary string length

      PARAMETER (ARB=100)

*   Import:
      BYTE STR(ARB)        ! string to be measured

      str_SIZE = 1

      DO WHILE (STR(str_SIZE).NE.EOS)

         IF (STR(str_SIZE).EQ.STRESC) GO TO 200

         str_SIZE = str_SIZE + 1
      END DO

 200  CONTINUE

      END
