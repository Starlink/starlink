      SUBROUTINE str_DESO(IVALUE, FIELD)

*+
*
*   Name:
*      SUBROUTINE str_DESO
*
*   Description:
*      Design field size for O-format.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          23-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          22-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The required field width is calculated for an integer value
*      encoded in O-format.
*      Just code it! Then find string length.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER IVALUE      ! integer value

*   Export:
      INTEGER FIELD       ! minimum field size

*   External references:
      INTEGER str_LEN     ! string length

*   Local variables:
      BYTE STR(256)       ! temporary string to hold number

*   Code value
      CALL gen_ITOO(IVALUE, 43, 256, STR)

*   Remove blanks
      CALL str_RMBLK(STR)

*   Find length
      FIELD = str_LEN(STR)

      END
