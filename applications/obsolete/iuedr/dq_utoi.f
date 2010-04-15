      SUBROUTINE dq_UTOI(BVALUE, IVALUE)

*+
*
*   Name:
*      SUBROUTINE dq_UTOI
*
*   Description:
*      Copy byte DQ into integer retaining ubyte status.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      04-NOV-81
*         AT4 version.
*      Paul Rees          25-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          23-MAY-89     IUEDR Vn. 2.1
*         Conversin to SGP/16 style.
*
*   Method:
*      The data quality stored in the byte BVALUE is copied into
*      the integer IVALUE so that the unsigned nature is retained.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE BVALUE         ! byte value

*   Export:
      INTEGER IVALUE     ! integer value

      IF (BVALUE.LT.0) THEN
         IVALUE = BVALUE + 256
      ELSE
         IVALUE = BVALUE
      END IF

      END
