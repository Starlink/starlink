      SUBROUTINE dq_ITOU(IVALUE, BVALUE)

*+
*
*   Name:
*      SUBROUTINE dq_ITOU
*
*   Description:
*      Copy integer DQ into byte retaining ubyte status.
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
*         Conversion to SGP/16 style.
*
*   Method:
*      The data quality stored in the byte BVALUE is copied from
*      the integer IVALUE so that the unsigned nature is retained.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER IVALUE     ! integer value

*   Export:
      BYTE BVALUE        ! byte value

      IF (IVALUE.GT.127) THEN
         BVALUE = IVALUE - 256
      ELSE
         BVALUE = IVALUE
      END IF

      END
