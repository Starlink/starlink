      INTEGER FUNCTION dq_AND(VALUE, MASK)

*+
*
*   Name:
*      INTEGER FUNCTION dq_AND
*
*   Description:
*      AND operation on packed integer values (8 bits only).
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
*      The VALUE and MASK are converted to bits patterns.
*      Only bits which are both set 1 are left set.
*      The resulting bit pattern is converted to a value
*      which will be zero if no match is found.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER VALUE        ! value to be mask
      INTEGER MASK         ! mask to be applied

*   Local variables:
      INTEGER I            ! loop index
      INTEGER MBITS(8)     ! mask bit array
      INTEGER VBITS(8)     ! value bit array


      CALL dq_UNPK(VALUE, 8, VBITS)
      CALL dq_UNPK(MASK, 8, MBITS)

      DO I = 1, 8
         VBITS(I) = MOD(VBITS(I)*MBITS(I), 2)
      END DO

      CALL dq_PACK(8, VBITS, dq_AND)

      END
