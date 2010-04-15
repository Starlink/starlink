      SUBROUTINE dq_WRPK(SUBVAL, START, NBIT, VALUE)

*+
*
*   Name:
*      SUBROUTINE dq_WRPK
*
*   Description:
*      Pack value into subset of packed data quality value.
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
*      The SUBVAL is converted to a bit pattern and in planted over
*      the VALUE bit pattern as specified by START bit and NBIT.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER SUBVAL       ! subset value to be planted
      INTEGER START        ! start bit
      INTEGER NBIT         ! number of bits to be planted

*   Import-Export:
      INTEGER VALUE        ! value to be modified

*   Local variables:
      INTEGER FIN          ! end bit position
      INTEGER QBITS(8)     ! bit array

      IF (START.GE.1 .AND. START.LE.8) THEN
         FIN = START + NBIT - 1

         IF (FIN.GE.START .AND. FIN.LE.8) THEN
            CALL dq_UNPK(VALUE, 8, QBITS)
            CALL dq_UNPK(SUBVAL, NBIT, QBITS(START))
            CALL dq_PACK(8, QBITS, VALUE)
         END IF
      END IF

      END
