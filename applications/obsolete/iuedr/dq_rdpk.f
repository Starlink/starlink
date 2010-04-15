      SUBROUTINE dq_RDPK(VALUE, START, NBIT, SUBVAL)

*+
*
*   Name:
*      SUBROUTINE dq_RDPK
*
*   Description:
*      Read value fram subset of packed data quality value.
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
*      The VALUE is converted to a bit pattern and SUBVAL is coded
*      from START to START+NBIT-1.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER VALUE        ! value to be read from
      INTEGER START        ! start bit
      INTEGER NBIT         ! number of bits to be read

*   Import-Export:
      INTEGER SUBVAL       ! subset value coded

*   Local variables:
      INTEGER FIN          ! end bit position
      INTEGER QBITS(8)     ! bit array

      SUBVAL = 0

      IF (START.GE.1 .AND. START.LE.8) THEN
         FIN = START + NBIT - 1

         IF (FIN.GE.START .AND. FIN.LE.8) THEN
            CALL dq_UNPK(VALUE, 8, QBITS)
            CALL dq_PACK(NBIT, QBITS(START), SUBVAL)
         END IF
      END IF

      END
