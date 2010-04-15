      SUBROUTINE dq_PACK(NBIT, BITS, DQ)

*+
*
*   Name:
*      SUBROUTINE dq_PACK
*
*   Description:
*      Pack data quality bits into integer.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      06-JUL-81
*         AT4 version.
*      Paul Rees          25-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          23-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The "bits" are an array of integer values modulo base 2.
*      Odd bit values are coded as -1, even values are coded as 0 .
*      The maximum number of bits that can be coded is limited by
*      the size of natural integers (32-bit for VAX).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Local constants:
      INTEGER MAXBIT         ! machine limit (there is a sign limit)

      PARAMETER (MAXBIT=31)

*   Import:
      INTEGER NBIT           ! number of bits to be coded
      INTEGER BITS(NBIT)     ! array containing bit values

*   Export:
      INTEGER DQ             ! coded data quality value

*   Local variables:
      INTEGER BASE           ! binary mask
      INTEGER IBIT           ! bit index
      INTEGER MBIT           ! number of bits considered

      MBIT = MIN(MAXBIT, NBIT)
      DQ = 0
      BASE = 1

      DO IBIT = 1, MBIT
         DQ = DQ + MOD(BITS(IBIT), 2)*BASE
         BASE = BASE*2
      END DO

      END
