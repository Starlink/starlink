      SUBROUTINE dq_UNPK(DQ, NBIT, BITS)

*+
*
*   Name:
*      SUBROUTINE dq_UNPK
*
*   Description:
*      Unpack data quality bits from integer.
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

*   Global constants:
      LOGICAL FALSE          ! .FALSE.
      LOGICAL TRUE           ! .TRUE.

      PARAMETER (FALSE=.FALSE., TRUE=.TRUE.)

*   Local constants:
      INTEGER MAXBIT         ! machine limit (there is a sign bit)

      PARAMETER (MAXBIT=31)

*   Import:
      INTEGER DQ             ! coded data quality value

*   Export:
      INTEGER NBIT           ! number of bits to be coded
      INTEGER BITS(NBIT)     ! array containing bit values

*   Local variables:
      LOGICAL FIRST          ! switch for local initialisation

      INTEGER BASE(MAXBIT)   ! array of binary bit masks
      INTEGER IBIT           ! bit index
      INTEGER Q              ! temporary residual data quality
      SAVE FIRST, BASE

*   Initialisations:
      DATA FIRST/ .TRUE./

      IF (FIRST) THEN
         BASE(1) = 1

         DO IBIT = 2, MAXBIT
            BASE(IBIT) = BASE(IBIT-1)*2
         END DO

         FIRST = FALSE
      END IF

      IBIT = MIN(MAXBIT, NBIT)
      Q = ABS(DQ)

      DO WHILE (IBIT.GT.0)
         BITS(IBIT) = Q/BASE(IBIT)
         Q = Q - BITS(IBIT)*BASE(IBIT)
         BITS(IBIT) = MOD(BITS(IBIT), 2)
         IBIT = IBIT - 1
      END DO

      END
