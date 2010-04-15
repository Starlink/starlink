      SUBROUTINE gen_REV(NBYTE, NWORD, WORDS)

*+
*
*   Name:
*      SUBROUTINE gen_REV
*
*   Description:
*      Reverse bytes in arbitrary word (1, 2, 4 only).
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      28-OCT-81
*         AT4 version.
*      Paul Rees          25-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          23-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      Use gen_REV2 or gen_REV4 as appropriate.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NBYTE                ! number of bytes per word
      INTEGER NWORD                ! number of words

*   Import-Export:
      BYTE WORDS(NBYTE, NWORD)     ! multi-byte words
      BYTE temp

      INTEGER i

      IF (NBYTE.EQ.2) THEN
         DO i = 1 , NWORD
            temp = WORDS(1,i)
            WORDS(1,i) = WORDS(2,i)
            WORDS(2,i) = temp
         END DO
!!!         CALL gen_REV2(WORDS, NWORD)
      ELSE IF (NBYTE.EQ.4) THEN
         DO i = 1 , NWORD
            temp = WORDS(1,i)
            WORDS(1,i) = WORDS(4,i)
            WORDS(4,i) = temp
            temp = WORDS(2,i)
            WORDS(2,i) = WORDS(3,i)
            WORDS(3,i) = temp
         END DO
!!!!         CALL gen_REV4(WORDS, NWORD)
      END IF

      END
