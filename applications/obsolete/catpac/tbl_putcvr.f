      SUBROUTINE TBL_PUTCVR( IN, START, LEN, OUT)

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_ERR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'TBL_PAR'

      REAL    IN(*)
      INTEGER START
      INTEGER LEN
      REAL    OUT(*)


      INTEGER I
      INTEGER OFF

      OFF = START - 1

      DO I = 1, LEN
          OUT(I+OFF) = IN(I)
      ENDDO

      RETURN
      END
