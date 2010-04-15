      SUBROUTINE TBL_PUTCVC( IN, START, LEN, OUT, CLENGTH)

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_ERR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'TBL_PAR'

      CHARACTER*(*) IN(*)
      INTEGER START
      INTEGER LEN
      INTEGER CLENGTH
      CHARACTER*1  OUT(*)


      INTEGER I, J
      INTEGER OFF


      OFF = START - 1

      DO I = 1, LEN
          DO J = 1, CLENGTH
              OUT((I + OFF-1)*CLENGTH+J) = IN(I)(J:J)
          ENDDO
      ENDDO

      RETURN
      END
