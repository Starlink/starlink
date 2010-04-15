      SUBROUTINE TBL_GETCVC( IN, START, LEN, SZARR, OUT, COLOUT,
     : CLENGTH)

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_ERR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'TBL_PAR'
      INCLUDE 'CHI_PAR'

      CHARACTER*(1) IN(*)
      INTEGER START
      INTEGER LEN
      INTEGER SZARR
      CHARACTER*(CHI__SZCVAL) OUT(LEN, SZARR)
      INTEGER COLOUT
      INTEGER CLENGTH


      INTEGER I, J
      INTEGER OFF

      OFF = START - 1

      DO I = 1, LEN
          DO J = 1, CLENGTH
                OUT(I, COLOUT)(J:J) = IN((I + OFF-1)*CLENGTH+J)
          ENDDO
      ENDDO

      RETURN
      END
