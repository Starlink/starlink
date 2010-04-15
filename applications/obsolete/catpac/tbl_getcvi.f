      SUBROUTINE TBL_GETCVI( IN, START, LEN, SZARR, OUT, COLOUT)

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_ERR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'TBL_PAR'

      INTEGER IN(*)
      INTEGER START
      INTEGER LEN
      INTEGER SZARR
      INTEGER OUT(LEN, SZARR)
      INTEGER COLOUT


      INTEGER I
      INTEGER OFF

      OFF = START - 1

      DO I = 1, LEN
          OUT(I, COLOUT) = IN(I + OFF)
      ENDDO


      RETURN
      END
