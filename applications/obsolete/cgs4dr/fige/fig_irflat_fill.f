      SUBROUTINE FIG_IRFLAT_FILL(NX,ND,NS,S,D)
      IMPLICIT NONE
      INTEGER NX,ND,NS,D(NX),S(NX)
      INTEGER IX,ID,IS

      IX=1
      DO IS=1,NS
         DO ID=1,ND
            D(IX)=ID
            S(IX)=IS
            IX=IX+1
         ENDDO
      ENDDO
      END
