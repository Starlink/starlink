*+  USI_CLOSE - global tidy up for USI
      SUBROUTINE USI_CLOSE()
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE  'SAE_PAR'
      INCLUDE  'DAT_PAR'
      INCLUDE  'ASTLIB(USI_CMN)'
*    Local constants :
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
*    Local variables :
      INTEGER N
      LOGICAL VALID
      INTEGER ISTAT
*-
      ISTAT=SAI__OK
      DO N=1,USI_NMAX
        IF (DS(N).USED) THEN
          CALL DAT_VALID(DS(N).LOC,VALID,ISTAT)
          IF (VALID) THEN
            CALL DAT_ANNUL(DS(N).LOC,ISTAT)
          ENDIF
          DS(N).LOC=BLANK
          DS(N).USED=.FALSE.
          DS(N).IO=BLANK
        ENDIF
      ENDDO
      END
