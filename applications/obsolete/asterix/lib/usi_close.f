*+  USI_CLOSE - global tidy up for USI
      SUBROUTINE USI_CLOSE()
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE  'SAE_PAR'
      INCLUDE  'ADI_PAR'
      INCLUDE  'DAT_PAR'
      INCLUDE  'USI_CMN'
*    Local constants :
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
*    Local variables :
      INTEGER N
      INTEGER ISTAT
*-

      CALL USI0_EXIT()

      ISTAT=SAI__OK
      DO N=1,USI__NMAX
        IF (DS(N).USED) THEN

          CALL ADI_FCLOSE( DS(N).ADI_ID, ISTAT )

          DS(N).ADI_ID = ADI__NULLID
          DS(N).USED=.FALSE.
          DS(N).IO=BLANK

        ENDIF
      ENDDO
      USI_SYINIT = .FALSE.
      END
