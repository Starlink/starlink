*+  USI_ANNUL - tidy dataset
      SUBROUTINE USI_ANNUL(LOC)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE  'SAE_PAR'
      INCLUDE  'DAT_PAR'
      INCLUDE  'USI_CMN'
*    Import:
      CHARACTER*(*) LOC
*    Local constants :
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
*    Local variables :
      INTEGER N
      LOGICAL FOUND
      LOGICAL VALID
      INTEGER ISTAT
*-
      ISTAT=SAI__OK
      N=1
      FOUND=.FALSE.
      DO WHILE (.NOT.FOUND.AND.N.LE.USI__NMAX)
        IF (DS(N).USED) THEN
          IF (LOC.EQ.(DS(N).LOC)) THEN
            FOUND=.TRUE.
          ELSE
            N=N+1
          ENDIF
        ELSE
          N=N+1
        ENDIF
      ENDDO

      IF (FOUND) THEN
        CALL DAT_VALID(DS(N).LOC,VALID,ISTAT)
        IF (VALID) THEN
          CALL DAT_ANNUL(DS(N).LOC,ISTAT)
        ENDIF
        DS(N).LOC=DAT__NOLOC
        DS(N).USED=.FALSE.
        DS(N).IO=BLANK
      ENDIF
      END
