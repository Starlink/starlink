*+  HDX_ERASE - erase all components within structure
      SUBROUTINE HDX_ERASE(LOC,STATUS)
*    Description :
*    Method :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) LOC
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC
      CHARACTER*(DAT__SZNAM) NAME
      INTEGER ICOMP,NCOMP
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL DAT_NCOMP(LOC,NCOMP,STATUS)

        DO ICOMP=NCOMP,1,-1
          CALL DAT_INDEX(LOC,ICOMP,ILOC,STATUS)
          CALL DAT_NAME(ILOC,NAME,STATUS)
          CALL DAT_ANNUL(ILOC,STATUS)
          CALL DAT_ERASE(LOC,NAME,STATUS)
        ENDDO


      ENDIF

      END
