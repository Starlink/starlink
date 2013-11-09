*+  GMD_QPLOTS - how many plots defined for multiple dataset
      SUBROUTINE GMD_QPLOTS(LOC,NPLOT,STATUS)
*    Description :
*    Method :
*    Authors :
*              (BHVAD::RJV)
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) LOC	! locator to dataset
*    Import-Export :
*    Export :
      INTEGER NPLOT
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      LOGICAL THERE			! whether attribute there
*-

      IF (STATUS.EQ.SAI__OK) THEN


*  see if container structure there
        CALL DAT_THERE(LOC,'PLOTS',THERE,STATUS)

        IF (THERE) THEN
          CALL CMP_SIZE(LOC,'PLOTS',NPLOT,STATUS)
        ELSE
          NPLOT=0
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_QPLOTS',STATUS)
        ENDIF
      ENDIF
      END
