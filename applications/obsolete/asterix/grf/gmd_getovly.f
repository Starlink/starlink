*+  GMD_GETOVLY - get overlays for multiple dataset
      SUBROUTINE GMD_GETOVLY(LOC,SET,OVLY,STATUS)
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
      INCLUDE 'GMD_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) LOC	! locator to dataset
*    Import-Export :
*    Export :
      LOGICAL SET
      INTEGER OVLY(GMD__MXPLOT,GMD__MXGRAF)
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) PLOC	! locator to attribute parameter
      LOGICAL THERE			! whether attribute there
      INTEGER DIMS(2)
*-
      SET=.FALSE.

      IF (STATUS.EQ.SAI__OK) THEN


*  see if attribute parameters are present
        CALL DAT_THERE(LOC,'DESIGN',THERE,STATUS)
        IF (THERE) THEN
*  see if value is set
          CALL DAT_FIND(LOC,'DESIGN',PLOC,STATUS)
          CALL DAT_STATE(PLOC,SET,STATUS)
          IF (SET) THEN
*  extract value
            DIMS(1)=GMD__MXPLOT
            DIMS(2)=GMD__MXGRAF
            CALL DAT_GETNI(PLOC,2,DIMS,OVLY,DIMS,STATUS)
            SET=(STATUS.EQ.SAI__OK)
            CALL DAT_ANNUL(PLOC,STATUS)
          ENDIF
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_GETOVLY',STATUS)
        ENDIF
      ENDIF
      END
