*+  GMD_GETLAYOUT - get LAYOUT attributes from multiple dataset
      SUBROUTINE GMD_GETLAYOUT(LOC,XSET,NX,YSET,NY,STATUS)
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
      LOGICAL XSET
      INTEGER NX
      LOGICAL YSET
      INTEGER NY
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) SLOC	! locator to LAYOUT structure
      CHARACTER*(DAT__SZLOC) PLOC	! locator to individual attribute
      LOGICAL THERE			! whether attribute there
      LOGICAL SET			! whether attribute set
*-
      XSET=.FALSE.
      YSET=.FALSE.

      IF (STATUS.EQ.SAI__OK) THEN


*  see if container structure there
        CALL DAT_THERE(LOC,'LAYOUT',THERE,STATUS)

        IF (THERE) THEN
          CALL DAT_FIND(LOC,'LAYOUT',SLOC,STATUS)

*  look for x and y components
          CALL DAT_THERE(SLOC,'NX',THERE,STATUS)
          IF (THERE) THEN
            CALL DAT_FIND(SLOC,'NX',PLOC,STATUS)
            CALL DAT_STATE(PLOC,SET,STATUS)
            IF (SET) THEN
              CALL DAT_GET0I(PLOC,NX,STATUS)
              XSET=(STATUS.EQ.SAI__OK)
              CALL DAT_ANNUL(PLOC,STATUS)
            ENDIF
          ENDIF

          CALL DAT_THERE(SLOC,'NY',THERE,STATUS)
          IF (THERE) THEN
            CALL DAT_FIND(SLOC,'NY',PLOC,STATUS)
            CALL DAT_STATE(PLOC,SET,STATUS)
            IF (SET) THEN
              CALL DAT_GET0I(PLOC,NY,STATUS)
              YSET=(STATUS.EQ.SAI__OK)
              CALL DAT_ANNUL(PLOC,STATUS)
            ENDIF
          ENDIF

          CALL DAT_ANNUL(SLOC,STATUS)

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_GETLAYOUT',STATUS)
        ENDIF
      ENDIF
      END
