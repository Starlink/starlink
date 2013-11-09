*+  GMD_SETLAYOUT - set LAYOUT attributes for multiple dataset
      SUBROUTINE GMD_SETLAYOUT(LOC,SETX,NX,SETY,NY,STATUS)
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
      LOGICAL SETX
      INTEGER NX
      LOGICAL SETY
      INTEGER NY
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) SLOC	! locator to LAYOUT structure
      LOGICAL THERE			! whether attribute there
*-

      IF (STATUS.EQ.SAI__OK) THEN


*  see if container structure there
        CALL DAT_THERE(LOC,'LAYOUT',THERE,STATUS)

        IF (.NOT.THERE) THEN
          CALL DAT_NEW(LOC,'LAYOUT','PLOT_LAYOUT',0,0,STATUS)
        ENDIF

        CALL DAT_FIND(LOC,'LAYOUT',SLOC,STATUS)

        IF (SETX) THEN
          CALL DAT_THERE(SLOC,'NX',THERE,STATUS)
          IF (.NOT.THERE) THEN
            CALL DAT_NEW0I(SLOC,'NX',STATUS)
          ENDIF
          CALL CMP_PUT0I(SLOC,'NX',NX,STATUS)
        ENDIF

        IF (SETY) THEN
          CALL DAT_THERE(SLOC,'NY',THERE,STATUS)
          IF (.NOT.THERE) THEN
            CALL DAT_NEW0I(SLOC,'NY',STATUS)
          ENDIF
          CALL CMP_PUT0I(SLOC,'NY',NY,STATUS)
        ENDIF

        CALL DAT_ANNUL(SLOC,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_SETLAYOUT',STATUS)
        ENDIF
      ENDIF
      END
