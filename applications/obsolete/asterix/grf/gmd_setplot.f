*+  GMD_SETPLOT - set design of given plot
      SUBROUTINE GMD_SETPLOT(LOC,PLOT,BASE,OVLY,STATUS)
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
      INTEGER PLOT
      INTEGER BASE
      CHARACTER*(*) OVLY
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) SLOC,PLOC
      INTEGER NPLOT
      LOGICAL THERE
*-

      IF (STATUS.EQ.SAI__OK) THEN


*  see if container structure there
        CALL DAT_THERE(LOC,'PLOTS',THERE,STATUS)

*  create or resize structure
        IF (THERE) THEN
          CALL DAT_FIND(LOC,'PLOTS',SLOC,STATUS)
          CALL DAT_SIZE(SLOC,NPLOT,STATUS)
          IF (PLOT.GT.NPLOT) THEN
            CALL DAT_ALTER(SLOC,1,PLOT,STATUS)
          ENDIF
        ELSE
          CALL DAT_NEW(LOC,'PLOTS','PLOT_DESIGN',1,PLOT,STATUS)
          CALL DAT_FIND(LOC,'PLOTS',SLOC,STATUS)
        ENDIF

        CALL DAT_CELL(SLOC,1,PLOT,PLOC,STATUS)
        CALL DAT_THERE(PLOC,'BASE',THERE,STATUS)
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0I(PLOC,'BASE',STATUS)
        ENDIF
        CALL CMP_PUT0I(PLOC,'BASE',BASE,STATUS)
        CALL DAT_THERE(PLOC,'OVLY',THERE,STATUS)
        IF (.NOT.THERE) THEN
          CALL DAT_NEWC(PLOC,'OVLY',132,0,0,STATUS)
        ENDIF
        CALL CMP_PUT0C(PLOC,'OVLY',OVLY,STATUS)
        CALL DAT_ANNUL(PLOC,STATUS)
        CALL DAT_ANNUL(SLOC,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_SETPLOT',STATUS)
        ENDIF
      ENDIF
      END
