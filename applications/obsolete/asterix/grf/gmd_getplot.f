*+  GMD_GETPLOT - get design of given plot
      SUBROUTINE GMD_GETPLOT(LOC,PLOT,BASE,OVLY,STATUS)
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
*    Import-Export :
*    Export :
      INTEGER BASE
      CHARACTER*(*) OVLY
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

        IF (THERE) THEN
          CALL CMP_SIZE(LOC,'PLOTS',NPLOT,STATUS)
        ELSE
          NPLOT=0
        ENDIF

        IF (NPLOT.GT.0.AND.PLOT.LE.NPLOT) THEN

          CALL DAT_FIND(LOC,'PLOTS',SLOC,STATUS)
          CALL DAT_CELL(SLOC,1,PLOT,PLOC,STATUS)
          CALL CMP_GET0I(PLOC,'BASE',BASE,STATUS)
          CALL CMP_GET0C(PLOC,'OVLY',OVLY,STATUS)
          CALL DAT_ANNUL(PLOC,STATUS)
          CALL DAT_ANNUL(SLOC,STATUS)

        ELSE
          CALL MSG_PRNT('AST_ERR: invalid plot number')
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_GETPLOT',STATUS)
        ENDIF
      ENDIF
      END
