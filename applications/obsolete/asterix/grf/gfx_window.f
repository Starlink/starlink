
*+  GFX_WINDOW - set limits of plotting window
      SUBROUTINE GFX_WINDOW(X1,X2,Y1,Y2,SCALED,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
*    Import-export :
      REAL X1,X2,Y1,Y2			! plot window in world coords
      LOGICAL SCALED
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL LO,HI
      LOGICAL OK
      LOGICAL XLOG,YLOG
      LOGICAL SCLD
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  if limits set then overide defaults
        CALL GCB_GETR('XAXIS_LO',OK,LO,STATUS)
        IF (OK) THEN
          X1=LO
        ENDIF
        CALL GCB_GETR('XAXIS_HI',OK,HI,STATUS)
        IF (OK) THEN
          X2=HI
        ENDIF
        CALL GCB_GETR('YAXIS_LO',OK,LO,STATUS)
        IF (OK) THEN
          Y1=LO
        ENDIF
        CALL GCB_GETR('YAXIS_HI',OK,HI,STATUS)
        IF (OK) THEN
          Y2=HI
        ENDIF

*  see if axes logarithmic
        CALL GCB_GETL('XAXIS_LOG',OK,XLOG,STATUS)
        IF (.NOT.OK) THEN
          XLOG=.FALSE.
        ENDIF
        CALL GCB_GETL('YAXIS_LOG',OK,YLOG,STATUS)
        IF (.NOT.OK) THEN
          YLOG=.FALSE.
        ENDIF

*  adjust lower limit for log axes
        IF (XLOG.AND.STATUS.EQ.SAI__OK) THEN
*  upper limit too small - can't do anything
          IF (X2.LE.VAL__SMLR) THEN
            CALL MSG_PRNT(
     :                'AST_ERR: x-axis limits too small for log plot')
            STATUS=SAI__ERROR
          ELSE
*  lower limit too small - try to set to something sensible
            IF (X1.LE.VAL__SMLR) THEN
*  try to allow at least 3 decades
              IF (X2.GT.1.1E3) THEN
                X1=0.9
              ELSEIF (X2.GT.VAL__SMLR*1.1E3) THEN
                X1=X2/1.0E3
              ELSE
                X1=VAL__SMLR
              ENDIF
            ENDIF
            X1=LOG10(MAX(X1,VAL__SMLR))
            X2=LOG10(MAX(X2,VAL__SMLR))
          ENDIF
        ENDIF

        IF (YLOG.AND.STATUS.EQ.SAI__OK) THEN
          IF (Y2.LE.VAL__SMLR) THEN
            CALL MSG_PRNT(
     :                'AST_ERR: y-axis limits too small for log plot')
            STATUS=SAI__ERROR
          ELSE
            IF (Y1.LE.VAL__SMLR) THEN
              IF (Y2.GT.1.1E3) THEN
                Y1=0.9
              ELSEIF (Y2.GT.VAL__SMLR*1.1E3) THEN
                Y1=Y2/1.0E3
              ELSE
                Y1=VAL__SMLR
              ENDIF
            ENDIF
            Y1=LOG10(MAX(Y1,VAL__SMLR))
            Y2=LOG10(MAX(Y2,VAL__SMLR))
          ENDIF
        ENDIF


*  see if axes to be scaled equally
        CALL GCB_GETL('AXES_SCALING',OK,SCLD,STATUS)
        IF (OK) THEN
          SCALED=SCLD
        ENDIF

*  set window
        IF (STATUS.EQ.SAI__OK) THEN

          IF (SCALED) THEN
            CALL PGWNAD(X1,X2,Y1,Y2)
          ELSE
            CALL PGWINDOW(X1,X2,Y1,Y2)
          ENDIF


        ELSE
          CALL ERR_REP(' ','from GFX_WINDOW',STATUS)
        ENDIF
      ENDIF
      END
