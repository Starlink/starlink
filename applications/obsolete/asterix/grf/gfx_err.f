*+  GFX_ERR - draw error bars at points
      SUBROUTINE GFX_ERR(NVAL,N1,N2,X,XW,Y,V,STATUS)
*    Description :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER NVAL		! number of points
      INTEGER N1,N2		! range of points to plot
      REAL X(NVAL)		! array of x values
      REAL XW(NVAL)		! array of x-widths
      REAL Y(NVAL)		! array of y values
      REAL V(NVAL)		! array of variances
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      REAL E
      REAL X1,X2,Y1,Y2
      INTEGER I
      INTEGER SHAPE,WIDTH,COLOUR
      LOGICAL OK
      LOGICAL XLOG,YLOG
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_SETDEF(STATUS)

*  see if log axes
        CALL GCB_GETL('XAXIS_LOG',OK,XLOG,STATUS)
        IF (.NOT.OK) THEN
          XLOG=.FALSE.
        ENDIF
        CALL GCB_GETL('YAXIS_LOG',OK,YLOG,STATUS)
        IF (.NOT.OK) THEN
          YLOG=.FALSE.
        ENDIF

*  set plotting attributes
        CALL GCB_SETDEF(STATUS)
        CALL GCB_GETI('ERR_SHAPE',OK,SHAPE,STATUS)
        IF (.NOT.OK) THEN
          SHAPE=1
        ENDIF
        CALL GCB_GETI('ERR_WIDTH',OK,WIDTH,STATUS)
        IF (OK) THEN
          CALL PGSLW(WIDTH)
        ENDIF
        CALL GCB_GETI('ERR_COLOUR',OK,COLOUR,STATUS)
        IF (OK) THEN
          CALL PGSCI(COLOUR)
        ENDIF



*  simple case of lin/lin axes
        IF (.NOT.(XLOG.OR.YLOG)) THEN

          DO I=N1,N2
            X1=X(I)-XW(I)/2.0
            X2=X1+XW(I)
            E=SQRT(MAX(V(I),0.0))
            Y1=Y(I)-E
            Y2=Y(I)+E
            CALL GFX_ERRSYM(X(I),X1,X2,Y(I),Y1,Y2,SHAPE,STATUS)
          ENDDO

*  one or both log axes
        ELSEIF (XLOG.AND..NOT.YLOG) THEN
          DO I=N1,N2
            X1=MAX(X(I)-XW(I)/2.0,VAL__SMLR)
            X2=MAX(X1+XW(I),VAL__SMLR)
            E=SQRT(MAX(V(I),0.0))
            Y1=Y(I)-E
            Y2=Y(I)+E
            IF (X(I).GT.VAL__SMLR) THEN
              CALL GFX_ERRSYM(LOG10(X(I)),LOG10(X1),LOG10(X2),
     :                                  Y(I),Y1,Y2,SHAPE,STATUS)
            ENDIF
          ENDDO

        ELSEIF (.NOT.XLOG.AND.YLOG) THEN
          DO I=N1,N2
            X1=X(I)-XW(I)/2.0
            X2=X1+XW(I)
            E=SQRT(MAX(V(I),0.0))
            Y1=MAX(Y(I)-E,VAL__SMLR)
            Y2=MAX(Y(I)+E,VAL__SMLR)
            IF (Y(I).GT.VAL__SMLR) THEN
              CALL GFX_ERRSYM(X(I),X1,X2,
     :                           LOG10(Y(I)),LOG10(Y1),LOG10(Y2),
     :                                              SHAPE,STATUS)
            ENDIF
          ENDDO


        ELSEIF (XLOG.AND.YLOG) THEN
          DO I=N1,N2
            X1=MAX(X(I)-XW(I)/2.0,VAL__SMLR)
            X2=MAX(X1+XW(I),VAL__SMLR)
            E=SQRT(MAX(V(I),0.0))
            Y1=MAX(Y(I)-E,VAL__SMLR)
            Y2=MAX(Y(I)+E,VAL__SMLR)
            IF (X(I).GT.VAL__SMLR.AND.Y(I).GT.VAL__SMLR) THEN
              CALL GFX_ERRSYM(LOG10(X(I)),LOG10(X1),LOG10(X2),
     :                         LOG10(Y(I)),LOG10(Y1),LOG10(Y2),
     :                                            SHAPE,STATUS)
            ENDIF
          ENDDO


        ENDIF

        CALL GCB_SETDEF(STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_ERR',STATUS)
        ENDIF

      ENDIF

      END
