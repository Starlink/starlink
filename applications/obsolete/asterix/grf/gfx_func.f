*+  GFX_FUNC - draw a polyline through points
      SUBROUTINE GFX_FUNC(STATUS)
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
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      REAL GFX_FUNC_FUNC
*    Local constants :
      INTEGER NVAL		! number of points
      PARAMETER (NVAL=500)
*    Local variables :
      CHARACTER*12 NAME
      CHARACTER*6 TYPE
      REAL X1,X2
      REAL XW1,XW2,YW1,YW2
      REAL X,Y,XX,YY
      REAL XINC
      REAL PAR(6)
      INTEGER I
      INTEGER STYLE,WIDTH,COLOUR
      LOGICAL FLAG
      LOGICAL OK
      LOGICAL XLOG,YLOG			! whether axes are log
      LOGICAL FIRST
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETL('FUNC_FLAG',OK,FLAG,STATUS)

        IF (OK.AND.FLAG) THEN

          CALL GCB_GETC('FUNC_TYPE',OK,TYPE,STATUS)
          IF (OK) THEN
            CALL CHR_UCASE(TYPE)
            TYPE=TYPE(:4)
          ELSE
            TYPE='POLY'
          ENDIF

          NAME='FUNC_PAR'
          DO I=1,6
            WRITE(NAME(9:9),'(I1)') I
            CALL GCB_GETR(NAME,OK,PAR(I),STATUS)
            IF (.NOT.OK) THEN
              PAR(I)=0.0
            ENDIF
          ENDDO

*  get window bounds
          CALL PGQWIN(XW1,XW2,YW1,YW2)
          X1=MIN(XW1,XW2)
          X2=MAX(XW1,XW2)
          XINC=(X2-X1)/REAL(NVAL-1)

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
          CALL GCB_GETI('FUNC_STYLE',OK,STYLE,STATUS)
          IF (OK) THEN
            CALL PGSLS(STYLE)
          ENDIF
          CALL GCB_GETI('FUNC_WIDTH',OK,WIDTH,STATUS)
          IF (OK) THEN
            CALL PGSLW(WIDTH)
          ENDIF
          CALL GCB_GETI('FUNC_COLOUR',OK,COLOUR,STATUS)
          IF (OK) THEN
            CALL PGSCI(COLOUR)
          ENDIF


*  simple case of lin/lin axes
          IF (.NOT.(XLOG.OR.YLOG)) THEN
            FIRST=.TRUE.
            DO I=1,NVAL
              X=XW1+REAL(I-1)*XINC
              Y=GFX_FUNC_FUNC(TYPE,PAR,X)
              IF (FIRST) THEN
                CALL PGMOVE(X,Y)
                FIRST=.FALSE.
              ELSE
                CALL PGDRAW(X,Y)
              ENDIF
            ENDDO


*  one or both log axes
          ELSEIF (XLOG.AND..NOT.YLOG) THEN
            FIRST=.TRUE.
            DO I=1,NVAL
              X=XW1+REAL(I-1)*XINC
              Y=GFX_FUNC_FUNC(TYPE,PAR,X)
              IF (X.GT.VAL__SMLR) THEN
                XX=LOG10(X)
                YY=Y
                IF (FIRST) THEN
                  CALL PGMOVE(XX,YY)
                  FIRST=.FALSE.
                ELSE
                  CALL PGDRAW(XX,YY)
                ENDIF
              ELSE
                FIRST=.TRUE.
              ENDIF
            ENDDO



          ELSEIF (.NOT.XLOG.AND.YLOG) THEN
            FIRST=.TRUE.
            DO I=1,NVAL
              X=XW1+REAL(I-1)*XINC
              Y=GFX_FUNC_FUNC(TYPE,PAR,X)
              IF (Y.GT.VAL__SMLR) THEN
                XX=X
                YY=LOG10(Y)
                IF (FIRST) THEN
                  CALL PGMOVE(XX,YY)
                  FIRST=.FALSE.
                ELSE
                  CALL PGDRAW(XX,YY)
                ENDIF
              ELSE
                FIRST=.TRUE.
              ENDIF
            ENDDO


          ELSEIF (XLOG.AND.YLOG) THEN
            FIRST=.TRUE.
            DO I=1,NVAL
              X=XW1+REAL(I-1)*XINC
              Y=GFX_FUNC_FUNC(TYPE,PAR,X)
              IF (X.GT.VAL__SMLR.AND.Y.GT.VAL__SMLR) THEN
                XX=LOG10(X)
                YY=LOG10(Y)
                IF (FIRST) THEN
                  CALL PGMOVE(XX,YY)
                  FIRST=.FALSE.
                ELSE
                  CALL PGDRAW(XX,YY)
                ENDIF
              ELSE
                FIRST=.TRUE.
              ENDIF
            ENDDO


          ENDIF

          CALL GCB_SETDEF(STATUS)


        ENDIF

      ENDIF

      END





      REAL FUNCTION GFX_FUNC_FUNC(TYPE,PAR,X)

      CHARACTER*(*) TYPE
      REAL PAR(6)
      REAL X

      IF (TYPE.EQ.'POLY') THEN
        GFX_FUNC_FUNC=PAR(1) + PAR(2)*X + PAR(3)*X**2 + PAR(4)*X**3 +
     :                     PAR(5)*X**4 + PAR(6)*X**5
      ENDIF


      END
