*+  GFX_POLY - draw a polyline through points
      SUBROUTINE GFX_POLY(NVAL,N1,N2,X,Y,STATUS)
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
      REAL Y(NVAL)		! array of y values
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      REAL X1,X2,Y1,Y2
      REAL XW1,XW2,YW1,YW2
      INTEGER I,N
      INTEGER STYLE,WIDTH,COLOUR
      LOGICAL OK
      LOGICAL XLOG,YLOG			! whether axes are log
      LOGICAL FIRST			! first point of a continuous segment
*    Statement function :
      LOGICAL INBOX
      REAL X,Y
      INBOX(X,Y)=(X.GE.X1.AND.X.LE.X2.AND.Y.GE.Y1.AND.Y.LE.Y2)
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get window bounds so points outside can be filtered out
        CALL PGQWIN(XW1,XW2,YW1,YW2)
        X1=MIN(XW1,XW2)
        X2=MAX(XW1,XW2)
        Y1=MIN(YW1,YW2)
        Y2=MAX(YW1,YW2)


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
        CALL GCB_GETI('POLY_STYLE',OK,STYLE,STATUS)
        IF (OK) THEN
          CALL PGSLS(STYLE)
        ENDIF
        CALL GCB_GETI('POLY_WIDTH',OK,WIDTH,STATUS)
        IF (OK) THEN
          CALL PGSLW(WIDTH)
        ENDIF
        CALL GCB_GETI('POLY_COLOUR',OK,COLOUR,STATUS)
        IF (OK) THEN
          CALL PGSCI(COLOUR)
        ENDIF


*  simple case of lin/lin axes
        IF (.NOT.(XLOG.OR.YLOG)) THEN
          FIRST=.TRUE.
          DO I=N1,N2
            IF (INBOX(X(I),Y(I))) THEN
              IF (FIRST) THEN
                CALL PGMOVE(X(I),Y(I))
                FIRST=.FALSE.
              ELSE
                CALL PGDRAW(X(I),Y(I))
              ENDIF
            ELSE
              FIRST=.TRUE.
            ENDIF
          ENDDO


*  one or both log axes
        ELSEIF (XLOG.AND..NOT.YLOG) THEN
          FIRST=.TRUE.
          DO I=N1,N2
            IF (INBOX(X(I),Y(I)).AND.X(I).GT.VAL__SMLR) THEN
              IF (FIRST) THEN
                CALL PGMOVE(LOG10(X(I)),Y(I))
                FIRST=.FALSE.
              ELSE
                CALL PGDRAW(LOG10(X(I)),Y(I))
              ENDIF
            ELSE
              FIRST=.TRUE.
            ENDIF
          ENDDO

        ELSEIF (.NOT.XLOG.AND.YLOG) THEN
          FIRST=.TRUE.
          DO I=N1,N2
            IF (INBOX(X(I),Y(I)).AND.Y(I).GT.VAL__SMLR) THEN
              IF (FIRST) THEN
                CALL PGMOVE(X(I),LOG10(Y(I)))
                FIRST=.FALSE.
              ELSE
                CALL PGDRAW(X(I),LOG10(Y(I)))
              ENDIF
            ELSE
              FIRST=.TRUE.
            ENDIF
          ENDDO


        ELSEIF (XLOG.AND.YLOG) THEN
          FIRST=.TRUE.
          DO I=N1,N2
            IF (INBOX(X(I),Y(I)).AND.
     :                X(I).GT.VAL__SMLR.AND.Y(I).GT.VAL__SMLR) THEN
              IF (FIRST) THEN
                CALL PGMOVE(LOG10(X(I)),LOG10(Y(I)))
                FIRST=.FALSE.
              ELSE
                CALL PGDRAW(LOG10(X(I)),LOG10(Y(I)))
              ENDIF
            ELSE
              FIRST=.TRUE.
            ENDIF
          ENDDO


        ENDIF

        CALL GCB_SETDEF(STATUS)

      ENDIF

      END
