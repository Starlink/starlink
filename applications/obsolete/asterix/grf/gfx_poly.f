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
      INTEGER I,N
      INTEGER STYLE,WIDTH,COLOUR
      LOGICAL OK
      LOGICAL XLOG,YLOG			! whether axes are log
      LOGICAL FIRST			! first point of a continuous segment
*-

      IF (STATUS.EQ.SAI__OK) THEN

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
          N=N2-N1+1
          CALL PGLINE(N,X(N1),Y(N1))

*  one or both log axes
        ELSEIF (XLOG.AND..NOT.YLOG) THEN
          FIRST=.TRUE.
          DO I=N1,N2
            IF (X(I).GT.VAL__SMLR) THEN
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
            IF (Y(I).GT.VAL__SMLR) THEN
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
            IF (X(I).GT.VAL__SMLR.AND.Y(I).GT.VAL__SMLR) THEN
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
