*+  GFX_STEPQ - draw a stepped line through points with good quality
      SUBROUTINE GFX_STEPQ(NVAL,N1,N2,X,XW,Y,Q,MASK,STATUS)
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
      INCLUDE 'QUAL_PAR'
*    Import :
      INTEGER NVAL		! number of points
      INTEGER N1,N2		! range of points to plot
      REAL X(NVAL)		! array of x values
      REAL XW(NVAL)		! array of x-width values
      REAL Y(NVAL)		! array of y values
      BYTE Q(NVAL)		! quality array
      BYTE MASK			! quality mask
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
*    Local constants :
*    Local variables :
      REAL X1,X2
      INTEGER I
      INTEGER STYLE,WIDTH,COLOUR
      LOGICAL FIRST			! first point of a continuous segment
      LOGICAL XLOG,YLOG			! whether axes are log
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  see if axes logarithmic
        CALL GCB_GETL('XAXIS_LOG',OK,XLOG,STATUS)
        IF (.NOT.OK) THEN
          XLOG=.FALSE.
        ENDIF
        CALL GCB_GETL('YAXIS_LOG',OK,YLOG,STATUS)
        IF (.NOT.OK) THEN
          YLOG=.FALSE.
        ENDIF

*  set line-style etc.
        CALL GCB_SETDEF(STATUS)
        CALL GCB_GETI('STEP_STYLE',OK,STYLE,STATUS)
        IF (OK) THEN
          CALL PGSLS(STYLE)
        ENDIF
        CALL GCB_GETI('STEP_WIDTH',OK,WIDTH,STATUS)
        IF (OK) THEN
          CALL PGSLW(WIDTH)
        ENDIF
        CALL GCB_GETI('STEP_COLOUR',OK,COLOUR,STATUS)
        IF (OK) THEN
          CALL PGSCI(COLOUR)
        ENDIF

*  simple case of lin/lin axes
        IF (.NOT.(XLOG.OR.YLOG)) THEN
          FIRST=.TRUE.
          X2=X(N1)-XW(N1)/2.0
          DO I=N1,N2
            X1=X2
            X2=X(I)+XW(I)/2.0
            IF (BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
              IF (FIRST) THEN
                CALL PGMOVE(X1,Y(I))
                CALL PGDRAW(X2,Y(I))
                FIRST=.FALSE.
              ELSE
                CALL PGDRAW(X1,Y(I))
                CALL PGDRAW(X2,Y(I))
              ENDIF
            ELSE
              FIRST=.TRUE.
            ENDIF
          ENDDO

*  one or both log axes
        ELSEIF (XLOG.AND..NOT.YLOG) THEN
          FIRST=.TRUE.
          X2=X(N1)-XW(N1)/2.0
          DO I=N1,N2
            X1=X2
            X2=X(I)+XW(I)/2.0
            IF (X1.GT.VAL__SMLR
     :              .AND.BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
              IF (FIRST) THEN
                CALL PGMOVE(LOG10(X1),Y(I))
                CALL PGDRAW(LOG10(X2),Y(I))
                FIRST=.FALSE.
              ELSE
                CALL PGDRAW(LOG10(X1),Y(I))
                CALL PGDRAW(LOG10(X2),Y(I))
              ENDIF
            ELSE
              FIRST=.TRUE.
            ENDIF
          ENDDO

        ELSEIF (.NOT.XLOG.AND.YLOG) THEN
          FIRST=.TRUE.
          X2=X(N1)-XW(N1)/2.0
          DO I=N1,N2
            X1=X2
            X2=X(I)+XW(I)/2.0
            IF (Y(I).GT.VAL__SMLR.AND.
     :                  BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
              IF (FIRST) THEN
                CALL PGMOVE(X1,LOG10(Y(I)))
                CALL PGDRAW(X2,LOG10(Y(I)))
                FIRST=.FALSE.
              ELSE
                CALL PGDRAW(X1,LOG10(Y(I)))
                CALL PGDRAW(X2,LOG10(Y(I)))
              ENDIF
            ELSE
              FIRST=.TRUE.
            ENDIF
          ENDDO


        ELSEIF (XLOG.AND.YLOG) THEN
          FIRST=.TRUE.
          X2=X(N1)-XW(N1)/2.0
          DO I=N1,N2
            X1=X2
            X2=X(I)+XW(I)/2.0
            IF (X1.GT.VAL__SMLR.AND.Y(I).GT.VAL__SMLR.AND.
     :                        BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
              IF (FIRST) THEN
                CALL PGMOVE(LOG10(X1),LOG10(Y(I)))
                CALL PGDRAW(LOG10(X2),LOG10(Y(I)))
                FIRST=.FALSE.
              ELSE
                CALL PGDRAW(LOG10(X1),LOG10(Y(I)))
                CALL PGDRAW(LOG10(X2),LOG10(Y(I)))
              ENDIF
            ELSE
              FIRST=.TRUE.
            ENDIF
          ENDDO


        ENDIF

        CALL GCB_SETDEF(STATUS)

      ENDIF

      END
