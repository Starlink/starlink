*+  GFX_POLYQ - draw a polyline through points with good quality
      SUBROUTINE GFX_POLYQ(NVAL,N1,N2,X,Y,Q,MASK,STATUS)
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
            IF (BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
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
            IF (X(I).GT.VAL__SMLR.AND.
     :                 BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
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
            IF (Y(I).GT.VAL__SMLR.AND.
     :                   BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
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
            IF (X(I).GT.VAL__SMLR.AND.Y(I).GT.VAL__SMLR.AND.
     :                        BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
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
