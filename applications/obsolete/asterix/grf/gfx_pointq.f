*+  GFX_POINTQ - plot symbol at points with good quality
      SUBROUTINE GFX_POINTQ(NVAL,N1,N2,X,Y,Q,MASK,STATUS)
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
      INTEGER SYMBOL,BOLD,COLOUR
      REAL SIZE
      LOGICAL XLOG,YLOG
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

*  get symbol number
        CALL GCB_GETI('POINT_SYMBOL',OK,SYMBOL,STATUS)
        IF (.NOT.OK) THEN
          SYMBOL=1
        ENDIF

*  set size etc.
        CALL GCB_SETDEF(STATUS)
        CALL GCB_GETR('POINT_SIZE',OK,SIZE,STATUS)
        IF (OK) THEN
          CALL PGSCH(SIZE)
        ENDIF
        CALL GCB_GETI('POINT_BOLD',OK,BOLD,STATUS)
        IF (OK) THEN
          CALL PGSLW(BOLD)
        ENDIF
        CALL GCB_GETI('POINT_COLOUR',OK,COLOUR,STATUS)
        IF (OK) THEN
          CALL PGSCI(COLOUR)
        ENDIF

*  simple case of lin/lin axes
        IF (.NOT.(XLOG.OR.YLOG)) THEN
          DO I=N1,N2
            IF (BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
              CALL PGPOINT(1,X(I),Y(I),SYMBOL)
            ENDIF
          ENDDO

*  one or both log axes
        ELSEIF (XLOG.AND..NOT.YLOG) THEN
          DO I=N1,N2
            IF (X(I).GT.VAL__SMLR.AND.
     :                BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
              CALL PGPOINT(1,LOG10(X(I)),Y(I),SYMBOL)
            ENDIF
          ENDDO

        ELSEIF (.NOT.XLOG.AND.YLOG) THEN
          DO I=N1,N2
            IF (Y(I).GT.VAL__SMLR.AND.
     :                BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
              CALL PGMOVE(1,X(I),LOG10(Y(I)),SYMBOL)
            ENDIF
          ENDDO


        ELSEIF (XLOG.AND.YLOG) THEN
          DO I=N1,N2
            IF (X(I).GT.VAL__SMLR.AND.Y(I).GT.VAL__SMLR.AND.
     :                        BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD) THEN
              CALL PGPOINT(1,LOG10(X(I)),LOG10(Y(I)),SYMBOL)
            ENDIF
          ENDDO


        ENDIF

        CALL GCB_SETDEF(STATUS)

      ENDIF

      END
