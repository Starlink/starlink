*+  GFX_POINT - plot symbol at each point
      SUBROUTINE GFX_POINT(NVAL,N1,N2,X,Y,STATUS)
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
      INTEGER SYMBOL,BOLD,COLOUR
      REAL SIZE
      LOGICAL OK
      LOGICAL XLOG,YLOG		! whether axes are log
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
        CALL GCB_GETI('POINT_SYMBOL',OK,SYMBOL,STATUS)
        IF (.NOT.OK) THEN
          SYMBOL=1
        ENDIF
        CALL GCB_GETI('POINT_BOLD',OK,BOLD,STATUS)
        IF (OK) THEN
          CALL PGSLW(BOLD)
        ENDIF
        CALL GCB_GETI('POINT_COLOUR',OK,COLOUR,STATUS)
        IF (OK) THEN
          CALL PGSCI(COLOUR)
        ENDIF
        CALL GCB_GETR('POINT_SIZE',OK,SIZE,STATUS)
        IF (OK) THEN
          CALL PGSCH(SIZE)
        ENDIF


*  simple case of lin/lin axes
        IF (.NOT.(XLOG.OR.YLOG)) THEN
          N=N2-N1+1
          CALL PGPOINT(N,X(N1),Y(N1),SYMBOL)

*  one or both log axes
        ELSEIF (XLOG.AND..NOT.YLOG) THEN
          DO I=N1,N2
            IF (X(I).GT.VAL__SMLR) THEN
              CALL PGPOINT(1,LOG10(X(I)),Y(I),SYMBOL)
            ENDIF
          ENDDO

        ELSEIF (.NOT.XLOG.AND.YLOG) THEN
          DO I=N1,N2
            IF (Y(I).GT.VAL__SMLR) THEN
              CALL PGPOINT(1,X(I),LOG10(Y(I)),SYMBOL)
            ENDIF
          ENDDO


        ELSEIF (XLOG.AND.YLOG) THEN
          DO I=N1,N2
            IF (X(I).GT.VAL__SMLR.AND.Y(I).GT.VAL__SMLR) THEN
              CALL PGPOINT(1,LOG10(X(I)),LOG10(Y(I)),SYMBOL)
            ENDIF
          ENDDO


        ENDIF

        CALL GCB_SETDEF(STATUS)

      ENDIF

      END
