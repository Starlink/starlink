*+  GFX_MARKS - puts markers onto plot
      SUBROUTINE GFX_MARKS(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*5 STR
      REAL X,Y				! position on plot
      REAL SIZE				! text size
      REAL CH
      INTEGER BOLD			! boldness factor
      INTEGER COLOUR
      INTEGER SYMBOL
      INTEGER I,N
      INTEGER NDIGIT
      LOGICAL OK,SOK,XOK,YOK
      LOGICAL XLOG,YLOG			! log axes
      LOGICAL NUMBER
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  find out how many markers
        CALL GCB_GETI('MARKER_N',OK,N,STATUS)
        IF (.NOT.OK) THEN
          N=0
        ENDIF

*  see if axes are logarithmic
        CALL GCB_GETL('XAXIS_LOG',OK,XLOG,STATUS)
        IF (.NOT.OK) THEN
          XLOG=.FALSE.
        ENDIF
        CALL GCB_GETL('YAXIS_LOG',OK,YLOG,STATUS)
        IF (.NOT.OK) THEN
          YLOG=.FALSE.
        ENDIF

*  see if numbering required
        CALL GCB_GETL('MARKER_NUMBER',OK,NUMBER,STATUS)
        IF (.NOT.OK) THEN
          NUMBER=.FALSE.
        ENDIF

*  get default character size for numbering
        CALL GCB_GETR('DEFAULT_SIZE',OK,CH,STATUS)

*  do each one in turn
        DO I=1,N

          CALL GCB_SETDEF(STATUS)

          CALL GCB_GET1I('MARKER_SYMBOL',I,1,SOK,SYMBOL,STATUS)
          CALL GCB_GET1R('MARKER_X',I,1,XOK,X,STATUS)
          CALL GCB_GET1R('MARKER_Y',I,1,YOK,Y,STATUS)

          IF (SOK.AND.XOK.AND.YOK) THEN

            CALL GCB_GET1R('MARKER_SIZE',I,1,OK,SIZE,STATUS)
            IF (OK) THEN
              CALL PGSCH(SIZE)
            ENDIF
            CALL GCB_GET1I('MARKER_BOLD',I,1,OK,BOLD,STATUS)
            IF (OK) THEN
              CALL PGSLW(BOLD)
            ENDIF
            CALL GCB_GET1I('MARKER_COLOUR',I,1,OK,COLOUR,STATUS)
            IF (OK) THEN
              CALL PGSCI(COLOUR)
            ENDIF

*  convert positions to log(pos)
            IF (XLOG.AND.X.GT.0.0) THEN
              X=LOG10(X)
            ENDIF
            IF (YLOG.AND.Y.GT.0.0) THEN
              Y=LOG10(Y)
            ENDIF

            CALL PGPOINT(1,X,Y,SYMBOL)

            IF (NUMBER) THEN
              STR=' '
              CALL CHR_ITOC( I, STR(2:), NDIGIT )
              CALL PGSCH(CH)
              CALL PGTEXT(X,Y,STR(:NDIGIT+1))
            ENDIF
          ENDIF

        ENDDO

        CALL GCB_SETDEF(STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_MARKS',STATUS)
        ENDIF
      ENDIF
      END
