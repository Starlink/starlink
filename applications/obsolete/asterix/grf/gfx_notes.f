
*+  GFX_NOTES - puts annotations onto plot
      SUBROUTINE GFX_NOTES(STATUS)
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
      CHARACTER*80 TEXT			! text
      CHARACTER*1 JUST
      REAL X,Y				! position on plot
      REAL ANGLE			! orientation angle
      REAL ALIGN				! alignment
      INTEGER FONT			! font number
      REAL SIZE				! text size
      INTEGER BOLD			! boldness factor
      INTEGER COLOUR
      INTEGER I,N
      LOGICAL OK,TOK,XOK,YOK
      LOGICAL XLOG,YLOG			! log axes

*-
      IF (STATUS.EQ.SAI__OK) THEN

*  find out how many notes
        CALL GCB_GETI('NOTE_N',OK,N,STATUS)
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

*  do each one in turn
        DO I=1,N

          CALL GCB_SETDEF(STATUS)

          CALL GCB_GET1C('NOTE_TEXT',I,1,TOK,TEXT,STATUS)
          CALL GCB_GET1R('NOTE_X',I,1,XOK,X,STATUS)
          CALL GCB_GET1R('NOTE_Y',I,1,YOK,Y,STATUS)

          IF (TOK.AND.XOK.AND.YOK) THEN

            CALL GCB_GET1R('NOTE_ANGLE',I,1,OK,ANGLE,STATUS)
            IF (.NOT.OK) THEN
              ANGLE=0.0
            ENDIF
            CALL GCB_GET1C('NOTE_JUST',I,1,OK,JUST,STATUS)
            IF (.NOT.OK) THEN
              ALIGN=0.0
            ELSEIF (JUST.EQ.'L') THEN
              ALIGN=0.0
            ELSEIF (JUST.EQ.'C') THEN
              ALIGN=0.5
            ELSEIF (JUST.EQ.'R') THEN
              ALIGN=1.0
            ENDIF
            CALL GCB_GET1I('NOTE_FONT',I,1,OK,FONT,STATUS)
            IF (OK) THEN
              CALL PGSCF(FONT)
            ENDIF
            CALL GCB_GET1R('NOTE_SIZE',I,1,OK,SIZE,STATUS)
            IF (OK) THEN
              CALL PGSCH(SIZE)
            ENDIF
            CALL GCB_GET1I('NOTE_BOLD',I,1,OK,BOLD,STATUS)
            IF (OK) THEN
              CALL PGSLW(BOLD)
            ENDIF
            CALL GCB_GET1I('NOTE_COLOUR',I,1,OK,COLOUR,STATUS)
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

            CALL PGPTEXT(X,Y,ANGLE,ALIGN,TEXT)

          ENDIF

        ENDDO

        CALL GCB_SETDEF(STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_NOTES',STATUS)
        ENDIF
      ENDIF
      END
