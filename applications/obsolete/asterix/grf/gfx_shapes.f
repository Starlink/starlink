*+  GFX_SHAPES - puts shapes onto plot
      SUBROUTINE GFX_SHAPES(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*         (BHVAD::DJA)
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
*    Functions :
      LOGICAL STR_ABBREV
*    Local Constants :
*    Local variables :
      CHARACTER*20 TYPE			! shape type
      REAL X,Y				! position on plot
      REAL DATA1,DATA2,DATA3
      REAL XP,YP,B,CPA,SPA
      INTEGER COLOUR,WIDTH,STYLE
      INTEGER I,N,ANG
      LOGICAL OK,TOK,XOK,YOK
      LOGICAL D1OK,D2OK,D3OK
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  find out how many shapes
        CALL GCB_GETI('SHAPE_N',OK,N,STATUS)
        IF (.NOT.OK) THEN
          N=0
        ENDIF

*  do each one in turn
        DO I=1,N

          CALL GCB_SETDEF(STATUS)

          CALL GCB_GET1C('SHAPE_TYPE',I,1,TOK,TYPE,STATUS)
          CALL GCB_GET1R('SHAPE_X',I,1,XOK,X,STATUS)
          CALL GCB_GET1R('SHAPE_Y',I,1,YOK,Y,STATUS)

          IF (TOK.AND.XOK.AND.YOK) THEN

            CALL GCB_GET1I('SHAPE_COLOUR',I,1,OK,COLOUR,STATUS)
            IF (OK) THEN
              CALL PGSCI(COLOUR)
            ENDIF
            CALL GCB_GET1I('SHAPE_WIDTH',I,1,OK,WIDTH,STATUS)
            IF (OK) THEN
              CALL PGSLW(WIDTH)
            ENDIF
            CALL GCB_GET1I('SHAPE_STYLE',I,1,OK,STYLE,STATUS)
            IF (OK) THEN
              CALL PGSLS(STYLE)
            ENDIF

*          draw the different shapes
            IF (STR_ABBREV(TYPE,'VECTOR')) THEN

*             data values specifiy x,y offsets
               CALL GCB_GET1R('SHAPE_DATA1',I,1,D1OK,DATA1,STATUS)
               CALL GCB_GET1R('SHAPE_DATA2',I,1,D2OK,DATA2,STATUS)
               IF (D1OK.AND.D2OK) THEN
                 CALL PGMOVE(X,Y)
                 CALL PGDRAW(X+DATA1,Y+DATA2)
               ENDIF

            ELSEIF (STR_ABBREV(TYPE,'CIRCLE')) THEN

*             data value is radius
               CALL GCB_GET1R('SHAPE_DATA1',I,1,D1OK,DATA1,STATUS)

               IF (D1OK) THEN
                 CALL PGMOVE(X+DATA1,Y)
                 DO ANG=1,359
                    CALL PGDRAW(X+COSD(REAL(ANG))*DATA1,
     :                          Y+SIND(REAL(ANG))*DATA1)
                 ENDDO
               ENDIF

            ELSEIF (STR_ABBREV(TYPE,'ELLIPSE')) THEN

*             data values are a,a/b and position angle
               CALL GCB_GET1R('SHAPE_DATA1',I,1,D1OK,DATA1,STATUS)
               CALL GCB_GET1R('SHAPE_DATA2',I,1,D2OK,DATA2,STATUS)
               CALL GCB_GET1R('SHAPE_DATA3',I,1,D3OK,DATA3,STATUS)

               IF (D1OK.AND.D2OK.AND.D3OK) THEN
                 CPA = COSD(DATA3)
                 SPA = SIND(DATA3)
                 B = DATA1 / DATA2
                 CALL PGMOVE(X+DATA1*CPA,Y+DATA1*SPA)
                 DO ANG=1,359
                    XP = DATA1*COSD(REAL(ANG))
                    YP = B*SIND(REAL(ANG))
                    CALL PGDRAW(X+XP*CPA-YP*SPA,Y+XP*SPA+YP*CPA)
                 ENDDO
               ENDIF

            ELSEIF (STR_ABBREV(TYPE,'BOX')) THEN

*             data values specifiy x,y box widths
               CALL GCB_GET1R('SHAPE_DATA1',I,1,D1OK,DATA1,STATUS)
               CALL GCB_GET1R('SHAPE_DATA2',I,1,D2OK,DATA2,STATUS)

               IF (D1OK.AND.D2OK) THEN
                 CALL PGMOVE(X-DATA1/2.0,Y-DATA2/2.0)
                 CALL PGDRAW(X-DATA1/2.0,Y+DATA2/2.0)
                 CALL PGDRAW(X+DATA1/2.0,Y+DATA2/2.0)
                 CALL PGDRAW(X+DATA1/2.0,Y-DATA2/2.0)
                 CALL PGDRAW(X-DATA1/2.0,Y-DATA2/2.0)
               ENDIF

            ELSE
             CALL MSG_SETC('T',TYPE)
             CALL MSG_PRNT('Invalid shape type /^T/')
            END IF
          ENDIF


        ENDDO

        CALL GCB_SETDEF(STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_SHAPES',STATUS)
        ENDIF
      ENDIF
      END
