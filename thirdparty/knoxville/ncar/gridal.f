      SUBROUTINE GRIDAL(MAJRX,MINRX,MAJRY,MINRY,IXLAB,IYLAB,IGPH,X,Y)
        EXTERNAL GRIDT
        COMMON /LAB/ SIZX,SIZY,XDEC,YDEC,IXORI
        COMMON /CLAB/ XFMT, YFMT
        COMMON /TICK/ MAJX, MINX, MAJY, MINY
        COMMON /GRIINT/ IGRIMJ, IGRIMN, IGRITX
C
C INTERNAL VARIABLES:
C
C               NERR            Counts error number
C
C               XMIRRO,         Logical flags for mirror-image
C               YMIRRO          plotting.
C
C               LASF(13)        Aspect source flag table as used by GKS.
C
C               OPLASF          Stores value of polyline color ASF on
C                               entry to GRIDAL
C
C               OTXASF          Stores value of text color ASF on
C                               entry to GRIDAL
C
C               OCOLI           Color index on entry to GRIDAL
C
C               OTXCOL          Text color index on entry to GRIDAL
C
C               ICNT            Normalization transformation number in
C                               effect on entry to GRIDAL
C
C               OWIND(4)        Window limits in world coordinates            TO
C                               on entry to GRIDAL
C
C               WIND(4)         Same as OWIND(4) except if log scaling
C                               then LOG10(OWIND)
C
C               XI              LOG10(X), if log scaling
C
C               YI              LOG10(Y), if log scaling
C
C               LOGVAL          Linear or log scaling
C                               1 = X linear, Y linear
C                               2 = X linear, Y log
C                               3 = X log, Y linear
C                               4 = X log, Y log
C
C               VIEW(4)         Viewport limits in NDC prior to
C                               expansion for labelling
C
C               XRANGE          Total range in X direction in world
C                               coordinates prior to expansion for
C                               labelling.
C
C               YRANGE          Total range in Y direction in world
C                               coordinates prior to expansion for
C                               labelling.
C
C               OLDCH           Character height on entry to GRIDAL
C
C               CHUPX,CHUPY     Character up vector values on entry
C
C               OLDALH,OLDALV   Text alignment values on entry
C                               (horizontal and vertical)
C
C               NWIND(4)        Window limits in world coordinates
C                               after expansion
C
C               XRNEW           Range in X direction in world
C                               coordinates, after expansion
C
C               YRNEW           Range in Y direction in world
C                               coordinates, after expansion
C
C               TICMIN          Length of minor ticks in world
C                               coordinates
C
C               TICMAJ          Length of major ticks in world
C                               coordinates
C
C               LOGMIN          .TRUE.  if log scaling is in effect and
C                               minor tick marks or grids are desired
C
C               XINTM           Interval between major X-axis
C                               ticks/grids in world coordinates
C
C               YINTM           Interval between major Y-axis
C                               ticks/grids in world coordinates
C
C               XINT            Interval between minor X-axis
C                               ticks/grids in world coordinates
C
C               XNUM            Total number of X-axis ticks/grids
C                               with linear scaling
C
C               YNUM            Total number of Y-axis ticks/grids
C                               with linear scaling
C
C               XCUR            A tick/grid is drawn at this position
C                               if log scaling is in effect.
C
C               YCUR            A tick/grid is drawn at this position
C                               if log scaling is in effect.
C
C               MINCNT          Number of minor divisions per major
C
C               CURMAJ          If LOGMIN=.TRUE., then this is the
C                               current major tick/grid position
C
C               NEXTMAJ         If LOGMIN=.TRUE., then this is the next
C                               major tick/grid position
C
C               LGRID           .TRUE. if grids are to be drawn on the
C                               current axis (opposed to ticks)
C
C               START           If drawing ticks/grids on X-axis:
C                                       Y-coord of origin of each line;
C                               If drawing ticks/grids on Y-axis:
C                                       X-coord of origin of each line
C
C               XPOS            If linear scaling, keeps track of X-axis
C                               position for current tick/grid
C
C               YPOS            If linear scaling, keeps track of Y-axis
C                               position for current tick/grid
C
C               PY(2)           2 Y-coordinates for line to be drawn
C                               via GKS routine GPL
C
C               PX(2)           2 X-coordinates for line to be drawn
C                               via GKS routine GPL
C
C               TICEND          End of minor tick line in world
C                               coordinates
C
C               TICBIG          End of major tick line in world
C                               coordinates
C
C               XDEC            Length in world coordinates from
C                               X-axis to label
C
C               YDEC            Length in world coordinates from
C                               Y-axis to label
C
C               XLAB,YLAB       If labelling X-axis, Y-coordinate for
C                               for text position;
C                               if labelling Y-axis, X-coordinate for
C                               text position.
C
C
C
        CHARACTER*8 XFMT,YFMT
        REAL WIND(4), VIEW(4), PX(2), PY(2), NWIND(4), OWIND(4)
        REAL MAJX, MINX, MAJY, MINY
        INTEGER TCOUNT, XTNUM, YTNUM, FIRST, LAST
        INTEGER OPLASF, OTXASF, LASF(13), OCOLI, OTEXCI, OLDALH ,OLDALV
        LOGICAL LGRID,LOGMIN
        LOGICAL XMIRRO,YMIRRO
        REAL MAJDIV, NEXTMA
        CHARACTER*15 LABEL
C
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
        CALL Q8QST4('GRAPHX','GRIDAL','GRIDAL','VERSION 01')
        XRNEW = 0.
        YRNEW = 0.
C
C  INTIALIZE ERROR COUNT
C
      NERR = 0
C
C  CHECK FOR BAD VALUES OF IGPH
C
        IF (IGPH.LT.0.OR.IGPH.EQ.3.OR.IGPH.EQ.7.OR.IGPH.GT.10) THEN
            NERR = NERR + 1
            CALL SETER(' GRIDAL--INVALID IGPH VALUE',NERR,2)
        ENDIF
C
C  GET STANDARD ERROR MESSAGE UNIT
C
        IERUNT = I1MACH(4)
        XMIRRO = .FALSE.
        YMIRRO = .FALSE.
C
C  SET POLYLINE COLOR ASF TO INDIVIDUAL
C
        CALL GQASF(IERR,LASF)
        OPLASF = LASF(3)
        LASF(3) = 1
        OTXASF = LASF(10)
        LASF(10) = 1
        CALL GSASF(LASF)
C
C  INQUIRE CURRENT POLYLINE COLOR
C
        CALL GQPLCI(IERR,OCOLI)
C
C  SET POLYLINE COLOR TO VALUE IN COMMON
C
        CALL GSPLCI(IGRIMJ)
C
C  INQUIRE CURRENT NORMALIZATION NUMBER
C
        CALL GQCNTN(IERR,ICNT)
C
C  INQUIRE CURRENT WINDOW AND VIEWPORT LIMITS
C
        CALL GQNT(ICNT,IERR,WIND,VIEW)
C
C  DETERMINE IF MIRROR-IMAGING EXISTS
C
        IF (WIND(1) .GT. WIND(2)) THEN
            XMIRRO = .TRUE.
        ENDIF
        IF (WIND(3) .GT. WIND(4)) THEN
            YMIRRO = .TRUE.
        ENDIF
C
C  IF IGPH=10 CHECK FOR X(Y) VALUES IN RANGE (IF NOT,CHANGE TO DEFAULT)
C
      IF (IGPH .EQ. 10) THEN
            XI = X
            YI = Y
            IF (((XI .LT. WIND(1) .OR. XI .GT. WIND(2)) .AND. .NOT.
     1  XMIRRO) .OR.(XMIRRO.AND.(XI.GT.WIND(1).OR.XI.LT.WIND(2))))
     2      THEN
                NERR = NERR + 1
              CALL SETER(' GRIDAL--X VALUE OUT OF WINDOW RANGE',NERR,1)
                WRITE(IERUNT,1001)NERR
 1001 FORMAT(' ERROR',I3,' IN GRIDAL--X VALUE OUT OF WINDOW RANGE!')
                CALL ERROF
                XI = WIND(1)
            ENDIF
            IF (((YI .LT. WIND(3) .OR. YI .GT. WIND(4)) .AND. .NOT.
     1   YMIRRO).OR.(YMIRRO.AND.(YI.GT.WIND(3).OR.YI.LT.WIND(4))))
     2      THEN
                NERR = NERR + 1
              CALL SETER(' GRIDAL--Y VALUE OUT OF WINDOW RANGE',NERR,1)
                WRITE(IERUNT,1002)NERR
 1002 FORMAT(' ERROR',I3,' IN GRIDAL--Y VALUE OUT OF WINDOW RANGE!')
                CALL ERROF
                YI = WIND(3)
            ENDIF
      ENDIF
C
C  STORE WINDOW VALUES
C
        DO 10 I = 1,4
 10       OWIND(I) = WIND(I)
        MX = MAJRX
        MY = MAJRY
C
C  LOG OR LINEAR SCALING ?
C           1 = X LINEAR, Y LINEAR
C           2 = X LINEAR, Y LOG
C           3 = X LOG, Y LINEAR
C           4 = X LOG, Y LOG
C
        CALL GETUSV('LS',LOGVAL)
C
            IF (LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 3) THEN
                  IF (MX .LT. 1) MX = 1
                  IF (WIND(1) .LE. 0.) THEN
                      NERR = NERR + 1
                      CALL SETER(' GRIDAL--NON-POSITIVE WINDOW BOUNDARY
     1 WITH LOG SCALING',NERR,2)
                  ELSE
                      WIND(1) = LOG10(WIND(1))
                  ENDIF
                  IF (WIND(2) .LE. 0.) THEN
                      NERR = NERR + 1
                      CALL SETER(' GRIDAL--NON-POSITIVE WINDOW BOUNDARY
     1 WITH LOG SCALING',NERR,2)
                  ELSE
                      WIND(2) = LOG10(WIND(2))
                  ENDIF
                  IF (IGPH .EQ. 10) THEN
                      XI = LOG10(XI)
                  ENDIF
            ENDIF
          IF(LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 2) THEN
                  IF (MY .LT. 1) MY = 1
                  IF (WIND(3) .LE. 0.) THEN
                      NERR = NERR + 1
                      CALL SETER(' GRIDAL--NON-POSITIVE WINDOW BOUNDARY
     1 WITH LOG SCALING',NERR,2)
                  ELSE
                      WIND(3) = LOG10(WIND(3))
                  ENDIF
                  IF (WIND(4) .LE. 0.) THEN
                      NERR = NERR + 1
                      CALL SETER(' GRIDAL--NON-POSITIVE WINDOW BOUNDARY
     1 WITH LOG SCALING',NERR,2)
                  ELSE
                      WIND(4) = LOG10(WIND(4))
                  ENDIF
                  IF (IGPH .EQ. 10) THEN
                      YI = LOG10(YI)
                  ENDIF
          ENDIF
C
C  RESET WINDOW LIMITS TO REFLECT LOG SCALING
C
      CALL GSWN(1,WIND(1),WIND(2),WIND(3),WIND(4))
      CALL GSVP(1,VIEW(1),VIEW(2),VIEW(3),VIEW(4))
      CALL GSELNT(1)
C
C  CALCULATE X AND Y WORLD COORDINATE RANGES
C
      XRANGE = WIND(2) - WIND(1)
      YRANGE = WIND(4) - WIND(3)
C
C  IF LABELS, INQUIRE AND SAVE TEXT ATTRIBUTES
C
        IF (IXLAB .EQ. 1 .OR. IYLAB .EQ. 1) THEN
          CALL GQCHH(IERR,OLDCHH)
          CALL GQCHUP(IERR,CHUPX,CHUPY)
          CALL GQTXAL(IERR,OLDALH,OLDALV)
          CALL GQTXCI (IERR,OTEXCI)
          CALL GSTXCI (IGRITX)
C
C  EXPAND WINDOW AND VIEWPORT FOR LABELS AND CALCULATE NEW
C  X AND Y WORLD COORD RANGES
C
          CALL EXPAND(NWIND)
          XRNEW = NWIND(2) - NWIND(1)
          YRNEW = NWIND(4) - NWIND(3)
C
C  SET CHARACTER HEIGHT (1% OF YRANGE)
C
          CHARH = SIZX * YRNEW
          IF (YMIRRO) THEN
              CHARH = -CHARH
          ENDIF
          CALL GSCHH(CHARH)
        ENDIF
C
      IF (IGPH .EQ. 0) GOTO 50
C
C  CALCULATE TIC LENGTH
C
C  IF NO LABELS AND TICK4 (OR TICKS) WERE NOT CALLED
C
      IF (MAJX .EQ. 0.) THEN
            MAJX = .013
            MINX = .007
            TICMIN = MINX * YRANGE
            TICMAJ = MAJX * YRANGE
        ELSE
C
C  EXPAND WINDOW IF NOT ALREADY EXPANDED
C  (IF LABMOD WAS NOT CALLED BUT, TICK4(S) WAS CALLED)
C
        IF (IXLAB .NE. 1 .AND. IYLAB .NE. 1) THEN
            CALL EXPAND (NWIND)
            XRNEW = NWIND(2) - NWIND(1)
            YRNEW = NWIND(4) - NWIND(3)
        ENDIF
            TICMIN = MINX * YRNEW
            TICMAJ = MAJX * YRNEW
        ENDIF
C
C  **** X-AXIS TICS/GRIDS AND LABELS ****
C
C  CALCULATE TIC/GRID INTERVALS ON X AXES
C
  50  IF (IXLAB .EQ. -1) GOTO 175
        MINCNT = MINRX
        IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 2) THEN
            LOGMIN = .FALSE.
            XINTM = XRANGE/MX
            XINT = XINTM
            IF (MINCNT .GT. 1) THEN
                XINT = XINT/MINCNT
            ENDIF
C
C  CALCULATE TOTAL NUMBER OF TICS/GRIDS ON AXIS
C
            XTNUM = MX * MINCNT
            IF (MINCNT .EQ. 0) XTNUM = MX
        ELSE
            XTNUM = 50
            XCUR = OWIND(1)
            MAJDIV = 10 ** MX
            IF (MINCNT .LE. 10 .AND. MX .LE. 1) THEN
                  LOGMIN = .TRUE.
                  CURMAJ = XCUR
                  NEXTMA = XCUR * MAJDIV
                  XINT = (NEXTMA - CURMAJ) / 9.
                  MINCNT = 9
            ELSE
                  LOGMIN = .FALSE.
                  MINCNT = 1
            ENDIF
      ENDIF
C
C
        LGRID = .FALSE.
        LOOP = 1
C
C  DETERMINE ORIGIN OF TICK/GRID LINES (Y COORD)
C
        IF (IGPH .NE. 10) THEN
            START = WIND(3)
        ELSE
            START = YI
        ENDIF
C
C
        XPOS = WIND(1)
        PY(1) = START
        TICEND = START + TICMIN
        TICBIG = START + TICMAJ
C
        PX(1) = XPOS
        PX(2) = PX(1)
C
C  DRAW LEFT-MOST TICK ON X-AXIS (IF IGPH = 10
C  AND INTERSECTION OF AXES IS NOT AT BOTTOM LEFT
C  OF WINDOW)
C
      IF (IGPH .EQ. 10) THEN
            IF (XI .NE. WIND(1)) THEN
                PY(2) = TICBIG
                CALL GPL(2,PX,PY)
          ENDIF
C
C  DRAW X-AXIS FOR IGPH = 10
C
            PX(2) = WIND(2)
            PY(2) = PY(1)
            CALL GPL(2,PX,PY)
            PX(2) = PX(1)
        ELSE
C
C  DRAW Y-AXIS FOR ANY OTHER IGPH (FIRST TICK)
C
            PY(2) = WIND(4)
            CALL GPL(2,PX,PY)
        ENDIF
C
C  TICKS OR GRIDS ?
C
        IF (IGPH .EQ. 0 .OR. IGPH .EQ. 1 .OR. IGPH .EQ.2) THEN
            PY(2) = WIND(4)
            LGRID = .TRUE.
        ELSE
            PY(2) = TICEND
        ENDIF
C
        IF (IXLAB .EQ. 1) THEN
C
C  IF VERTICAL X-AXIS LABEL ORIENATION THEN SET CHAR UP VECTOR
C  TO BE VERTICAL AND TEXT ALIGNMENT TO (RIGHT,HALF)
C  OTHERWISE TO (CENTER,TOP)
C
          IF (YMIRRO) THEN
              IF (IXORI .EQ. 1) THEN
                  CALL GSCHUP(1.,0.)
                  CALL GSTXAL(3,3)
              ELSE
                  CALL GSCHUP(0.,-1.)
                  CALL GSTXAL(2,1)
              ENDIF
          ELSE
              IF (IXORI .EQ. 1) THEN
                  CALL GSCHUP(-1.,0.)
                  CALL GSTXAL(3,3)
              ELSE
                  CALL GSTXAL(2,1)
              ENDIF
          ENDIF
          IF (XDEC .NE. 0. .AND. XDEC .NE. 1.) THEN
                DEC = XDEC * YRNEW
          ELSE
                DEC = .02 * YRNEW
          ENDIF
          IF (XDEC .NE. 1) THEN
              XLAB = START - DEC
          ELSE
                XLAB = START
C
C  IF LABELS ON TOP OF X-AXIS
C  SET TEXT ALIGNMENT TO (LEFT,HALF) IF VERTICAL X-AXIS LABELS
C  OTHERWISE TO (CENTER,BASE)
C
                IF (IXORI .EQ. 1) THEN
                        CALL GSTXAL(1,3)
                ELSE
                        CALL GSTXAL(2,4)
                ENDIF
          ENDIF
          IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 2) THEN
              WRITE(LABEL,XFMT)XPOS
          ELSE
              WRITE(LABEL,XFMT)XCUR
          ENDIF
          CALL CHSTR(LABEL,FIRST,LAST)
          CALL GTX (XPOS,XLAB,LABEL(FIRST:LAST))
        ENDIF
C
C
 80   TCOUNT = 1
C
        DO 100 I = 1,XTNUM
            IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 2) THEN
                XPOS = XPOS + XINT
            ELSE
              IF (.NOT. LOGMIN) THEN
                        XCUR = XCUR * MAJDIV
                  ELSE
                        IF (TCOUNT .NE. MINCNT) THEN
                            XCUR = XCUR + XINT
                        ELSE
                            XCUR = XCUR + XINT
                            CURMAJ = NEXTMA
                            NEXTMA = CURMAJ * MAJDIV
                            XINT = (NEXTMA - CURMAJ) / 9.
                      ENDIF
                ENDIF
                IF (XCUR .GT. OWIND(2)) THEN
                    XPOS = WIND(2)
                ELSE
                    XPOS = LOG10(XCUR)
                ENDIF
            ENDIF
C
            PX(1) = XPOS
            PX(2) = XPOS
C
C  IF IGPH = 0,1,2,4,5,8 OR 9 AND XPOS=RIGHT AXIS THEN
C  DRAW AXIS, ELSEIF IGPH = 6 OR 10 DRAW TIC AND LABEL
C
            IF ((((LOGVAL .EQ. 1.OR.LOGVAL.EQ.2) .AND. (I .EQ. XTNUM))
     1     .OR.((LOGVAL .EQ.4 .OR.LOGVAL .EQ.3).AND.XCUR.GE.OWIND(2)))
     2       .AND.(IGPH.NE.6.AND.IGPH.NE.10)) THEN
                  IF (LOOP .EQ. 1) THEN
                        PY(2) = WIND(4)
                        CALL GPL(2,PX,PY)
                        IF (IXLAB .EQ. 1) THEN
                        IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 2) THEN
                        WRITE(LABEL,XFMT)XPOS
                        ELSE
                            IF (XCUR .GT. OWIND(2)) THEN
                                  GOTO 101
                            ELSE
                                  WRITE(LABEL,XFMT)XCUR
                            ENDIF
                        ENDIF
                            CALL CHSTR(LABEL,FIRST,LAST)
                            CALL GTX (XPOS,XLAB,LABEL(FIRST:LAST))
                        ENDIF
                  ENDIF
              GOTO 101
            ENDIF
        IF ((LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 3).AND. XCUR .GT. OWIND(2))
     1    GOTO 101
C
C  MINOR TIC/GRID ?
C
            IF (TCOUNT .NE. MINCNT .AND. MINCNT .NE. 0) THEN
                  IF (LGRID) THEN
                        CALL GSPLCI(IGRIMN)
                  ENDIF
                  CALL GPL(2,PX,PY)
                  IF (LGRID) THEN
                        CALL GSPLCI(IGRIMJ)
                  ENDIF
                  TCOUNT = TCOUNT + 1
C
C  MAJOR TIC/GRID
C
            ELSE
                  IF (.NOT. LGRID) THEN
                      PY(2) = TICBIG
                  ENDIF
                  CALL GPL(2,PX,PY)
C
C  LABEL
C
                  IF (IXLAB .EQ. 1 .AND. LOOP .EQ. 1) THEN
                    IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 2) THEN
                        WRITE(LABEL,XFMT)XPOS
                    ELSE
                        WRITE(LABEL,XFMT)XCUR
                    ENDIF
                        CALL CHSTR(LABEL,FIRST,LAST)
                        CALL GTX (XPOS,XLAB,LABEL(FIRST:LAST))
                  ENDIF
                  TCOUNT = 1
                  IF (.NOT. LGRID) THEN
                      PY(2) = TICEND
                  ENDIF
          ENDIF
        IF ((LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 3).AND. XCUR .EQ. OWIND(2))
     1    GOTO 101
 100  CONTINUE
 101  CONTINUE
C
C
C  TOP X-AXIS TICKS ?
C
        IF (LOOP .EQ. 1 .AND.(IGPH .EQ. 4 .OR. IGPH .EQ. 5 .OR. IGPH
     1    .EQ. 6)) THEN
            START = WIND(4)
            TICEND = START - TICMIN
            TICBIG = START - TICMAJ
            PY(1) = START
            PY(2) = TICEND
            XPOS = WIND(1)
            LOOP = 2
            IF (LOGVAL .EQ. 4 .OR. LOGVAL .EQ.3) THEN
                XCUR = OWIND(1)
                IF (LOGMIN) THEN
                      CURMAJ = XCUR
                      NEXTMA = XCUR * MAJDIV
                      XINT = (NEXTMA - CURMAJ) / 9.
                ENDIF
            ENDIF
            GOTO 80
        ENDIF
C
C  **** Y-AXIS TICS/GRIDS AND LABELS ****
C
 175  IF (IYLAB .EQ. -1) GOTO 999
C
C  CALCULATE Y-AXIS TICS
C
        MINCNT = MINRY
        IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 3) THEN
            LOGMIN = .FALSE.
            YINTM = YRANGE/MY
            YINT = YINTM
            IF (MINCNT .GT. 1) THEN
              YINT = YINT/MINCNT
            ENDIF
            YTNUM = MY * MINCNT
            IF (MINCNT .EQ. 0) YTNUM = MY
        ELSE
            YTNUM = 50
            YCUR = OWIND(3)
            MAJDIV = 10 ** MY
            IF (MINCNT .LE. 10 .AND. MY .LE. 1) THEN
                  LOGMIN = .TRUE.
                  CURMAJ = YCUR
                  NEXTMA = YCUR * MAJDIV
                  YINT = (NEXTMA - CURMAJ) / 9.
                  MINCNT = 9
            ELSE
                  LOGMIN = .FALSE.
                  MINCNT = 1
            ENDIF
      ENDIF
C
C
        LGRID = .FALSE.
        LOOP = 1
C
C  DETERMINE ORIGIN OF TICK/GRID LINES (X COORD)
C
        IF (IGPH .NE. 10) THEN
            START = WIND(1)
        ELSE
            START = XI
        ENDIF
C
C
        YPOS = WIND(3)
        PX(1) = START
C
C  DETERMINE Y-AXIS TICK LENGTHS
C
        IF (MAJY .EQ. 0.) THEN
            MAJY = .013
            MINY = .007
        ENDIF
        IF (XRNEW .EQ. 0.) THEN
            TICMIN = MINY * XRANGE
            TICMAJ = MAJY * XRANGE
        ELSE
            TICMIN = MINY * XRNEW
            TICMAJ = MAJY * XRNEW
        ENDIF
        TICEND = START + TICMIN
        TICBIG = START + TICMAJ
C
        PY(1) = YPOS
        PY(2) = PY(1)
C
C  DRAW BOTTOM-MOST TICK ON Y-AXIS IF (IGPH = 10
C  AND INTERSECTION OF AXES IS NOT AT BOTTOM LEFT
C  OF WINDOW)
C
        IF (IGPH .EQ. 10) THEN
            IF (YI .NE. WIND(3)) THEN
                PX(2) = TICBIG
                CALL GPL(2,PX,PY)
          ENDIF
C
C  DRAW Y-AXIS FOR IGPH = 10
C
            PY(2) = WIND(4)
            PX(2) = PX(1)
            CALL GPL(2,PX,PY)
            PY(2) = PY(1)
        ELSE
C
C  DRAW X-AXIS FOR ANY OTHER IGPH (FIRST TICK)
C
            PX(2) = WIND(2)
            CALL GPL(2,PX,PY)
        ENDIF
C
C  GRID OR TICS ?
C
        IF ((IGPH .EQ. 0 .OR. IGPH .EQ. 4).OR. IGPH .EQ. 8) THEN
            PX(2) = WIND(2)
            LGRID = .TRUE.
        ELSE
            PX(2) = TICEND
        ENDIF
C
C  SET TEXT ATTRIBUTES IF Y-AXIS IS TO BE LABELLED
C
      IF (IYLAB .EQ. 1) THEN
            IF (IXORI .EQ. 1) THEN
                IF (YMIRRO) THEN
                    CALL GSCHUP(0.,-1.)
                ELSE
                    CALL GSCHUP(0.,1.)
                ENDIF
            ENDIF
C
C  SET TEXT ALIGNMENT TO (RIGHT,HALF)
C
            CALL GSTXAL(3,3)
C
C  RECALCULATE CHARACTER HIEGHT IF Y-AXIS LABEL ARE DIFFERENT SIZE
C  FROM X-AXIS LABELS
C
            CHARH = SIZY * YRNEW
            IF (YMIRRO) THEN
                CHARH = -CHARH
            ENDIF
            CALL GSCHH(CHARH)
            IF (YDEC .NE. 0. .AND. YDEC .NE. 1.) THEN
                  DEC = YDEC * XRNEW
            ELSE
                  DEC = .02 * XRNEW
            ENDIF
            IF (YDEC .NE. 1) THEN
                  YLAB = START - DEC
            ELSE
                  YLAB = START
C
C  SET TEXT ALIGNMENT TO (LEFT,HALF) IF LABELLING ON RIGHT OF Y-AXIS
C
                  CALL GSTXAL(1,3)
            ENDIF
            IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ.3) THEN
                WRITE(LABEL,YFMT)YPOS
            ELSE
                WRITE(LABEL,YFMT)YCUR
            ENDIF
            CALL CHSTR(LABEL,FIRST,LAST)
            CALL GTX(YLAB,YPOS,LABEL(FIRST:LAST))
        ENDIF
C
C
 180  TCOUNT = 1
C
        DO 200 I = 1,YTNUM
            IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ. 3) THEN
                YPOS = YPOS + YINT
            ELSE
              IF (.NOT. LOGMIN) THEN
                  YCUR = YCUR * MAJDIV
              ELSE
                  IF (TCOUNT .NE. MINCNT) THEN
                      YCUR = YCUR + YINT
                  ELSE
                      YCUR = YCUR + YINT
                      CURMAJ = NEXTMA
                      NEXTMA = CURMAJ * MAJDIV
                      YINT = (NEXTMA - CURMAJ) / 9.
                  ENDIF
              ENDIF
              IF (YCUR .GT. OWIND(4)) THEN
                  YPOS = WIND(4)
              ELSE
                  YPOS = LOG10(YCUR)
              ENDIF
            ENDIF
C
            PY(1) = YPOS
            PY(2) = YPOS
C
C  IF IGPH = 0,1,2,4,5,6 OR 8 AND YPOS = TOP AXIS THEN
C  DRAW AXIS, ELSEIF IGPH = 9 OR 10 DRAW TIC
C
            IF ((((LOGVAL .EQ. 1.OR.LOGVAL.EQ.3) .AND. (I .EQ. YTNUM))
     1     .OR.((LOGVAL .EQ.4 .OR.LOGVAL .EQ.2).AND.YCUR.GE.OWIND(4)))
     2       .AND.(IGPH.NE.9.AND.IGPH.NE.10)) THEN
                  IF (LOOP .EQ. 1) THEN
                        PX(2) = WIND(2)
                        CALL GPL(2,PX,PY)
                        IF (IYLAB .EQ. 1) THEN
                        IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ.3) THEN
                            WRITE(LABEL,YFMT)YPOS
                        ELSE
                            IF (YCUR .GT. OWIND(4)) THEN
                                GOTO 201
                            ELSE
                                WRITE(LABEL,YFMT)YCUR
                                ENDIF
                            ENDIF
                            CALL CHSTR(LABEL,FIRST,LAST)
                            CALL GTX (YLAB,YPOS,LABEL(FIRST:LAST))
                        ENDIF
                  ENDIF
                  GOTO 201
            ENDIF
        IF ((LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 2).AND. YCUR .GT. OWIND(4))
     1     GOTO 201
C
C  MINOR TIC/GRID ?
C
            IF (TCOUNT .NE. MINCNT .AND. MINCNT .NE. 0) THEN
                  IF (LGRID) THEN
                        CALL GSPLCI(IGRIMN)
                  ENDIF
                  CALL GPL(2,PX,PY)
                  IF (LGRID) THEN
                        CALL GSPLCI(IGRIMJ)
                  ENDIF
                  TCOUNT = TCOUNT + 1
C
C  MAJOR TIC/GRID
C
            ELSE
                IF (.NOT. LGRID) THEN
                    PX(2) = TICBIG
                ENDIF
                CALL GPL(2,PX,PY)
C
C  LABEL
C
                  IF (IYLAB .EQ. 1 .AND. LOOP .EQ.1) THEN
                    IF (LOGVAL .EQ. 1 .OR. LOGVAL .EQ.3) THEN
                        WRITE(LABEL,YFMT)YPOS
                    ELSE
                        WRITE(LABEL,YFMT)YCUR
                    ENDIF
                    CALL CHSTR(LABEL,FIRST,LAST)
                    CALL GTX(YLAB,YPOS,LABEL(FIRST:LAST))
                  ENDIF
                  TCOUNT = 1
                IF (.NOT. LGRID) THEN
                      PX(2) = TICEND
                  ENDIF
          ENDIF
        IF ((LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 2).AND. YCUR .EQ. OWIND(4))
     1     GOTO 201
 200  CONTINUE
 201  CONTINUE
C
C
C  RIGHT Y AXIS TICKS ?
C
        IF (LOOP .EQ. 1 .AND.(IGPH.EQ.1 .OR. IGPH .EQ. 5 .OR.
     1    IGPH .EQ. 9)) THEN
            START = WIND(2)
            TICEND = START - TICMIN
            TICBIG = START - TICMAJ
            PX(1) = START
            PX(2) = TICEND
            YPOS = WIND(3)
            LOOP = 2
            IF (LOGVAL .EQ. 4 .OR. LOGVAL .EQ. 2) THEN
                YCUR = OWIND(3)
                IF (LOGMIN) THEN
                      CURMAJ = YCUR
                      NEXTMA = YCUR * MAJDIV
                      YINT = (NEXTMA - CURMAJ) / 9.
                ENDIF
            ENDIF
          GOTO 180
        ENDIF
C
C  RESET NORMALIZATION TRANSFORMATION TO WHAT IT WAS UPON ENTRY
C  TO GRIDAL
C
      IF (ICNT .NE. 0) THEN
          CALL GSWN(ICNT,OWIND(1),OWIND(2),OWIND(3),OWIND(4))
          CALL GSVP(ICNT,VIEW(1),VIEW(2),VIEW(3),VIEW(4))
      ENDIF
      CALL GSELNT(ICNT)
C
C  IF LABELS, RESTORE TEXT ATTRIBUTES
C
        IF (IXLAB .EQ. 1 .OR. IYLAB .EQ. 1) THEN
            CALL GSCHH(OLDCHH)
            CALL GSCHUP(CHUPX,CHUPY)
            CALL GSTXAL(OLDALH,OLDALV)
            CALL GSTXCI(OTEXCI)
        ENDIF
C
C  RESTORE ORIGINAL COLOR
C
      CALL GSPLCI(OCOLI)
C
C  RESTORE POLYLINE COLOR ASF TO WHAT IT WAS ON ENTRY TO GRIDAL
C
        LASF(10) = OTXASF
        LASF(3) = OPLASF
        CALL GSASF(LASF)
C
 999  RETURN
        END
