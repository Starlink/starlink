      SUBROUTINE HST1(DATA,NPTS,LOW,HI,NCLASS,NXY,NXY2,X,Y,W,DST,
     -  IWORK,LIWORK,IER)
C
      COMMON /HSTGC1/ HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM,
     -       HFRAME, LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL,
     -       FREQNC, HWIND(4), COLSHA, COLREC, COLAXI, COLMED, COLTEX,
     -       COLTIT, COLPER
      LOGICAL HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM, HFRAME,
     -        LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL, FREQNC
      COMMON /HSTGC2/ STRFOR, STRTIT, STRLAB, STRFRE
      CHARACTER*55  STRFOR, STRTIT, STRLAB, STRFRE
C
      DIMENSION DATA(NPTS),X(NXY),Y(NXY),DST(NXY2),W(NPTS),IWORK(LIWORK)
      REAL     LOW, HI, MED
      REAL     PX(5), PY(5), NEXTX, YTICKS(4), TICINT, PER(4)
      REAL     OWIND(4), OVIEW(4), VUPORT(4)
      REAL     NEWWIN(4), LABMAX, LASTLB
      INTEGER  NPTS, NCLASS, FIRST, LAST, LASF(13)
      INTEGER  OLDALH, OLDALV
      CHARACTER*15 TEX
      LOGICAL DONE
      SAVE
C
C  THE FOLLOWING IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4('GRAPHX','HSTGRM','HSTGRM','VERSION 01')
C
C  INTIALIZE ERROR COUNT
C
      NERR = 0
C
C  GET STANDARD ERROR MESSAGE UNIT
C
        IERUNT = I1MACH(4)
C
C  PERFORM ERROR CHECKING
C
      IF (NPTS .LT. 1) THEN
          NERR = NERR + 1
          CALL SETER(' HSTGRM--NPTS LESS THAN 1',NERR,2)
      ENDIF
      IF (NCLASS .LT. 1) THEN
          NERR = NERR + 1
          CALL SETER(' HSTGRM--NCLASS LESS THAN 1',NERR,2)
      ENDIF
      IF (IER .NE. 0) THEN
          NERR = NERR + 1
          IF (IER .EQ. 1) THEN
          CALL SETER(' HSTGRM--DIMENSION OF ARRAY WORK TOO SMALL',NERR,
     - 2)
          ELSEIF (IER .EQ. 2) THEN
          CALL SETER(' HSTGRM--DIMENSION OF ARRAY IWORK TOO SMALL',NERR,
     - 2)
          ELSE
          CALL SETER(' HSTGRM--DIMENSIONS OF ARRAYS WORK AND IWORK ARE T
     -OO SMALL',NERR,
     - 2)
          ENDIF
      ENDIF
C
C  IF LISTOP IS ON, LIST ALL OPTION VALUES
C
      IF (LISTOP) THEN
          CALL HSTLST
      ENDIF
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
C  INQUIRE CURRENT NORMALIZATION NUMBER
C
        CALL GQCNTN(IERR,ICNT)
C
C  INQUIRE CURRENT WINDOW AND VIEWPORT LIMITS
C
        CALL GQNT(ICNT,IERR,OWIND,OVIEW)
C
C  INQUIRE AND SAVE TEXT ATTRIBUTES
C
          CALL GQCHH(IERR,OLDCHH)
          CALL GQCHUP(IERR,CHUPX,CHUPY)
          CALL GQTXAL(IERR,OLDALH,OLDALV)
          CALL GQTXCI (IERR,OTEXCI)
C
C  SET TEXT COLOR
C
      IF (COLORS) CALL GSTXCI(COLTEX)
C
C  INITIALIZE VARIABLES
C
      LABMAX = 8.
      DONE = .FALSE.
C
      DO 100 I = 1,NXY
  100     Y(I) = 0.
C
C  COMPUTE NUMBER OF RECTANGLES TO BE DRAWN AND LABELLED
C
      IF (MIDVAL) THEN
          NXINT = NCLASS + 3
          NUMLAB = NCLASS
      ELSE
          NXINT = NCLASS + 2
          NUMLAB = NCLASS + 1
      ENDIF
      XINT = (HI - LOW) / FLOAT(NCLASS)
      HAFINT = XINT / 2.
      LASTRC = NXY - 2
C
C  DO SHELL SORT, FIND MEDIAN OF ALL DATA POINTS
C
      CALL HSTMED(DATA,NPTS,W,MED)
C
C  GIVE WARNING IF UPPER/LOWER BOUND IS OUT OF RANGE
C
      IF (HI .GT. W(NPTS)) THEN
          NERR = NERR + 1
          CALL SETER(' HSTGRM--UPPER BOUND, RHI, .GT. ALL DATA VALUES',
     - NERR,1)
          CALL ERROF
          WRITE(IERUNT,1001)
 1001 FORMAT(' HSTGRM--UPPER BOUND, RHI, .GT. ALL DATA VALUES')
      ENDIF
      IF (LOW .LT. W(1)) THEN
          NERR = NERR + 1
          CALL SETER(' HSTGRM--LOWER BOUND, RLOW, .LT. ALL DATA VALUES',
     - NERR,1)
          WRITE(IERUNT,1002)
 1002 FORMAT(' HSTGRM--LOWER BOUND, RLOW, .LT. ALL DATA VALUES')
          CALL ERROF
      ENDIF
C
C  DTERMINE XMIN,XMAX
C
      XMIN = LOW - XINT
      XSTART = XMIN
      IF (MIDVAL) THEN
          XMIN = XMIN - HAFINT
      ENDIF
C
C  FILL IN X ARRAY FOR SHADING (LATER)
C
      XPOS = XSTART
      DO 500 I = 2,NXY,2
          X(I - 1) = XPOS
          X(I) = XPOS
  500 XPOS = XPOS + XINT
      XMAX = X(NXY)
      IF (MIDVAL) THEN
          XMAX = XMAX + HAFINT
      ENDIF
C
C  Y ARRAY (DETERMINE FREQUENCY FOR EACH CLASS)
C
      DO 200 I = 1,NPTS
          IF (DATA(I) .LT. LOW) THEN
              Y(2) = Y(2) + 1.
          ELSEIF (DATA(I) .GE. HI) THEN
              Y(LASTRC) = Y(LASTRC) + 1.
          ELSE
              DO 210 J = 5,LASTRC,2
                IF (DATA(I) .LT. X(J)) THEN
                    Y(J-1) = Y(J-1) + 1.
                    GOTO 200
                ENDIF
  210         CONTINUE
          ENDIF
  200 CONTINUE
C
C  FIND MAX Y, FILL IN Y ARRAY AND ADJUST Y WINDOW LIMIT
C
      YBOUND = Y(2)
      Y(3) = Y(2)
      DO 400  I = 4,LASTRC,2
          YBOUND = AMAX1(YBOUND,Y(I))
 400      Y(I+1) = Y(I)
      IF (YBOUND .LT. 10.) THEN
          YMAX = YBOUND + 1.
      ELSEIF (YBOUND .LT. 100.) THEN
          YMAX = 10.*INT(YBOUND/10. + 1.)
      ELSEIF (YBOUND .LT. 1000.) THEN
          YMAX = 100.*INT(YBOUND/100. + 1.)
      ELSEIF (YBOUND .LT. 10000.) THEN
          YMAX = 1000.*INT(YBOUND/1000. + 1.)
      ELSEIF (YBOUND .LT. 100000.) THEN
          YMAX = 10000.*INT(YBOUND/10000. + 1.)
      ELSE
          YMAX = 1000000.
      ENDIF
C
C  DETERMINE Y-AXIS TICK SPACING AND EQUIVALENT PERCENTAGES
C
      TICINT = YMAX / 4.
      YTICKS(1) = TICINT
      YTICKS(2) = TICINT * 2.
      YTICKS(3) = TICINT * 3.
      YTICKS(4) = YMAX
      IF (PERCNT) THEN
          PERC = YMAX / FLOAT(NPTS) * 100.
          PER(1) = PERC / 4.
          PER(2) = PER(1) * 2.
          PER(3) = PER(1) * 3.
          PER(4) = PERC
      ENDIF
C
C  SET UP WINDOW COORDINATES
C
      IF (.NOT. HORZNT) THEN
          CALL GSWN(1,XMIN,XMAX,0.,YMAX)
          VUPORT(1) = .15
          VUPORT(2) = .85
          VUPORT(3) = .15
          VUPORT(4) = .85
      ELSE
          CALL GSWN(1,0.,YMAX,XMIN,XMAX)
          VUPORT(1) = .28
          VUPORT(2) = .95
          VUPORT(3) = .1
          VUPORT(4) = .85
      ENDIF
      IF (WINDOW) THEN
          DO 505 I = 1,4
            IF (HWIND(I) .LT. 0. .OR. HWIND(I) .GT. 1.) THEN
              NERR = NERR + 1
            CALL SETER(' HSTGRM--WINDOW OPTION ERROR, RANGE IS 0. - 1.',
     - NERR,1)
              CALL ERROF
              HWIND(1) = 0.
              HWIND(2) = 1.
              HWIND(3) = 0.
              HWIND(4) = 1.
              GOTO 506
            ENDIF
  505 CONTINUE
  506 CONTINUE
          WXRANG = HWIND(2) - HWIND(1)
          WYRANG = HWIND(4) - HWIND(3)
          VUPORT(1) = HWIND(1) + VUPORT(1)*WXRANG
          VUPORT(2) = HWIND(1) + VUPORT(2)*WXRANG
          VUPORT(3) = HWIND(3) + VUPORT(3)*WYRANG
          VUPORT(4) = HWIND(3) + VUPORT(4)*WYRANG
      ELSE
          HWIND(1) = 0.
          HWIND(2) = 1.
          HWIND(3) = 0.
          HWIND(4) = 1.
      ENDIF
      CALL GSVP(1,VUPORT(1),VUPORT(2),VUPORT(3),VUPORT(4))
      CALL GSELNT(1)
C
C  EXPAND WINDOW AND VEIWPORT FOR LABELING
C  DETERMINE CHARACTER HEIGHT AND TICK LENGTHS
C
      CALL HSTEXP(HWIND,NEWWIN)
      FRACT = 18./1024.
      YRANGE = NEWWIN(4) - NEWWIN(3)
      XRANGE = NEWWIN(2) - NEWWIN(1)
      IF (.NOT. HORZNT) THEN
          XTIC = FRACT * YRANGE
          YTIC = FRACT * XRANGE
      ELSE
          XTIC = FRACT * XRANGE
          YTIC = FRACT * YRANGE
      ENDIF
      XDEC = .8 * XTIC
      YDEC = .8 * YTIC
      CHARH = .014 * YRANGE
      CALL GSCHH(CHARH)
C
C  DRAW FREQUENCY-AXIS
C
      IF (COLORS) CALL GSPLCI(COLAXI)
      PX(1) = XMIN
      PX(2) = XMIN
      PY(1) = 0.
      PY(2) = YMAX
      IF (.NOT. HORZNT) THEN
          CALL GPL(2,PX,PY)
C                            RIGHT,HALF
          CALL GSTXAL(3,3)
      ELSE
          CALL GPL(2,PY,PX)
C                            CENTER,TOP
          CALL GSTXAL(2,1)
      ENDIF
      PX(1) = XMIN
      PX(2) = XMIN + YTIC
      XPOS = XMIN - YDEC
      DO 510 I = 1,4
          PY(1) = YTICKS(I)
          PY(2) = PY(1)
          YPOS = PY(1)
          NUM = INT(PY(1))
          WRITE(TEX,'(I9)')NUM
          CALL HSTSTR(TEX,FIRST,LAST)
          IF (.NOT. HORZNT) THEN
              CALL GPL(2,PX,PY)
              CALL GTX(XPOS,YPOS,TEX(FIRST:LAST))
          ELSE
              CALL GPL(2,PY,PX)
              CALL GTX(YPOS,XPOS,TEX(FIRST:LAST))
          ENDIF
  510 CONTINUE
C
C  SET UP VALUES FOR LABELING AND DRAWING TICKS ON X-AXIS
C
      PX(1) = XMIN
      PX(2) = XMAX
      PY(1) = 0.
      PY(2) = 0.
      IF (MIDVAL) THEN
          XPOS = X(3) + HAFINT
      ELSE
          XPOS = X(3)
      ENDIF
      IF (.NOT. HORZNT) THEN
          CALL GPL(2,PX,PY)
C                           RIGHT,TOP
          CALL GSTXAL(3,1)
          CALL GSCHUP(-.5,1.)
      ELSE
          CALL GPL(2,PY,PX)
C                           RIGHT,HALF
          CALL GSTXAL(3,3)
      ENDIF
      YPOS = 0. - 1.3 * XTIC
      PY(1) = 0.
      PY(2) = 0. - XTIC
      PX(1) = XPOS
      PX(2) = PX(1)
      DO 515 I = 1,NUMLAB
          IF (.NOT. HORZNT) THEN
              CALL GPL(2,PX,PY)
          ELSE
              CALL GPL(2,PY,PX)
          ENDIF
          PX(1) = PX(1) + XINT
          PX(2) = PX(1)
  515 CONTINUE
C
C  IF MORE THAN LABMAX LABELS TO BE LABELED THEN
C  COMPUTE LABEL INTERVAL SO THAT THERE ARE NO MORE
C  THAN LABMAX LABELS
C
      IF (FLOAT(NUMLAB) .GT. LABMAX) THEN
          XDIV = FLOAT(NUMLAB)/LABMAX
          IF ((AINT(XDIV) - XDIV) .NE. 0.) THEN
            NDIV = 2 * INT(XDIV + 1.)
          ENDIF
      ELSE
          NDIV = 2
      ENDIF
      IF (MIDVAL) THEN
          LASTLB = X(LASTRC) - HAFINT
          LASTL = LASTRC - 2
      ELSE
          LASTL = LASTRC
          LASTLB = X(LASTRC)
      ENDIF
      I = 3
  525 CONTINUE
C
C  CHECK TO MAKE SURE CURRENT LABEL IS NOT TOO CLOSE TO
C  LAST LABEL ( X(LASTRC) )
C
          XPOS = X(I)
          IF (MIDVAL) THEN
              XPOS = XPOS + HAFINT
          ENDIF
          IF (I + NDIV .GT. LASTL) THEN
              DONE = .TRUE.
              XPOS = LASTLB
          ENDIF
          WRITE(TEX,STRFOR)XPOS
          CALL HSTSTR(TEX,FIRST,LAST)
          IF (.NOT. HORZNT) THEN
              CALL GTX(XPOS,YPOS,TEX(FIRST:LAST))
          ELSE
              CALL GTX(YPOS,XPOS,TEX(FIRST:LAST))
          ENDIF
          IF (DONE) GOTO 526
          I = I + NDIV
          GOTO 525
  526 CONTINUE
      IF (.NOT. HORZNT) THEN
          CALL GSCHUP(0.,1.)
      ENDIF
C
C  DRAW PERCENT AXIS
C
      IF (PERCNT) THEN
          PX(1) = XMAX
          PX(2) = XMAX
          PY(1) = 0.
          PY(2) = YMAX
          IF (.NOT. HORZNT) THEN
C                             (LEFT,HALF)
            CALL GSTXAL(1,3)
            CALL GPL(2,PX,PY)
          ELSE
C                             (CENTER,BOTTOM)
            CALL GSTXAL(2,4)
            CALL GPL(2,PY,PX)
          ENDIF
          PX(1) = XMAX
          PX(2) = XMAX - YTIC
          XPOS = XMAX + YDEC
          DO 550 J = 1,4
              PY(1) = YTICKS(J)
              PY(2) = YTICKS(J)
              WRITE(TEX,'(F5.1)')PER(J)
              CALL HSTSTR(TEX,FIRST,LAST)
              YPOS = PY(1)
              IF (.NOT. HORZNT) THEN
                  CALL GPL(2,PX,PY)
                  CALL GTX(XPOS,YPOS,TEX(FIRST:LAST))
              ELSE
                  CALL GPL(2,PY,PX)
                  CALL GTX(YPOS,XPOS,TEX(FIRST:LAST))
              ENDIF
  550     CONTINUE
      ENDIF
C
C  DRAW RECTANGLES, ONE AT A TIME
C
      IF (COLORS) CALL GSPLCI(COLREC)
      CURX = X(1)
      NEXTX = CURX + XINT
      NUM = 4
      PX(1) = CURX
      PX(2) = CURX
      PX(3) = NEXTX
      PX(4) = NEXTX
      PY(1) = 0.
      PY(2) = Y(2)
      PY(3) = Y(2)
      PY(4) = 0.
      IF (.NOT. HORZNT) THEN
          CALL GPL(NUM,PX,PY)
      ELSE
          CALL GPL(NUM,PY,PX)
      ENDIF
C
C
      DO 600 I = 4,LASTRC,2
      CURX = NEXTX
      NEXTX = NEXTX + XINT
C
C  NEXT RECTANGLE IS LOWER THEN LAST ONE
C
      IF (Y(I) .LT. Y(I-1)) THEN
          NUM = 3
          PX(1) = CURX
          PX(2) = NEXTX
          PX(3) = NEXTX
          PY(1) = Y(I)
          PY(2) = Y(I)
          PY(3) = 0.
C
C  NEXT RECTANGLE IS HIGHER THAN LAST ONE
C
      ELSE
          NUM = 4
          PX(1) = CURX
          PX(2) = CURX
          PX(3) = NEXTX
          PX(4) = NEXTX
          PY(1) = Y(I - 1)
          PY(2) = Y(I)
          PY(3) = Y(I)
          PY(4) = 0.
      ENDIF
      IF (.NOT. HORZNT) THEN
          CALL GPL(NUM,PX,PY)
      ELSE
          CALL GPL(NUM,PY,PX)
      ENDIF
C
C  DRAW LINE THROUGH RECTANGLE WHERE FREQUENCY TICK WOULD BE
C
      DO 650 J = 1,3
          IF (Y(I) .GT. YTICKS(J)) THEN
              PY(1) = YTICKS(J)
              PY(2) = YTICKS(J)
              PX(2) = NEXTX
              IF (.NOT. HORZNT) THEN
                CALL GPL(2,PX,PY)
              ELSE
                CALL GPL(2,PY,PX)
              ENDIF
          ENDIF
  650 CONTINUE
  600 CONTINUE
C
C  DONE DRAWING RECTANGLES
C
C  DO SHADING
C
      IF (SHADE) THEN
          IF (COLORS) CALL GSPLCI(COLSHA)
          NST = NXY2
          NND = LIWORK
          ISPACE = INT(900 * (VUPORT(2) - VUPORT(1)))
          IF (.NOT. HORZNT) THEN
              CALL GFA(NXY,X,Y)
          ELSE
              CALL GFA(NXY,Y,X)
          ENDIF
C
C  CALL PLOTIT TO FLUSH OUT BUFFER (FINISH DRAWING SHADE LINES)
C
          CALL PLOTIT(0,0,0)
      ENDIF
C
C  DRAW MEDIAN
C
      IF (MEDIAN) THEN
          IF (COLORS) CALL GSPLCI(COLMED)
          PX(1) = MED
          PX(2) = MED
          PY(1) = 0.
          PY(2) = YMAX
          IF (.NOT. HORZNT) THEN
              MED = (MED - NEWWIN(1))/XRANGE
              CALL GPL(2,PX,PY)
          ELSE
              MED = (MED - NEWWIN(3))/YRANGE
              CALL GPL(2,PY,PX)
          ENDIF
      ENDIF
C
C  DRAW PERIMETER
C
      IF (WINDOW) THEN
          CALL GSWN(1,0.,1.,0.,1.)
          CALL GSVP(1,HWIND(1),HWIND(2),HWIND(3),HWIND(4))
          CALL GSELNT(1)
      ELSE
          CALL GSELNT(0)
      ENDIF
      IF (PERIM) THEN
          IF (COLORS) CALL GSPLCI(COLPER)
          PX(1)=0.
          PX(2)=0.
          PX(3)=1.
          PX(4)=1.
          PX(5)=0.
          PY(1)=0.
          PY(2)=1.
          PY(3)=1.
          PY(4)=0.
          PY(5)=0.
          CALL GPL(5,PX,PY)
      ENDIF
C
C  OUTPUT LABELS
C
      CHARH = .015
      CALL GSCHH(CHARH)
C                       CENTER,HALF
      CALL GSTXAL(2,3)
      XPOS = .5
      YPOS = .04
      NCHAR = LEN(STRLAB)
      IF (.NOT. LABEL) THEN
          IF (MIDVAL) THEN
              TEX = 'CLASS MIDVALUES'
          ELSE
              TEX = 'CLASS INTERVALS'
          ENDIF
          NCHAR = 15
          IF (.NOT. HORZNT) THEN
              CALL GTX(XPOS,YPOS,TEX(1:NCHAR))
          ELSE
              CALL GSCHUP(-1.,0.)
              CALL GTX(YPOS,XPOS,TEX(1:NCHAR))
          ENDIF
      ELSE
          CALL HSTSTR(STRLAB,FIRST,LAST)
          IF (.NOT. HORZNT) THEN
              CALL GTX(XPOS,YPOS,STRLAB(FIRST:LAST))
          ELSE
              CALL GSCHUP(-1.,0.)
              CALL GTX(YPOS,XPOS,STRLAB(FIRST:LAST))
          ENDIF
      ENDIF
      IF (FREQNC) THEN
          NCHAR = LEN(STRFRE)
          CALL HSTSTR(STRFRE,FIRST,LAST)
          IF (.NOT. HORZNT) THEN
              CALL GSCHUP(-1.,0.)
              CALL GTX(YPOS,XPOS,STRFRE(FIRST:LAST))
          ELSE
              XPOS = .6
              CALL GSCHUP(0.,1.)
              CALL GTX(XPOS,YPOS,STRFRE(FIRST:LAST))
          ENDIF
      ELSE
          IF (.NOT. HORZNT) THEN
              CALL GSCHUP(-1.,0.)
              CALL GTX(YPOS,XPOS,'FREQUENCY')
          ELSE
              XPOS = .6
              CALL GSCHUP(0.,1.)
              CALL GTX(XPOS,YPOS,'FREQUENCY')
          ENDIF
      ENDIF
C
C  LABEL PERCENT AXIS
C
      IF (PERCNT) THEN
          IF (.NOT. HORZNT) THEN
            YPOS = .96
            XPOS = .5
            CALL GTX(YPOS,XPOS,'PERCENT')
          ELSE
            YPOS = .92
            XPOS = .6
            CALL GTX(XPOS,YPOS,'PERCENT')
          ENDIF
      ENDIF
C
C  LABEL MEDIAN
C
      IF (MEDIAN) THEN
          XPOS = MED - .018
          IF (.NOT. HORZNT) THEN
              CALL GTX(XPOS,YPOS,'MEDIAN')
              YPOS = .85
          ELSE
              CALL GTX(YPOS,XPOS,'MEDIAN')
              YPOS = .90
          ENDIF
      ENDIF
C
C  OUTPUT TITLE
C
      IF (TITLE) THEN
          IF (COLORS) CALL GSPLCI(COLTIT)
          CHARH = .016
          CALL HSTSTR(STRTIT,FIRST,LAST)
          CALL GSCHH(CHARH)
          CALL GSCHUP(0.,1.)
          CALL GTX(.5,.965,STRTIT(FIRST:LAST))
      ENDIF
C
C  CALL FRAME UNLESS .HFRAME. IS FALSE
C
      IF (HFRAME) CALL FRAME
C
C  RESET NORMALIZATION TRANSFORMATION TO WHAT IT WAS UPON ENTRY
C  TO HISTOGRAM
C
      IF (ICNT .NE. 0) THEN
          CALL GSWN(ICNT,OWIND(1),OWIND(2),OWIND(3),OWIND(4))
          CALL GSVP(ICNT,OVIEW(1),OVIEW(2),OVIEW(3),OVIEW(4))
      ENDIF
      CALL GSELNT(ICNT)
C
C  RESTORE TEXT ATTRIBUTES
C
            CALL GSCHH(OLDCHH)
            CALL GSCHUP(CHUPX,CHUPY)
            CALL GSTXAL(OLDALH,OLDALV)
            CALL GSTXCI(OTEXCI)
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
      CALL PLOTIT(0,0,0)
      RETURN
      END
