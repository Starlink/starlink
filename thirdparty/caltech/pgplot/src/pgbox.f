C*PGBOX -- draw labeled frame around viewport
C%void cpgbox(const char *xopt, float xtick, int nxsub, \
C% const char *yopt, float ytick, int nysub);
C+
      SUBROUTINE PGBOX (XOPT, XTICK, NXSUB, YOPT, YTICK, NYSUB)
      CHARACTER*(*) XOPT, YOPT
      REAL XTICK, YTICK
      INTEGER NXSUB, NYSUB
C
C Annotate the viewport with frame, axes, numeric labels, etc.
C PGBOX is called by on the user's behalf by PGENV, but may also be
C called explicitly.
C
C Arguments:
C  XOPT   (input)  : string of options for X (horizontal) axis of
C                    plot. Options are single letters, and may be in
C                    any order (see below).
C  XTICK  (input)  : world coordinate interval between major tick marks
C                    on X axis. If XTICK=0.0, the interval is chosen by
C                    PGBOX, so that there will be at least 3 major tick
C                    marks along the axis.
C  NXSUB  (input)  : the number of subintervals to divide the major
C                    coordinate interval into. If XTICK=0.0 or NXSUB=0,
C                    the number is chosen by PGBOX.
C  YOPT   (input)  : string of options for Y (vertical) axis of plot.
C                    Coding is the same as for XOPT.
C  YTICK  (input)  : like XTICK for the Y axis.
C  NYSUB  (input)  : like NXSUB for the Y axis.
C
C Options (for parameters XOPT and YOPT):
C  A : draw Axis (X axis is horizontal line Y=0, Y axis is vertical
C      line X=0).
C  B : draw bottom (X) or left (Y) edge of frame.
C  C : draw top (X) or right (Y) edge of frame.
C  G : draw Grid of vertical (X) or horizontal (Y) lines.
C  I : Invert the tick marks; ie draw them outside the viewport
C      instead of inside.
C  L : label axis Logarithmically (see below).
C  N : write Numeric labels in the conventional location below the
C      viewport (X) or to the left of the viewport (Y).
C  P : extend ("Project") major tick marks outside the box (ignored if
C      option I is specified).
C  M : write numeric labels in the unconventional location above the
C      viewport (X) or to the right of the viewport (Y).
C  T : draw major Tick marks at the major coordinate interval.
C  S : draw minor tick marks (Subticks).
C  V : orient numeric labels Vertically. This is only applicable to Y.
C      The default is to write Y-labels parallel to the axis.
C  1 : force decimal labelling, instead of automatic choice (see PGNUMB).
C  2 : force exponential labelling, instead of automatic.
C
C To get a complete frame, specify BC in both XOPT and YOPT.
C Tick marks, if requested, are drawn on the axes or frame
C or both, depending which are requested. If none of ABC is specified,
C tick marks will not be drawn. When PGENV calls PGBOX, it sets both
C XOPT and YOPT according to the value of its parameter AXIS:
C -1: 'BC', 0: 'BCNST', 1: 'ABCNST', 2: 'ABCGNST'.
C
C For a logarithmic axis, the major tick interval is always 1.0. The
C numeric label is 10**(x) where x is the world coordinate at the
C tick mark. If subticks are requested, 8 subticks are drawn between
C each major tick at equal logarithmic intervals.
C
C To label an axis with time (days, hours, minutes, seconds) or
C angle (degrees, arcmin, arcsec), use routine PGTBOX.
C--
C 19-Oct-1983
C 23-Sep-1984 - fix bug in labelling reversed logarithmic axes.
C  6-May-1985 - improve behavior for pen plotters [TJP].
C 23-Nov-1985 - add 'P' option [TJP].
C 14-Jan-1986 - use new routine PGBOX1 to fix problem of missing
C               labels at end of axis [TJP].
C  8-Apr-1987 - improve automatic choice of tick interval; improve
C               erroneous rounding of tick interval to 1 digit [TJP].
C 23-Apr-1987 - fix bug: limit max number of ticks to ~10 [TJP].
C  7-Nov-1987 - yet another change to algorithm for choosing tick
C               interval; maximum tick interval is now 0.2*range of
C               axis, which may round up to 0.5 [TJP].
C 15-Dec-1988 - correct declaration of MAJOR [TJP].
C  6-Sep-1989 - use Fortran generic intrinsic functions [TJP].
C 18-Oct-1990 - correctly initialize UTAB(1) [AFT].
C 19-Oct-1990 - do all plotting in world coordinates [TJP].
C  6-Nov-1991 - label logarithmic subticks when necessary [TJP].
C  4-Jul-1994 - add '1' and '2' options [TJP].
C 20-Apr-1995 - adjust position of labels slightly, and move out
C               when ticks are inverted [TJP].
C 26-Feb-1997 - use new routine pgclp [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      CHARACTER*20  CLBL
      CHARACTER*64  OPT
      LOGICAL  XOPTA, XOPTB, XOPTC, XOPTG, XOPTN, XOPTM, XOPTT, XOPTS
      LOGICAL  YOPTA, YOPTB, YOPTC, YOPTG, YOPTN, YOPTM, YOPTT, YOPTS
      LOGICAL  XOPTI, YOPTI, YOPTV, XOPTL, YOPTL, XOPTP, YOPTP, RANGE
      LOGICAL  IRANGE, MAJOR, XOPTLS, YOPTLS, PGNOTO
      REAL     TAB(9), UTAB(9)
      INTEGER  I, I1, I2, J, NC, NP, NV, KI, CLIP
      INTEGER  NSUBX, NSUBY, JMAX, XNFORM, YNFORM
      REAL     TIKL, TIKL1, TIKL2, XC, YC
      REAL     XINT, XINT2, XVAL, YINT, YINT2, YVAL
      REAL     PGRND
      REAL     A, B, C
      REAL     XNDSP, XMDSP, YNDSP, YMDSP, YNVDSP, YMVDSP
      REAL     XBLC, XTRC, YBLC, YTRC
      INTRINSIC ABS, INDEX, INT, LOG10, MAX, MIN, MOD, NINT, SIGN, REAL
C
C Table of logarithms 1..9
C
      DATA TAB / 0.00000, 0.30103, 0.47712, 0.60206, 0.69897,
     1           0.77815, 0.84510, 0.90309, 0.95424 /
C
      RANGE(A,B,C) = (A.LT.B.AND.B.LT.C) .OR. (C.LT.B.AND.B.LT.A)
      IRANGE(A,B,C) = (A.LE.B.AND.B.LE.C) .OR. (C.LE.B.AND.B.LE.A)
C
      IF (PGNOTO('PGBOX')) RETURN
      CALL PGBBUF
      CALL PGQWIN(XBLC, XTRC, YBLC, YTRC)
C
C Decode options.
C
      CALL GRTOUP(OPT,XOPT)
      XOPTA = INDEX(OPT,'A').NE.0 .AND. RANGE(YBLC,0.0,YTRC)
      XOPTB = INDEX(OPT,'B').NE.0
      XOPTC = INDEX(OPT,'C').NE.0
      XOPTG = INDEX(OPT,'G').NE.0
      XOPTI = INDEX(OPT,'I').NE.0
      XOPTL = INDEX(OPT,'L').NE.0
      XOPTM = INDEX(OPT,'M').NE.0
      XOPTN = INDEX(OPT,'N').NE.0
      XOPTS = INDEX(OPT,'S').NE.0
      XOPTT = INDEX(OPT,'T').NE.0
      XOPTP = INDEX(OPT,'P').NE.0 .AND. (.NOT.XOPTI)
      XNFORM = 0
      IF (INDEX(OPT,'1').NE.0) XNFORM = 1
      IF (INDEX(OPT,'2').NE.0) XNFORM = 2
      CALL GRTOUP(OPT,YOPT)
      YOPTA = INDEX(OPT,'A').NE.0 .AND. RANGE(XBLC,0.0,XTRC)
      YOPTB = INDEX(OPT,'B').NE.0
      YOPTC = INDEX(OPT,'C').NE.0
      YOPTG = INDEX(OPT,'G').NE.0
      YOPTI = INDEX(OPT,'I').NE.0
      YOPTL = INDEX(OPT,'L').NE.0
      YOPTN = INDEX(OPT,'N').NE.0
      YOPTM = INDEX(OPT,'M').NE.0
      YOPTS = INDEX(OPT,'S').NE.0
      YOPTT = INDEX(OPT,'T').NE.0
      YOPTV = INDEX(OPT,'V').NE.0
      YOPTP = INDEX(OPT,'P').NE.0 .AND. (.NOT.YOPTI)
      YNFORM = 0
      IF (INDEX(OPT,'1').NE.0) YNFORM = 1
      IF (INDEX(OPT,'2').NE.0) YNFORM = 2
C
C Displacement of labels from edge of box
C (for X bottom/top, Y left/right, and Y left/right with V option).
C
      XNDSP = 1.2
      XMDSP = 0.7
      YNDSP = 0.7
      YMDSP = 1.2
      YNVDSP = 0.7
      YMVDSP = 0.7
      IF (XOPTI) THEN
         XNDSP = XNDSP + 0.3
         XMDSP = XMDSP + 0.3
      END IF
      IF (YOPTI) THEN
         YNDSP = YNDSP + 0.3
         YMDSP = YMDSP + 0.3
         YNVDSP = YNVDSP + 0.3
         YMVDSP = YMVDSP + 0.3
      END IF
C
C Disable clipping.
C
      CALL PGQCLP(CLIP)
      CALL PGSCLP(0)
C
C Draw box.
C
      IF (XOPTB) THEN
          CALL GRMOVA(XBLC, YBLC)
          CALL GRLINA(XTRC, YBLC)
      END IF
      IF (YOPTC) THEN
          CALL GRMOVA(XTRC, YBLC)
          CALL GRLINA(XTRC, YTRC)
      END IF
      IF (XOPTC) THEN
          CALL GRMOVA(XTRC, YTRC)
          CALL GRLINA(XBLC, YTRC)
      END IF
      IF (YOPTB) THEN
          CALL GRMOVA(XBLC, YTRC)
          CALL GRLINA(XBLC, YBLC)
      END IF
C
C Draw axes if required.
C
      IF (XOPTA.AND..NOT.XOPTG) THEN
          CALL GRMOVA(XBLC, 0.0)
          CALL GRLINA(XTRC, 0.0)
      END IF
      IF (YOPTA.AND..NOT.YOPTG) THEN
          CALL GRMOVA(0.0, YBLC)
          CALL GRLINA(0.0, YTRC)
      END IF
C
C Length of X tick marks.
C
      TIKL1 = PGXSP(PGID)*0.6*(YTRC-YBLC)/PGYLEN(PGID)
      IF (XOPTI) TIKL1 = -TIKL1
      TIKL2 = TIKL1*0.5
C
C Choose X tick intervals. Major interval = XINT,
C minor interval = XINT2 = XINT/NSUBX.
C
      UTAB(1) = 0.0
      IF (XOPTL) THEN
          XINT = SIGN(1.0,XTRC-XBLC)
          NSUBX = 1
          DO 10 J=2,9
              UTAB(J) = TAB(J)
              IF (XINT.LT.0.0) UTAB(J) = 1.0-TAB(J)
   10     CONTINUE
      ELSE IF (XTICK.EQ.0.0) THEN
          XINT = MAX(0.05, MIN(7.0*PGXSP(PGID)/PGXLEN(PGID), 0.20))
     1           *(XTRC-XBLC)
          XINT = PGRND(XINT,NSUBX)
      ELSE
          XINT = SIGN(XTICK,XTRC-XBLC)
          NSUBX = MAX(NXSUB,1)
      END IF
      IF (.NOT.XOPTS) NSUBX = 1
      NP = INT(LOG10(ABS(XINT)))-4
      NV = NINT(XINT/10.**NP)
      XINT2 = XINT/NSUBX
      XOPTLS = XOPTL .AND. XOPTS .AND. (ABS(XTRC-XBLC).LT.2.0)
C
C Draw X grid.
C
      IF (XOPTG) THEN
          CALL PGBOX1(XBLC, XTRC, XINT, I1, I2)
          DO 20 I=I1,I2
              CALL GRMOVA(REAL(I)*XINT, YBLC)
              CALL GRLINA(REAL(I)*XINT, YTRC)
   20     CONTINUE
      END IF
C
C Draw X ticks.
C
      IF (XOPTT.OR.XOPTS) THEN
          CALL PGBOX1(XBLC, XTRC, XINT2, I1, I2)
          JMAX = 1
          IF (XOPTL.AND.XOPTS) JMAX=9
C
C         Bottom ticks.
C
          IF (XOPTB) THEN
            DO 40 I=I1-1,I2
            DO 30 J=1,JMAX
                MAJOR = (MOD(I,NSUBX).EQ.0).AND.XOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                XVAL = (I+UTAB(J))*XINT2
                IF (IRANGE(XBLC,XVAL,XTRC)) THEN
                    IF (XOPTP.AND.MAJOR) THEN
                        CALL GRMOVA(XVAL, YBLC-TIKL2)
                    ELSE
                        CALL GRMOVA(XVAL, YBLC)
                    END IF
                    CALL GRLINA(XVAL, YBLC+TIKL)
                END IF
   30        CONTINUE
   40       CONTINUE
          END IF
C
C         Axis ticks.
C
          IF (XOPTA) THEN
            DO 60 I=I1-1,I2
            DO 50 J=1,JMAX
                MAJOR = (MOD(I,NSUBX).EQ.0).AND.XOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                XVAL = (I+UTAB(J))*XINT2
                IF (IRANGE(XBLC,XVAL,XTRC)) THEN
                    CALL GRMOVA(XVAL, -TIKL)
                    CALL GRLINA(XVAL, TIKL)
                END IF
   50       CONTINUE
   60       CONTINUE
          END IF
C
C         Top ticks.
C
          IF (XOPTC) THEN
            DO 80 I=I1-1,I2
            DO 70 J=1,JMAX
                MAJOR = (MOD(I,NSUBX).EQ.0).AND.XOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                XVAL = (I+UTAB(J))*XINT2
                IF (IRANGE(XBLC,XVAL,XTRC)) THEN
                    CALL GRMOVA(XVAL, YTRC-TIKL)
                    CALL GRLINA(XVAL, YTRC)
                END IF
   70       CONTINUE
   80       CONTINUE
          END IF
      END IF
C
C Write X labels.
C
      IF (XOPTN .OR. XOPTM) THEN
          CALL PGBOX1(XBLC, XTRC, XINT, I1, I2)
          DO 90 I=I1,I2
              XC = (I*XINT-XBLC)/(XTRC-XBLC)
              IF (XOPTL) THEN
                  CALL PGNUMB(1,NINT(I*XINT),XNFORM,CLBL,NC)
              ELSE
                  CALL PGNUMB(I*NV,NP,XNFORM,CLBL,NC)
              END IF
              IF (XOPTN) CALL PGMTXT('B', XNDSP, XC, 0.5, CLBL(1:NC))
              IF (XOPTM) CALL PGMTXT('T', XMDSP, XC, 0.5, CLBL(1:NC))
   90     CONTINUE
      END IF
C
C Extra X labels for log axes.
C
      IF (XOPTLS) THEN
          CALL PGBOX1(XBLC, XTRC, XINT2, I1, I2)
          DO 401 I=I1-1,I2
             DO 301 J=2,5,3
                XVAL = (I+UTAB(J))*XINT2
                XC = (XVAL-XBLC)/(XTRC-XBLC)
                KI = I
                IF (XTRC.LT.XBLC) KI = KI+1
                IF (IRANGE(XBLC,XVAL,XTRC)) THEN
                    CALL PGNUMB(J,NINT(KI*XINT2),XNFORM,CLBL,NC)
                    IF (XOPTN) 
     1                CALL PGMTXT('B', XNDSP, XC, 0.5, CLBL(1:NC))
                    IF (XOPTM) 
     1                CALL PGMTXT('T', XMDSP, XC, 0.5, CLBL(1:NC))
                END IF
  301       CONTINUE
  401     CONTINUE
      END IF
C
C Length of Y tick marks.
C
      TIKL1 = PGXSP(PGID)*0.6*(XTRC-XBLC)/PGXLEN(PGID)
      IF (YOPTI) TIKL1 = -TIKL1
      TIKL2 = TIKL1*0.5
C
C Choose Y tick intervals. Major interval = YINT,
C minor interval = YINT2 = YINT/NSUBY.
C
      UTAB(1) = 0.0
      IF (YOPTL) THEN
          YINT = SIGN(1.0,YTRC-YBLC)
          NSUBY = 1
          DO 100 J=2,9
              UTAB(J) = TAB(J)
              IF (YINT.LT.0.0) UTAB(J) = 1.0-TAB(J)
  100     CONTINUE
      ELSE IF (YTICK.EQ.0.0) THEN
          YINT = MAX(0.05, MIN(7.0*PGXSP(PGID)/PGYLEN(PGID), 0.20))
     1           *(YTRC-YBLC)
          YINT = PGRND(YINT,NSUBY)
      ELSE
          YINT  = SIGN(YTICK,YTRC-YBLC)
          NSUBY = MAX(NYSUB,1)
      END IF
      IF (.NOT.YOPTS) NSUBY = 1
      NP = INT(LOG10(ABS(YINT)))-4
      NV = NINT(YINT/10.**NP)
      YINT2 = YINT/NSUBY
      YOPTLS = YOPTL .AND. YOPTS .AND. (ABS(YTRC-YBLC).LT.2.0)
C
C Draw Y grid.
C
      IF (YOPTG) THEN
          CALL PGBOX1(YBLC, YTRC, YINT, I1, I2)
          DO 110 I=I1,I2
              CALL GRMOVA(XBLC, REAL(I)*YINT)
              CALL GRLINA(XTRC, REAL(I)*YINT)
  110     CONTINUE
      END IF
C
C Draw Y ticks.
C
      IF (YOPTT.OR.YOPTS) THEN
          CALL PGBOX1(YBLC, YTRC, YINT2, I1, I2)
          JMAX = 1
          IF (YOPTL.AND.YOPTS) JMAX = 9
C
C               Left ticks.
C
            IF (YOPTB) THEN
            DO 130 I=I1-1,I2
            DO 120 J=1,JMAX
                MAJOR = (MOD(I,NSUBY).EQ.0).AND.YOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                YVAL = (I+UTAB(J))*YINT2
                IF (IRANGE(YBLC,YVAL,YTRC)) THEN
                    IF (YOPTP.AND.MAJOR) THEN
                        CALL GRMOVA(XBLC-TIKL2, YVAL)
                    ELSE
                        CALL GRMOVA(XBLC, YVAL)
                    END IF
                    CALL GRLINA(XBLC+TIKL, YVAL)
                END IF
  120       CONTINUE
  130       CONTINUE
            END IF
C
C               Axis ticks.
C
            IF (YOPTA) THEN
            DO 150 I=I1-1,I2
            DO 140 J=1,JMAX
                MAJOR = (MOD(I,NSUBY).EQ.0).AND.YOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                YVAL = (I+UTAB(J))*YINT2
                IF (IRANGE(YBLC,YVAL,YTRC)) THEN
                    CALL GRMOVA(-TIKL, YVAL)
                    CALL GRLINA(TIKL, YVAL)
                END IF
  140       CONTINUE
  150       CONTINUE
            END IF
C
C               Right ticks.
C
            IF (YOPTC) THEN
            DO 170 I=I1-1,I2
            DO 160 J=1,JMAX
                MAJOR = (MOD(I,NSUBY).EQ.0).AND.YOPTT.AND.J.EQ.1
                TIKL = TIKL2
                IF (MAJOR) TIKL = TIKL1
                YVAL = (I+UTAB(J))*YINT2
                IF (IRANGE(YBLC,YVAL,YTRC)) THEN
                    CALL GRMOVA(XTRC-TIKL, YVAL)
                    CALL GRLINA(XTRC, YVAL)
                END IF
  160       CONTINUE
  170       CONTINUE
            END IF
        END IF
C
C Write Y labels.
C
      IF (YOPTN.OR.YOPTM) THEN
          CALL PGBOX1(YBLC, YTRC, YINT, I1, I2)
          DO 180 I=I1,I2
              YC = (I*YINT-YBLC)/(YTRC-YBLC)
              IF (YOPTL) THEN
                  CALL PGNUMB(1,NINT(I*YINT),YNFORM,CLBL,NC)
              ELSE
                  CALL PGNUMB(I*NV,NP,YNFORM,CLBL,NC)
              END IF
              IF (YOPTV) THEN
                  IF (YOPTN) CALL PGMTXT('LV',YNVDSP,YC,1.0,CLBL(1:NC))
                  IF (YOPTM) CALL PGMTXT('RV',YMVDSP,YC,0.0,CLBL(1:NC))
              ELSE
                  IF (YOPTN) CALL PGMTXT('L',YNDSP,YC,0.5,CLBL(1:NC))
                  IF (YOPTM) CALL PGMTXT('R',YMDSP,YC,0.5,CLBL(1:NC))
              END IF
  180     CONTINUE
      END IF
C
C Extra Y labels for log axes.
C
      IF (YOPTLS) THEN
          CALL PGBOX1(YBLC, YTRC, YINT2, I1, I2)
          DO 402 I=I1-1,I2
            DO 302 J=2,5,3
                YVAL = (I+UTAB(J))*YINT2
                YC = (YVAL-YBLC)/(YTRC-YBLC)
                KI = I
                IF (YBLC.GT.YTRC) KI = KI+1
                IF (IRANGE(YBLC,YVAL,YTRC)) THEN
                    CALL PGNUMB(J,NINT(KI*YINT2),YNFORM,CLBL,NC)
                    IF (YOPTV) THEN
                    IF (YOPTN) 
     1                CALL PGMTXT('LV', YNVDSP, YC, 1.0, CLBL(1:NC))
                    IF (YOPTM) 
     1                CALL PGMTXT('RV', YMVDSP, YC, 0.0, CLBL(1:NC))
                    ELSE
                    IF (YOPTN) 
     1                CALL PGMTXT('L', YNDSP, YC, 0.5, CLBL(1:NC))
                    IF (YOPTM) 
     1                CALL PGMTXT('R', YMDSP, YC, 0.5, CLBL(1:NC))
                    END IF
                END IF
  302       CONTINUE
  402     CONTINUE
      END IF
C
C Enable clipping.
C
      CALL PGSCLP(CLIP)
C
      CALL PGEBUF
      END
