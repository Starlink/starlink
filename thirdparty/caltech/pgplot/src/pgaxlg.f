C PGAXLG -- draw a logarithmic axis [internal routine]
C
      SUBROUTINE PGAXLG (OPT, X1, Y1, X2, Y2, V1, V2, STEP,
     :                   DMAJL, DMAJR, FMIN, DISP, ORIENT)
      CHARACTER*(*) OPT
      REAL X1, Y1, X2, Y2, V1, V2, STEP
      REAL DMAJL, DMAJR, FMIN, DISP, ORIENT
C
C Draw a labelled graph axis from world-coordinate position (X1,Y1)
C  to (X2,Y2). The quantity described by the axis runs from 10**V1 to
C 10**V2. A logarithmic axis always has major, labeled, tick marks 
C spaced by one or more decades. If the major tick marks are spaced
C by one decade (as specified by the STEP argument), then minor
C tick marks are placed at 2, 3, .., 9 times each power of 10;
C otherwise minor tick marks are spaced by one decade. If the axis
C spans less than two decades, numeric labels are placed at 1, 2, and
C 5 times each power of ten.
C
C It is not advisable to use this routine if the axis spans less than
C one decade, or if it spans many decades. In these cases it is
C preferable to use a linear axis labeled with the logarithm of the
C quantity of interest.
C
C Arguments:
C  OPT    (input)  : a string containing single-letter codes for
C                    various options. The options currently
C                    recognized are:
C                    N : write numeric labels
C                    1 : force decimal labelling, instead of automatic
C                        choice (see PGNUMB).
C                    2 : force exponential labelling, instead of
C                        automatic.
C  X1, Y1 (input)  : world coordinates of one endpoint of the axis.
C  X2, Y2 (input)  : world coordinates of the other endpoint of the axis.
C  V1     (input)  : logarithm of axis value at first endpoint.
C  V2     (input)  : logarithm of axis value at second endpoint.
C  STEP   (input)  : the number of decades between major (labeled) tick
C                    marks.
C  DMAJL  (input)  : length of major tick marks drawn to left of axis
C                    (as seen looking from first endpoint to second), in
C                    units of the character height.
C  DMAJR  (input)  : length of major tick marks drawn to right of axis,
C                    in units of the character height.
C  FMIN   (input)  : length of minor tick marks, as fraction of major.
C  DISP   (input)  : displacement of baseline of tick labels to
C                    right of axis, in units of the character height.
C  ORIENT (input)  : orientation of text label relative to axis (see
C                    PGTICK).
C--
C 25-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      REAL V, VMIN, VMAX, DVMAJ, DVMIN, PGRND
      INTEGER I, K, K1, K2, LLAB, NSUBT, CLIP, FORM
      LOGICAL XLAB, OPTN
      CHARACTER*32 LABEL
      REAL TAB(9)
C
C Table of logarithms 1..9
C
      DATA TAB / 0.00000, 0.30103, 0.47712, 0.60206, 0.69897,
     :           0.77815, 0.84510, 0.90309, 0.95424 /
C
C Check arguments.
C
      IF (X1.EQ.X2 .AND. Y1.EQ.Y2) RETURN
      IF (V1.EQ.V2) RETURN
C
C Decode options.
C
      OPTN = INDEX(OPT,'N').NE.0 .OR. INDEX(OPT,'n').NE.0
      FORM =0
      IF (INDEX(OPT,'1').NE.0) FORM = 1
      IF (INDEX(OPT,'2').NE.0) FORM = 2
C
C Choose major interval (DVMAJ in the logarithm, with minimum value
C 1.0 = one decade). The minor interval is always 1.0.
C
      IF (STEP.GT.0.5) THEN
         DVMAJ = NINT(STEP)
      ELSE
         DVMAJ = PGRND(0.20*ABS(V1-V2),NSUBT)
         IF (DVMAJ.LT.1.0) DVMAJ = 1.0
      END IF
      DVMIN = 1.0
      NSUBT = DVMAJ/DVMIN
C
      CALL PGBBUF
      CALL PGQCLP(CLIP)
      CALL PGSCLP(0)
C
C Draw the axis.
C
      CALL PGMOVE(X1, Y1)
      CALL PGDRAW(X2, Y2)
C
C Draw the tick marks. Major ticks are drawn at V = K*DVMAJ.
C
      VMIN = MIN(V1, V2)
      VMAX = MAX(V1, V2)
      K1 = INT(VMIN/DVMIN)
      IF (DVMIN*K1.LT.VMIN) K1 = K1+1
      K2 = INT(VMAX/DVMIN)
      IF (DVMIN*K2.GT.VMAX) K2 = K2-1
      XLAB = (K2-K1) .LE. 2
      DO 20 K=K1,K2
         V = (K*DVMIN-V1)/(V2-V1)
         IF (MOD(K,NSUBT).EQ.0) THEN
C             -- major tick mark
            IF (OPTN) THEN
               CALL PGNUMB(1, NINT(K*DVMIN), FORM, LABEL, LLAB)
            ELSE
               LABEL = ' '
               LLAB = 1
            END IF
            CALL PGTICK(X1, Y1, X2, Y2, V, DMAJL, DMAJR,
     :                  DISP, ORIENT, LABEL(:LLAB))
         ELSE
C             -- minor tick mark
            CALL PGTICK(X1, Y1, X2, Y2, V, DMAJL*FMIN, DMAJR*FMIN,
     :                  0.0, ORIENT, ' ')
         END IF
 20   CONTINUE
C
C Draw intermediate tick marks if required. 
C Label them if axis spans less than 2 decades.
C
      IF (NSUBT.EQ.1) THEN
         DO 30 K=K1-1,K2+1
            DO 25 I=2,9
               V = (K*DVMIN + TAB(I) -V1)/(V2-V1)
               IF (V.GE.0.0 .AND. V.LE.1.0) THEN
                  IF (OPTN.AND.(XLAB .AND.(I.EQ.2 .OR. I.EQ.5))) THEN
C                    -- labeled minor tick mark
                     CALL PGNUMB(I, NINT(K*DVMIN), FORM, LABEL, LLAB)
                  ELSE
C                    -- unlabeled minor tick mark
                     LABEL = ' '
                     LLAB = 1
                  END IF
                  CALL PGTICK(X1, Y1, X2, Y2, V, DMAJL*FMIN, DMAJR*FMIN,
     :                        DISP, ORIENT, LABEL(:LLAB))
               END IF
 25         CONTINUE
 30      CONTINUE
      END IF
C
      CALL PGSCLP(CLIP)
      CALL PGEBUF
      END
