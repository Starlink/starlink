C*PGAXIS -- draw an axis
C%void cpgaxis(const char *opt, float x1, float y1, float x2, float y2, \
C%             float v1, float v2, float step, int nsub, float dmajl, \
C%             float dmajr, float fmin, float disp, float orient);
C+
      SUBROUTINE PGAXIS (OPT, X1, Y1, X2, Y2, V1, V2, STEP, NSUB,
     :                   DMAJL, DMAJR, FMIN, DISP, ORIENT)
      CHARACTER*(*) OPT
      REAL X1, Y1, X2, Y2, V1, V2, STEP, DMAJL, DMAJR, FMIN, DISP
      REAL ORIENT
      INTEGER NSUB
C
C Draw a labelled graph axis from world-coordinate position (X1,Y1) to
C (X2,Y2).
C
C Normally, this routine draws a standard LINEAR axis with equal
C subdivisions.   The quantity described by the axis runs from V1 to V2;
C this may be, but need not be, the same as X or Y. 
C
C If the 'L' option is specified, the routine draws a LOGARITHMIC axis.
C In this case, the quantity described by the axis runs from 10**V1 to
C 10**V2. A logarithmic axis always has major, labeled, tick marks 
C spaced by one or more decades. If the major tick marks are spaced
C by one decade (as specified by the STEP argument), then minor
C tick marks are placed at 2, 3, .., 9 times each power of 10;
C otherwise minor tick marks are spaced by one decade. If the axis
C spans less than two decades, numeric labels are placed at 1, 2, and
C 5 times each power of ten.
C
C If the axis spans less than one decade, or if it spans many decades,
C it is preferable to use a linear axis labeled with the logarithm of
C the quantity of interest.
C
C Arguments:
C  OPT    (input)  : a string containing single-letter codes for
C                    various options. The options currently
C                    recognized are:
C                    L : draw a logarithmic axis
C                    N : write numeric labels
C                    1 : force decimal labelling, instead of automatic
C                        choice (see PGNUMB).
C                    2 : force exponential labelling, instead of
C                        automatic.
C  X1, Y1 (input)  : world coordinates of one endpoint of the axis.
C  X2, Y2 (input)  : world coordinates of the other endpoint of the axis.
C  V1     (input)  : axis value at first endpoint.
C  V2     (input)  : axis value at second endpoint.
C  STEP   (input)  : major tick marks are drawn at axis value 0.0 plus
C                    or minus integer multiples of STEP. If STEP=0.0,
C                    a value is chosen automatically.
C  NSUB   (input)  : minor tick marks are drawn to divide the major
C                    divisions into NSUB equal subdivisions (ignored if
C                    STEP=0.0). If NSUB <= 1, no minor tick marks are
C                    drawn. NSUB is ignored for a logarithmic axis.
C  DMAJL  (input)  : length of major tick marks drawn to left of axis
C                    (as seen looking from first endpoint to second), in
C                    units of the character height.
C  DMAJR  (input)  : length of major tick marks drawn to right of axis,
C                    in units of the character height.
C  FMIN   (input)  : length of minor tick marks, as fraction of major.
C  DISP   (input)  : displacement of baseline of tick labels to
C                    right of axis, in units of the character height.
C  ORIENT (input)  : orientation of label text, in degrees; angle between
C                    baseline of text and direction of axis (0-360°).
C--
C 25-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      REAL V, VMIN, VMAX, DVMAJ, DVMIN
      REAL PGRND
      INTEGER I, K, K1, K2, NSUBT, NV, NP, LLAB, CLIP, FORM
      LOGICAL OPTN, PGNOTO
      CHARACTER CH, LABEL*32
C
C Check arguments.
C
      IF (PGNOTO('PGAXIS')) RETURN
      IF (X1.EQ.X2 .AND. Y1.EQ.Y2) RETURN
      IF (V1.EQ.V2) RETURN
C
C Decode options.
C
      FORM = 0
      OPTN = .FALSE.
      DO 10 I=1,LEN(OPT)
         CH = OPT(I:I)
         CALL GRTOUP(CH, CH)
         IF (CH.EQ.'N') THEN
C           -- numeric labels requested
            OPTN = .TRUE.
         ELSE IF (CH.EQ.'L') THEN
C           -- logarithmic axis requested
            CALL PGAXLG(OPT, X1, Y1, X2, Y2, V1, V2, STEP,
     :                  DMAJL, DMAJR, FMIN, DISP, ORIENT)
            RETURN
         ELSE IF (CH.EQ.'1') THEN
C           -- decimal labels requested
            FORM = 1
         ELSE IF (CH.EQ.'2') THEN
C           -- exponential labels requested
            FORM = 2
         END IF
 10   CONTINUE
C
C Choose major interval if defaulted. Requested interval = STEP,
C with NSUB subdivisions. We will use interval = DVMAJ with NSUBT
C subdivisions of size DVMIN. Note that DVMAJ is always positive.
C
      IF (STEP.EQ.0.0) THEN
          DVMAJ = PGRND(0.20*ABS(V1-V2),NSUBT)
      ELSE
          DVMAJ = ABS(STEP)
          NSUBT = MAX(NSUB,1)
      END IF
      DVMIN = DVMAJ/NSUBT
C
C For labelling, we need to express DVMIN as an integer times a
C power of 10, NV*(10**NP).
C
      NP = INT(LOG10(ABS(DVMIN)))-4
      NV = NINT(DVMIN/10.0**NP)
      DVMIN = REAL(NV)*(10.0**NP)
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
C Draw the tick marks. Minor ticks are drawn at V = K*DVMIN, 
C major (labelled) ticks where K is a multiple of NSUBT.
C
      VMIN = MIN(V1, V2)
      VMAX = MAX(V1, V2)
      K1 = INT(VMIN/DVMIN)
      IF (DVMIN*K1.LT.VMIN) K1 = K1+1
      K2 = INT(VMAX/DVMIN)
      IF (DVMIN*K2.GT.VMAX) K2 = K2-1
      DO 20 K=K1,K2
         V = (K*DVMIN-V1)/(V2-V1)
         IF (MOD(K,NSUBT).EQ.0) THEN
C             -- major tick mark
            IF (OPTN) THEN
               CALL PGNUMB(K*NV, NP, FORM, LABEL, LLAB)
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
      CALL PGSCLP(CLIP)
      CALL PGEBUF
      END
