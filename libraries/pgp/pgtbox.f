C*PGTBOX -- draw frame and write (DD) HH MM SS.S labelling
C%void cpgtbox(const char *xopt, float xtick, int nxsub, \
C% const char *yopt, float ytick, int nysub);
C+
      SUBROUTINE PGTBOX (XOPT, XTICK, NXSUB, YOPT, YTICK, NYSUB)
C
      REAL XTICK, YTICK
      INTEGER NXSUB, NYSUB
      CHARACTER XOPT*(*), YOPT*(*)
C
C Draw a box and optionally label one or both axes with (DD) HH MM SS 
C style numeric labels (useful for time or RA - DEC plots).   If this 
C style of labelling is desired, then PGSWIN should have been called
C previously with the extrema in SECONDS of time.
C
C In the seconds field, you can have at most 3 places after the decimal
C point, so that 1 ms is the smallest time interval you can time label.
C
C Large numbers are coped with by fields of 6 characters long.  Thus 
C you could have times with days or hours as big as 999999.  However, 
C in practice, you might have trouble with labels overwriting  themselves
C with such large numbers unless you a) use a small time INTERVAL, 
C b) use a small character size or c) choose your own sparse ticks in 
C the call to PGTBOX.  
C
C PGTBOX will attempt, when choosing its own ticks, not to overwrite
C the labels, but this algorithm is not very bright and may fail.
C
C Note that small intervals but large absolute times such as
C TMIN = 200000.0 s and TMAX=200000.1 s will cause the algorithm
C to fail.  This is inherent in PGPLOT's use of single precision
C and cannot be avoided.  In such cases, you should use relative
C times if possible.
C
C PGTBOX's labelling philosophy is that the left-most or bottom tick of
C the axis contains a full label.  Thereafter, only changing fields are
C labelled.  Negative fields are given a '-' label, positive fields
C have none.   Axes that have the DD (or HH if the day field is not
C used) field on each major tick carry the sign on each field.  If the
C axis crosses zero, the zero tick will carry a full label and sign.
C
C This labelling style can cause a little confusion with some special
C cases, but as long as you know its philosophy, the truth can be divined.
C Consider an axis with TMIN=20s, TMAX=-20s.   The labels will look like
C
C        +----------+----------+----------+----------+
C     0h0m20s      10s      -0h0m0s      10s        20s
C
C Knowing that the left field always has a full label and that
C positive fields are unsigned, informs that time is decreasing
C from left to right, not vice versa.   This can become very 
C unclear if you have used the 'F' option, but that is your problem !
C
C Exceptions to this labelling philosophy are when the finest time
C increment being displayed is hours (with option 'Y') or days.  
C Then all fields carry a label.  For example,
C
C        +----------+----------+----------+----------+
C      -10h        -8h        -6h        -4h        -2h
C
C
C PGTBOX can be used in place of PGBOX; it calls PGBOX and only invokes 
C time labelling if requested. Other options are passed intact to PGBOX.
C
C Inputs:
C  XOPT   :  X-options for PGTBOX.  Same as for PGBOX plus 
C
C             'Z' for (DD) HH MM SS.S time labelling
C             'Y' means don't include the day field so that labels
C                 are HH MM SS.S rather than DD HH MM SS.S   The hours
C                 will accumulate beyond 24 if necessary in this case.
C             'X' label the HH field as modulo 24.  Thus, a label
C                 such as 25h 10m would come out as 1h 10m
C             'H' means superscript numbers with d, h, m, & s  symbols
C             'D' means superscript numbers with    o, ', & '' symbols 
C             'F' causes the first label (left- or bottom-most) to
C                 be omitted. Useful for sub-panels that abut each other.
C                 Care is needed because first label carries sign as well.
C             'O' means omit leading zeros in numbers < 10
C                 E.g.  3h 3m 1.2s rather than 03h 03m 01.2s  Useful
C                 to help save space on X-axes. The day field does not 
C                 use this facility.
C
C  YOPT   :  Y-options for PGTBOX.  See above.
C  XTICK  :  X-axis major tick increment.  0.0 for default. 
C  YTICK  :  Y-axis major tick increment.  0.0 for default. 
C            If the 'Z' option is used then XTICK and/or YTICK must
C            be in seconds.
C  NXSUB  :  Number of intervals for minor ticks on X-axis. 0 for default
C  NYSUB  :  Number of intervals for minor ticks on Y-axis. 0 for default
C
C  The regular XOPT and YOPT axis options for PGBOX are
C
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
C      The default is to write Y-labels parallel to the axis
C  
C
C        ******************        EXCEPTIONS       *******************
C
C        Note that 
C          1) PGBOX option 'L' (log labels) is ignored with option 'Z'
C          2) The 'O' option will be ignored for the 'V' option as it 
C             makes it impossible to align the labels nicely
C          3) Option 'Y' is forced with option 'D'
C
C        ***************************************************************
C
C
C--
C 05-Sep-1988 - new routine (Neil Killeen)
C 20-Apr-1991 - add support for new DD (day) field and implement
C               labelling on any axis (bottom,top,left,right) [nebk]
C 10-Jun-1993 - add option 'O' for leading zeros, correctly deal with 
C               user ticks, fully support 'V' and 'NM' options, modify
C               slightly meaning of 'F' option [nebk]
C 16-Jan-1995 - add option 'X' [nebk]
C 16-Aug-1996 - Bring axis labelling displacements more in line with 
C               those of pgbox.f [nebk]
C-----------------------------------------------------------------------
      REAL XTICKD, YTICKD, XMIN, XMAX, YMIN, YMAX
      INTEGER IPT, TSCALX, TSCALY, NXSUBD, NYSUBD
      CHARACTER XXOPT*15, YYOPT*15, SUPTYP*4
      LOGICAL XTIME, YTIME, FIRST, DODAYX, DODAYY, DO2, DOPARA, MOD24
C------------------------------------------------------------------------
C
C  Copy inputs
C
      XTICKD = XTICK
      YTICKD = YTICK
      NXSUBD = NXSUB
      NYSUBD = NYSUB
C
C  Get window in world coordinates
C 
      CALL PGQWIN (XMIN, XMAX, YMIN, YMAX)
C
C  X-axis first
C
      CALL GRTOUP (XXOPT, XOPT)
      XTIME = .FALSE.
      IF (INDEX(XXOPT,'Z').NE.0) THEN
C
C  Work out units for labelling and find the tick increments.
C
        IF (ABS(XMAX-XMIN).LT.0.001) THEN
          CALL GRWARN ('PGTBOX: X-axis time interval too small '//
     *                 '(< 1 ms) for time labels')
        ELSE
          XTIME = .TRUE.
          DODAYX = .TRUE.
          IF (INDEX(XXOPT,'Y').NE.0 .OR. INDEX(XXOPT,'D').NE.0) 
     *        DODAYX = .FALSE.
C
          DOPARA = .TRUE.
          CALL PGTBX1 ('X', DODAYX, DOPARA, XMIN, XMAX, XTICKD, 
     *                 NXSUBD, TSCALX)
        END IF
      END IF
C
C  Same again for Y-axis
C
      CALL GRTOUP (YYOPT, YOPT)
      YTIME = .FALSE.
      IF (INDEX(YYOPT,'Z').NE.0) THEN
        IF (ABS(YMAX-YMIN).LT.0.001) THEN
          CALL GRWARN ('PGTBOX: Y-axis time interval too small '//
     *                 '(< 1ms) for time labels')
        ELSE
          YTIME = .TRUE.
          DODAYY = .TRUE.
          IF (INDEX(YYOPT,'Y').NE.0 .OR. INDEX(YYOPT,'D').NE.0)
     *        DODAYY = .FALSE.
C
          DOPARA = .TRUE.
          IF (INDEX(YYOPT,'V').NE.0) DOPARA = .FALSE.
C
          CALL PGTBX1 ('Y', DODAYY, DOPARA, YMIN, YMAX, YTICKD, 
     *                 NYSUBD, TSCALY)
        END IF
      END IF
C
C  Parse options list.  For call to PGBOX when doing time labelling, we 
C  don't want L (log), N or M (write numeric labels). 
C
      IF (XTIME) THEN
        IPT = INDEX(XXOPT,'L')
        IF (IPT.NE.0) XXOPT(IPT:IPT) = ' '
        IPT = INDEX(XXOPT,'N')
        IF (IPT.NE.0) XXOPT(IPT:IPT) = ' '
        IPT = INDEX(XXOPT,'M')
        IF (IPT.NE.0) XXOPT(IPT:IPT) = ' '
      END IF
C
      IF (YTIME) THEN
        IPT = INDEX(YYOPT,'L')
        IF (IPT.NE.0) YYOPT(IPT:IPT) = ' '
        IPT = INDEX(YYOPT,'N')
        IF (IPT.NE.0) YYOPT(IPT:IPT) = ' '
        IPT = INDEX(YYOPT,'M')
        IF (IPT.NE.0) YYOPT(IPT:IPT) = ' '
      END IF
C
C  Draw box and ticks
C
      CALL PGBOX (XXOPT, XTICKD, NXSUBD, YYOPT, YTICKD, NYSUBD)
C
C  Add (DD) HH MM SS labels if desired.  Go back to the original user
C  specified options list.
C
      XXOPT = ' '
      CALL GRTOUP (XXOPT, XOPT)
      IF (XTIME .AND. (INDEX(XXOPT,'N').NE.0 .OR.
     *                 INDEX(XXOPT,'M').NE.0)) THEN
        FIRST = .TRUE.
        IF (INDEX(XXOPT,'F').NE.0) FIRST = .FALSE.
C
        SUPTYP = 'NONE'
        IF (INDEX(XXOPT,'D').NE.0) SUPTYP = ' DMS'
        IF (INDEX(XXOPT,'H').NE.0) SUPTYP = 'DHMS'
C
        DO2 = .TRUE.
        IF (INDEX(XXOPT,'O').NE.0) DO2 = .FALSE.
C
        DOPARA = .TRUE.
C
        MOD24 = .FALSE.
        IF (INDEX(XXOPT,'X').NE.0) MOD24 = .TRUE.
C
        IF (INDEX(XXOPT,'N').NE.0)
     *    CALL PGTBX4 (DODAYX, SUPTYP, 'X', .TRUE., FIRST, 
     *      XMIN, XMAX, TSCALX, XTICKD, DO2, DOPARA, MOD24)
C
        IF (INDEX(XXOPT,'M').NE.0)
     *    CALL PGTBX4 (DODAYX, SUPTYP, 'X', .FALSE., FIRST, 
     *       XMIN, XMAX, TSCALX, XTICKD, DO2, DOPARA, MOD24)
      END IF
C
      YYOPT = ' '
      CALL GRTOUP (YYOPT, YOPT)
      IF (YTIME .AND. (INDEX(YYOPT,'N').NE.0 .OR.
     *                 INDEX(YYOPT,'M').NE.0)) THEN
        FIRST = .TRUE.
        IF (INDEX(YYOPT,'F').NE.0) FIRST = .FALSE.
C
        SUPTYP = 'NONE'
        IF (INDEX(YYOPT,'D').NE.0) SUPTYP = ' DMS'
        IF (INDEX(YYOPT,'H').NE.0) SUPTYP = 'DHMS'
C
        DOPARA = .TRUE.
        IF (INDEX(YYOPT,'V').NE.0) DOPARA = .FALSE.
C
        DO2 = .TRUE.
        IF (DOPARA .AND. INDEX(YYOPT,'O').NE.0) DO2 = .FALSE.
C
        MOD24 = .FALSE.
        IF (INDEX(YYOPT,'X').NE.0) MOD24 = .TRUE.
C
        IF (INDEX(YYOPT,'N').NE.0)
     *    CALL PGTBX4 (DODAYY, SUPTYP, 'Y', .TRUE., FIRST, 
     *       YMIN, YMAX, TSCALY, YTICKD, DO2, DOPARA, MOD24)
C
        IF (INDEX(YYOPT,'M').NE.0)
     *    CALL PGTBX4 (DODAYY, SUPTYP, 'Y', .FALSE., FIRST, 
     *       YMIN, YMAX, TSCALY, YTICKD, DO2, DOPARA, MOD24)
C
      END IF
C
      RETURN
      END
C PGTBX1 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX1 (AXIS, DODAY, DOPARA, TMIN, TMAX, TICK, 
     *                   NSUB, TSCALE)
C
      REAL TMIN, TMAX, TICK
      INTEGER NSUB, TSCALE
      LOGICAL DODAY, DOPARA
      CHARACTER AXIS*1
C
C Work out what the finest units the time labels will be in and
C return the tick increments if the user does not set them.
C
C This is a support routine for PGTBOX and should not 
C be called by the user.
C
C Input:
C  AXIS   :  'X' or 'Y' for use in determining if labels overwrite
C  TMIN   :  Start time in seconds 
C  TMAX   :  End   time in seconds
C  DOPARA :  True if label to be parallel to axis, else perpendicular
C Input/output:
C  DODAY  :  Write labels as DD HH MM SS.S else HH MM SS.S with
C            hours ranging above 24.  Useful for declination labels
C  TICK   :  Major tick interval in seconds.  If 0.0 on input, will 
C            be set here.
C  NSUB   :  Number of minor ticks between major ticks. If 0 on input
C            will be set here.
C Outputs:
C  TSCALE :  Determines finest unit of labelling 
C            (1 => ss, 60 => mm, 3600 => hh, 3600*24 => dd)
C
C 05-Sep-1988 - new routine (Neil Killeen)
C 08-Apr-1991 - correctly work out HH MM SS when the time > 60 h [nebk]
C 20-Apr-1991 - revise to add support for new DD (day) field and
C               do lots of work on tick algorithm [nebk]
C 10-Jun-1993 - deal with user given ticks & rename from PGTIME [nebk/jm]
C-----------------------------------------------------------------------
      INTEGER NLIST1, NLIST2, NLIST3, NLIST4, NTICMX
      PARAMETER (NLIST1 = 19, NLIST2 = 10, NLIST3 = 6, NLIST4 = 8,
     *           NTICMX = 8)
C
      REAL TICKS1(NLIST1), TICKS2(NLIST2), TICKS3(NLIST3), 
     *TICKS4(NLIST4), TOCK, TOCK2, TINT, TINTS, TMINS, TMAXS
      INTEGER NSUBS1(NLIST1), NSUBS2(NLIST2), NSUBS3(NLIST3), 
     *NSUBS4(NLIST4), NPL, NTICK, ITICK, STRLEN
      CHARACTER STR*15
C
      SAVE TICKS1, TICKS2, TICKS3, TICKS4
      SAVE NSUBS1, NSUBS2, NSUBS3, NSUBS4
C
      DATA TICKS1 /0.001,  0.002,                 0.005,
     *             0.01,   0.02,                  0.05,  
     *             0.1,    0.2,                   0.5,  
     *             1.0,    2.0,   3.0,    4.0,    5.0,
     *             6.0,   10.0,  15.0,   20.0,   30.0/
      DATA NSUBS1 / 4,      4,                     2,    
     *              4,      4,                     2,    
     *              4,      4,                     2,    
     *              4,      4,     3,      4,      5,
     *              3,      2,     3,      2,      3/
C
      DATA TICKS2 /1.0,    2.0,   3.0,    4.0,    5.0,
     *             6.0,   10.0,  15.0,   20.0,   30.0/
      DATA NSUBS2 / 4,      4,     3,      4,      5,
     *              3,      2,     3,      2,      3/
C
      DATA TICKS3 /1.0,    2.0,   3.0,    4.0,    6.0,   12.0/
      DATA NSUBS3 / 4,      4,     3,      4,      3,      2/
C
      DATA TICKS4 /1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0, 9.0/
      DATA NSUBS4 / 4,   4,   3,   4,   5,   3,   4,   3 /
C----------------------------------------------------------------------
C
C  Turn off DD (day) field if it has been unnecessarily asked for
C
      IF ((ABS(TMIN).LT.24.0*3600.0) .AND. (ABS(TMAX).LT.24.0*3600.0))
     *   DODAY = .FALSE.
C
C  If a tick size is provided, use it to determine TSCALE
C
      TINT = ABS(TMAX - TMIN)
      TICK = ABS(TICK)
      IF (TICK.NE.0.0) THEN
        IF (TICK.GE.TINT) THEN
          CALL GRWARN ('PGTBX1: user given tick bigger than time '
     *                 //'interval; will auto-tick')
          TICK = 0.0
        ELSE IF (TICK.LT.0.001) THEN
          CALL GRWARN ('PGTBX1: user given tick too small (< 1 ms); '
     *                 //'will auto-tick')
          TICK = 0.0
        ELSE 
          IF (MOD(TICK, 60.0) .NE. 0.0) THEN
            TSCALE = 1
          ELSE IF (MOD(TICK, 3600.0).NE.0.0) THEN
            TSCALE = 60
          ELSE IF (.NOT.DODAY) THEN
            TSCALE = 3600
          ELSE IF (MOD(TICK,(24.0*3600.0)).NE.0.0) THEN
            TSCALE = 3600
          ELSE
            TSCALE = 24 * 3600
          ENDIF
C
C  Make a simple default for the number of minor ticks and bug out
C
          IF (NSUB.EQ.0) NSUB = 2
          RETURN
        END IF
      END IF
C
C  Work out label units depending on time interval if user 
C  wants auto-ticking
C
      IF (TINT.LE.5*60) THEN
        TSCALE = 1
      ELSE IF (TINT.LE.5*3600) THEN
        TSCALE = 60
      ELSE 
        IF (.NOT.DODAY) THEN
          TSCALE = 3600
        ELSE
          IF (TINT.LE.5*24*3600) THEN
            TSCALE = 3600
          ELSE
            TSCALE = 3600*24
          END IF
        END IF
      END IF
C
CCCCC
C  Divide interval into NTICK major ticks and NSUB minor intervals
C  The tick choosing algorithm is not very robust, so watch out
C  if you fiddle anything. 
CCCCC
C
      TINTS = TINT / TSCALE
      IF (TSCALE.EQ.1) THEN
C
C  Time in seconds.  If the time interval is very small, may need to 
C  label with up to 3 decimal places.  Have less ticks to help prevent
C  label overwrite. STR is a dummy tick label to assess label 
C  overwrite potential
C
        IF (DOPARA) THEN
          IF (TINTS.LE.0.01) THEN
            NTICK = 4
            STR = '60.423'
            STRLEN = 6
          ELSE IF (TINTS.LE.0.1) THEN
            NTICK = 5
            STR = '60.42'
            STRLEN = 5
          ELSE IF (TINTS.LE.1.0) THEN
            NTICK = 6
            STR = '60.4'
            STRLEN = 4
          ELSE
            NTICK = 6
            STR = '60s'
            STRLEN = 3
          END IF
        ELSE
          NTICK = 6
          STR = ' '
          STRLEN = 1
        END IF
        TOCK = TINTS / NTICK
C
C  Select nearest tick to TOCK from list.
C
        CALL PGTBX2 (TOCK, NLIST1, TICKS1, NSUBS1, TICK, NSUB, ITICK)
C
C  Check label overwrite and/or too many ticks.
C
        CALL PGTBX3 (DODAY, 0, TSCALE, TINTS, NTICMX, NLIST1, TICKS1,
     *               NSUBS1, ITICK, AXIS, DOPARA, STR(1:STRLEN),
     *               TICK, NSUB)
      ELSE IF (TSCALE.EQ.60) THEN
C
C  Time in minutes 
C
        NTICK = 6
        TOCK = TINTS / NTICK
C
C  Select nearest tick from list
C
        CALL PGTBX2 (TOCK, NLIST2, TICKS2, NSUBS2, TICK, NSUB, ITICK)
C
C  Check label overwrite and/or too many ticks.
C
        IF (DOPARA) THEN
          STR = '42m'
          STRLEN = 3
        ELSE
          STR = ' '
          STRLEN = 1
        END IF
        CALL PGTBX3 (DODAY, 0, TSCALE, TINTS, NTICMX, NLIST2, TICKS2,
     *               NSUBS2, ITICK, AXIS, DOPARA, STR(1:STRLEN),
     *               TICK, NSUB)
      ELSE 
        IF (TSCALE.EQ.3600 .AND. DODAY) THEN
C
C  Time in hours with the day field 
C
          NTICK = 6
          TOCK = TINTS / NTICK
C
C  Select nearest tick from list
C
          CALL PGTBX2 (TOCK, NLIST3, TICKS3, NSUBS3, TICK, NSUB, ITICK)
C
C   Check label overwrite and/or too many ticks.
C
          IF (DOPARA) THEN
            STR = '42h'
            STRLEN = 3
          ELSE
            STR = ' '
            STRLEN = 1
          END IF
          CALL PGTBX3 (DODAY, 0, TSCALE, TINTS, NTICMX, NLIST3, TICKS3,
     *                 NSUBS3, ITICK, AXIS, DOPARA, STR(1:STRLEN),
     *                 TICK, NSUB)
        ELSE
C
C  Time in hours with no day field or time in days. Have less
C  ticks for big numbers or the parallel labels will overwrite.

          IF (DOPARA) THEN
            TMINS = ABS(TMIN) / TSCALE
            TMAXS = ABS(TMAX) / TSCALE            
            CALL PGNPL (-1, NINT(MAX(TINTS,TMINS,TMAXS)), NPL)
            IF (NPL.LE.3) THEN
              NTICK = 6
            ELSE IF (NPL.EQ.4) THEN
              NTICK = 5
            ELSE
              NTICK = 4
            END IF
            STR = '345678912'
            STR(NPL+1:) = 'd'
            STRLEN = NPL + 1
          ELSE
            STR = ' '
            STRLEN = 1
            NTICK = 6
          END IF
          TOCK = TINTS / NTICK
C
C   Select nearest tick from list; 1 choose nearest nice integer 
C   scaled by the appropriate power of 10
C
          CALL PGNPL (-1, NINT(TOCK), NPL)
          TOCK2 = TOCK / 10**(NPL-1)
C
          CALL PGTBX2 (TOCK2, NLIST4, TICKS4, NSUBS4, TICK, NSUB, ITICK)
          TICK = TICK * 10**(NPL-1)
C
C  Check label overwrite and/or too many ticks.
C
          CALL PGTBX3 (DODAY, NPL, TSCALE, TINTS, NTICMX, NLIST4, 
     *                 TICKS4, NSUBS4, ITICK, AXIS, DOPARA,
     *                 STR(1:STRLEN), TICK, NSUB)
        END IF
      END IF
C
C  Convert tick to seconds
C
      TICK = TICK * TSCALE
C
      RETURN
      END
C PGTBX2 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX2 (TOCK, NTICKS, TICKS, NSUBS, TICK, NSUB, ITICK)
C
      INTEGER NTICKS, NSUBS(NTICKS), NSUB, ITICK
      REAL TOCK, TICKS(NTICKS), TICK
C
C Find the nearest tick in a list to a given value.
C
C This is a support routine for PGTBOX and should not be called
C by the user.
C
C Input:
C  TOCK   :  Try to find the nearest tick in the list to TOCK
C  NTICKS :  Number of ticks in list
C  TICKS  :  List of ticks
C  NSUBS  :  List of number of minor ticks between ticks to go with TICKS
C Output:
C  TICK   :  The selected tick
C  ITICK  :  The index of the selected tick from the list TICKS
C Input/output
C  NSUB   :  Number of minor ticks between major ticks. If 0 on input
C            will be set here.
C
C 10-Jun-1993 - new routine [nebk]
C-----------------------------------------------------------------------
      INTEGER I, NSUBD
      REAL DMIN, DIFF
C----------------------------------------------------------------------
      NSUBD = NSUB
      DMIN = 1.0E30
      DO 100 I = 1, NTICKS
        DIFF = ABS(TOCK - TICKS(I))
        IF (DIFF.LT.DMIN) THEN
          TICK = TICKS(I)
          IF (NSUBD.EQ.0) NSUB = NSUBS(I)
          ITICK = I
C
          DMIN = DIFF
        END IF
 100  CONTINUE
C
      RETURN
      END
C PGTBX3 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX3 (DODAY, NPL, TSCALE, TINTS, NTICMX, NTICKS,
     *                   TICKS, NSUBS, ITICK, AXIS, DOPARA, STR,
     *                   TICK, NSUB)
C
      INTEGER TSCALE, NTICMX, NTICKS, ITICK, NSUB, NSUBS(NTICKS), NPL
      REAL TINTS, TICKS(NTICKS), TICK
      CHARACTER AXIS*1, STR*(*)
      LOGICAL DODAY, DOPARA
C
C Try to see if label overwrite is going to occur with this tick 
C selection, or if there are going to be more than a reasonable
C number of ticks in the displayed time range.  If so, choose, 
C if available, the next tick (bigger separation) up in the list.
C If the overwrite requires that we would need to go up to the bext
C TSCALE, give up.  They will need to choose a smaller character size
C
C This is a support routine for PGTBOX and should not 
C be called by the user.
C
C Input:
C  DODAY  :  True if day field being used
C  NPL    :  Number of characters needed to format TICK on input
C  TSCALE :  Dictates what the finest units of the labelling are.
C            1 = sec, 60 = min, 3600 = hr, 24*3600 = days
C  TINTS  :  Absolute time interval in units of TSCALE
C  NTICMX :  Max. reasonable number of ticks to allow in the time range
C  NTICKS :  Number of ticks in list of ticks to choose from
C  TICKS  :  List of ticks from which the current tick was chosen
C  NSUBS  :  List of number of minor ticks/major tick to choose NSUB from
C  ITICK  :  Index of chosen tick in list TICKS
C  AXIS   :  'X' or 'Y' axis
C  DOPARA :  Labels parallel or perpendicualr to axis
C  STR    :  A typical formatted string used for checking overwrite
C Input/output:
C  TICK   :  Current major tick interval in units of TSCALE. May be 
C            made larger if possible if overwrite likely.
C  NSUB   :  Number of minor ticks between major ticks. 
C
C 10-Jun-1993 - new routine [nebk]
C-----------------------------------------------------------------------
      INTEGER NTICK
      REAL LENS, LENX, LENY
C----------------------------------------------------------------------
      CALL PGLEN (4, STR, LENX, LENY)
      LENS = LENX
      IF ( (DOPARA .AND. AXIS.EQ.'Y') .OR.
     *     (.NOT.DOPARA .AND. AXIS.EQ.'X') ) LENS = LENY
C
      IF (TSCALE.EQ.1 .OR. TSCALE.EQ.60 .OR.
     *    (TSCALE.EQ.3600 .AND. DODAY)) THEN
C
C  Time in seconds or minutes, or in hours with a day field
C
        NTICK = INT(TINTS / TICK)
        IF ( (ITICK.LT.NTICKS)  .AND. 
     *       ((DOPARA .AND. (LENS/TSCALE).GT.0.9*TICK) .OR. 
     *       (NTICK.GT.NTICMX)) ) THEN
          IF (TICKS(ITICK+1).LT.TINTS) THEN
            NSUB = NSUBS(ITICK+1)
            TICK = TICKS(ITICK+1)
          END IF
        END IF
      ELSE
C
C  Time in hours and no day field or time in days
C
        NTICK = INT(TINTS / TICK)
        IF ( (DOPARA .AND. (LENS/TSCALE).GT.0.9*TICK) .OR. 
     *       (NTICK.GT.NTICMX) ) THEN
          IF (ITICK.LT.NTICKS) THEN
            IF (TICKS(ITICK+1)*10**(NPL-1).LT.TINTS) THEN
              NSUB = NSUBS(ITICK+1)
              TICK = TICKS(ITICK+1) * 10**(NPL-1)
            END IF
          ELSE
            IF (TICKS(1)*10**NPL.LT.TINTS) THEN
              NSUB = NSUBS(1)
              TICK = TICKS(1) * 10**NPL
            END IF
          END IF
        END IF
      END IF
C
      RETURN
      END
C PGTBX4 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX4 (DODAY, SUPTYP, AXIS, CONVTL, FIRST, TMIN,
     *                   TMAX, TSCALE, TICK, DO2, DOPARA, MOD24)
C
      REAL TMIN, TMAX, TICK
      INTEGER TSCALE
      CHARACTER AXIS*(*), SUPTYP*(*)
      LOGICAL FIRST, DODAY, CONVTL, DO2, DOPARA, MOD24
C
C Label an axis in (DD) HH MM SS.S style.    This is the main 
C workhorse of the PGTBOX routines.
C
C This is a support subroutine for PGTBOX and should not be 
C called by the user. 
C
C Inputs:
C  DODAY  :  Write labels as DD HH MM SS.S else HH MM SS.S with
C            hours ranging above 24.  Useful for declination labels
C  SUPTYP :  If 'DHMS' then superscript the fields with d, h, m, & s
C            If ' DMS' then superscript the fields with    o, '  & '' 
C              Good for declination plots.  You should obviously not 
C              ask for the day field for this to do anything sensible. 
C            If '    ' then no superscripting is done.
C  AXIS   :  'X' for x-axis, 'Y' for y-axis
C  CONVTL :  If .true., write the labels in the conventional axis 
C            locations (bottom and left for 'X' and 'Y').  Otherwise
C            write them on the top and right axes ('X' and 'Y')
C  FIRST  :  If .false. then omit the first label.
C  TMIN   :  Start time (seconds)
C  TMAX   :  End time (seconds)
C  TSCALE :  Determines finest units of axis
C              1 => ss, 60 => mm, 3600 => hh, 3600*24 => dd
C  TICK   :  Major tick interval in seconds
C  DO2    :  If .true., write labels less than 10 with a leading zero.
C  DOPARA :  Y axis label parallel to axis, else perpendicular
C  MOD24  :  HH field labelled as modulo 24
C
C 05-Sep-1988 - new routine (Neil Killeen)
C 20-Apr-1991 - add support for new DD (day) field [nebk]
C 10-Jun-1993 - complete rewrite & rename from PGTLAB. Fixes user given 
C               ticks bug too [nebk]
C 15-Jan-1995 - Add argument MOD24
C-----------------------------------------------------------------------
      INTEGER MAXTIK
      LOGICAL T, F
      PARAMETER (MAXTIK = 1000, T = .TRUE., F = .FALSE.)
C
      REAL SS(MAXTIK), TFRAC(MAXTIK)
      INTEGER DD(MAXTIK), HH(MAXTIK), MM(MAXTIK)
      CHARACTER*1 ASIGN(MAXTIK), ASIGNL
C
      REAL TIME, XLEN, YLEN, COORD, FJUST, RVAL, SSL, DISP,
     *XLEN2, YLEN2
      INTEGER IS, SD, NT, IZERO, IPOS, INEG, IT, I, J, K, SPREC,
     *JST(2), JEND(2), TLEN, LAST, IVAL(3), IVALO(3), IVALZ(3),
     *IVALF(3), IVALL(3), NPASS, INC, DDL, HHL, MML
      CHARACTER SIGNF*1, TEXT*80, AXLOC*2
      LOGICAL WRIT(4)
C-----------------------------------------------------------------------
      CALL PGBBUF
C
C  Direction signs
C
      SD = 1
      IF (TMAX.LT.TMIN) SD = -1
      IS = 1
      IF (TMIN.LT.0.0) IS = -1
C
C  Find first tick.  Return if none.
C
      NT = TMIN / TICK
      IF (IS*SD.EQ.1 .AND. ABS(TMIN).GT.ABS(NT)*TICK) NT = NT + SD
      TIME = NT * TICK
      IF ( (SD.EQ. 1.AND.(TIME.LT.TMIN.OR.TIME.GT.TMAX)) .OR.
     *     (SD.EQ.-1.AND.(TIME.GT.TMIN.OR.TIME.LT.TMAX)) ) RETURN
C
C  Now step through time range in TICK increments and convert
C  times in seconds at each tick to  +/- (DD) HH MM SS.S
C
      IZERO = 0
      IT = 1
 100  IF ( (SD.EQ.1  .AND. TIME.GT.(TMAX+1.0E-5)) .OR.
     *     (SD.EQ.-1 .AND. TIME.LT.(TMAX-1.0E-5)) ) GOTO 200
        IF (IT.GT.MAXTIK) THEN
          CALL GRWARN ('PGTBX4: storage exhausted -- you have'
     *                 //'asked for far too many ticks')
          GOTO 200
        END IF
C
C  Convert to (DD) HH MM SS.S and find fraction of window that this
C  tick falls at
C
        CALL PGTBX5 (DODAY, TIME, ASIGN(IT), DD(IT), HH(IT),
     *               MM(IT), SS(IT))
        TFRAC(IT) = (TIME - TMIN) / (TMAX - TMIN)
C
C  Note zero tick
C
        IF (NT.EQ.0) IZERO = IT
C
C  Increment time
C
        NT = NT + SD
        TIME = NT * TICK
        IT = IT + 1
C
        GOTO 100
 200  CONTINUE
      IT = IT - 1
C
C   Work out the precision with which to write fractional seconds 
C   labels into the SS.S field.   All other fields have integer labels.
C
      SPREC = 0
      IF (TSCALE.EQ.1) THEN
        IF (TICK.LT.0.01) THEN
          SPREC = 3
        ELSE IF (TICK.LT.0.1) THEN
          SPREC = 2
        ELSE IF (TICK.LT.1.0) THEN
          SPREC = 1
        END IF
      END IF
C
C  Label special case of first tick.  Prepare fields and label
C
      CALL PGTBX6 (DODAY, MOD24, TSCALE, DD(1), HH(1), MM(1), 
     *             SS(1), IVALF, RVAL, WRIT)
      SIGNF = 'H'
      IF (DODAY) SIGNF = 'D'
      CALL PGTBX7 (SUPTYP, SIGNF, ASIGN(1), IVALF, RVAL, WRIT,
     *             SPREC, DO2, TEXT, TLEN, LAST)
C
C   Set label displacements from axes.  This is messy for labels oriented
C   perpendicularly on the right hand axis as we need to know how long
C   the longest string we are going to write is before we write any 
C   labels as they are right justified.
C
      IF (AXIS.EQ.'X') THEN
        IF (CONVTL) THEN
          AXLOC = 'B'
          IF (SUPTYP.NE.'NONE') THEN
            DISP = 1.4
          ELSE
            DISP = 1.2
          END IF
        ELSE
          AXLOC = 'T'
          DISP = 0.7
        END IF
      ELSE IF (AXIS.EQ.'Y') THEN
        IF (CONVTL) THEN
          AXLOC = 'LV'
          IF (DOPARA) AXLOC = 'L'
          DISP = 0.7
        ELSE
          IF (DOPARA) THEN
            AXLOC = 'R'
            IF (SUPTYP.NE.'NONE') THEN
              DISP = 1.7
            ELSE
              DISP = 1.9
            END IF
          ELSE
C
C  Work out number of characters in first label
C
            AXLOC = 'RV'
            IF (ASIGN(1).NE.'-' .AND. TMIN*TMAX.LT.0.0) THEN
              CALL PGLEN (2, ' -'//TEXT(1:TLEN), XLEN, YLEN)
            ELSE
              CALL PGLEN (2, ' '//TEXT(1:TLEN), XLEN, YLEN)
            END IF
            CALL PGQCS (2, XLEN2, YLEN2)
            DISP = (XLEN/XLEN2)
          END IF
        END IF
      END IF
C
C  Now write the label to the plot.  The X-axis label for the first tick is
C  centred such that the last field of the label is centred on the tick
C
      IF (FIRST) THEN
        CALL PGLEN (5, TEXT(LAST:TLEN), XLEN, YLEN)
C
        IF (AXIS.EQ.'X') THEN
          COORD = TFRAC(1) + XLEN / 2.0
          FJUST = 1.0
        ELSE IF (AXIS.EQ.'Y') THEN
          IF (DOPARA) THEN
            COORD = TFRAC(1) + YLEN / 2.0
            FJUST = 1.0
          ELSE
            FJUST = 1.0
            COORD = TFRAC(1)
          END IF
        END IF
        CALL PGMTXT (AXLOC, DISP, COORD, FJUST, TEXT(1:TLEN))
      END IF
      IF (IT.EQ.1) RETURN
C
C   Designate which field out of DD or HH will carry the sign, depending
C   on whether you want the day field or not for the rest of the ticks
C
      SIGNF = 'H'
      IF (DODAY) SIGNF = 'D'
C
C  Set up labelling justifications for the rest of the labels
C
      IF (AXIS.EQ.'X') THEN
        FJUST = 0.5
      ELSE IF (AXIS.EQ.'Y') THEN
        IF (DOPARA) THEN
          FJUST = 0.5
        ELSE
          FJUST = 1.0
        END IF
      END IF
C
C  Note zero crossings; IPOS is the first positive tick and
C  INEG is the first negative tick on either side of 0
C
      IPOS = 0
      INEG = 0
C
      IF (IZERO.NE.0) THEN
        J = IZERO - 1
        IF (J.GE.1) THEN
          IF (ASIGN(J).EQ.'-') THEN
            INEG = J
          ELSE IF (ASIGN(J).EQ.' ') THEN
            IPOS = J
          END IF
        END IF
        J = IZERO + 1
        IF (J.LE.IT) THEN
          IF (ASIGN(J).EQ.'-') THEN
            INEG = J
          ELSE IF (ASIGN(J).EQ.' ') THEN
            IPOS = J
          END IF
        END IF
      END IF
C
C  Now label special case of zero tick. It carries the sign change
C  when going from positive to negative time, left to right.
C
      IF (IZERO.NE.0 .AND. IZERO.NE.1) THEN
        CALL PGTBX6 (DODAY, MOD24, TSCALE, DD(IZERO), HH(IZERO), 
     *               MM(IZERO), SS(IZERO), IVALZ, RVAL, WRIT)
C
        IF (ASIGN(IZERO-1).EQ.' ') ASIGN(IZERO) = '-'
        CALL PGTBX7 (SUPTYP, SIGNF, ASIGN(IZERO), IVALZ, RVAL, WRIT,
     *               SPREC, DO2, TEXT, TLEN, LAST)
C
        COORD = TFRAC(IZERO)
        CALL PGMTXT (AXLOC, DISP, COORD, FJUST, TEXT(1:TLEN))
      END IF
C
C   We may need an extra "virtual" tick if there is no zero crossing
C   and SD=-1 & IS=1 or SD=1 & IS=-1.  It is used to work out which
C   fields to label on the right most tick which is labelled first.
C
      IF (IZERO.EQ.0) THEN
        IF (SD*IS.EQ.-1) THEN 
          IF ( (SD.EQ.-1 .AND. TIME.LE.0.0) .OR.
     *         (SD.EQ. 1 .AND. TIME.GE.0.0) ) TIME = 0.0
          CALL PGTBX5 (DODAY, TIME, ASIGNL, DDL, HHL, MML, SSL)
          CALL PGTBX6 (DODAY, MOD24, TSCALE, DDL, HHL, MML, SSL,
     *                 IVALL, RVAL, WRIT)
        END IF
      END IF
C
C  We want to label in the direction(s) away from zero, so we may  need
C  two passes. Determine the start and end ticks for each required pass.
C
      JST(2) = 0
      JEND(2) = 0
      NPASS = 1
      IF (IZERO.EQ.0) THEN
        IF (IS*SD.EQ.1) THEN
          JST(1) = 1
          JEND(1) = IT
        ELSE
          JST(1) = IT
          JEND(1) = 1
        END IF
      ELSE
        IF (INEG.EQ.0 .OR. IPOS.EQ.0) THEN
          JST(1) = IZERO
          JEND(1) = IT
          IF (IZERO.EQ.IT) JEND(1) = 1
        ELSE
          NPASS = 2
          JST(1) = IZERO
          JEND(1) = 1
          JST(2) = IZERO
          JEND(2) = IT
        END IF
      END IF
C
C  Now label the rest of the ticks.  Always label away from 0
C
      DO 400 I = 1, NPASS
C
C  Initialize previous tick values.  Use virtual tick if labelling
C  left to right without a zero (one pass)
C
        DO 250 K = 1, 3
          IVALO(K) = IVALZ(K)
          IF (IZERO.EQ.0) THEN
            IVALO(K) = IVALL(K)
            IF (JST(I).EQ.1) IVALO(K) = IVALF(K)
          END IF
  250   CONTINUE
C
        INC = 1
        IF (JEND(I).LT.JST(I)) INC = -1
        DO 300 J = JST(I), JEND(I), INC
C
C  First and zero tick already labelled
C
          IF (J.NE.1 .AND. J.NE.IZERO) THEN
C
C  Prepare fields
C
            CALL PGTBX6 (DODAY, MOD24, TSCALE, DD(J), HH(J), MM(J),
     *                   SS(J), IVAL, RVAL, WRIT)
C
C  Don't write unchanging fields
C
            DO 275 K = 1, 3
              IF (IVAL(K).EQ.IVALO(K)) WRIT(K) = F
 275        CONTINUE
C
C  Prepare label
C
            CALL PGTBX7 (SUPTYP, SIGNF, ASIGN(J), IVAL, RVAL, WRIT,
     *                   SPREC, DO2, TEXT, TLEN, LAST)
C
C  Write label
C
            COORD = TFRAC(J)
            CALL PGMTXT (AXLOC, DISP, COORD, FJUST, TEXT(1:TLEN))
C
C  Update old values
C
            DO 280 K = 1, 3
              IVALO(K) = IVAL(K)
  280       CONTINUE
          END IF
 300    CONTINUE
 400  CONTINUE
      CALL PGEBUF
C 
      RETURN
      END
C PGTBX5 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX5 (DODAY, TSEC, ASIGN, D, H, M, S)
C      
      REAL S, TSEC
      INTEGER  D, H, M
      LOGICAL DODAY
      CHARACTER*1 ASIGN
C
C  Convert time in seconds to (DD) HH MM SS.S
C
C Input
C  DODAY  :  Use day field if true, else hours accumulates beyond 24
C  TSEC   :  Time in seconds (signed)
C Output
C  ASIGN  :  Sign, ' ' or '-'
C  D,H,M  :  DD, HH, MM (unsigned)
C  S      :  SS.S       (unsigned)
C
C 10-Jun-1993 - new routine [nebk]
C-----------------------------------------------------------------------
      INTEGER IT
C----------------------------------------------------------------------
      ASIGN = ' '
      IF (TSEC.LT.0.0) ASIGN = '-'
C
      S = MOD(ABS(TSEC),60.0)
C
      IT = NINT(ABS(TSEC)-S) / 60
      M = MOD(IT,60)
C
      IT = (IT - M) / 60
      IF (DODAY) THEN
        H = MOD(IT,24)
        D = (IT-H) / 24
      ELSE
        H = IT
        D = 0
      END IF
C
      RETURN
      END
C PGTBX6 -- support routine for PGTBOX
C
      SUBROUTINE PGTBX6 (DODAY, MOD24, TSCALE, DD, HH, MM, SS, IVAL, 
     *                   RVAL, WRIT)
C
      INTEGER TSCALE, IVAL(3), DD, HH, MM
      REAL SS, RVAL
      LOGICAL WRIT(4), DODAY, MOD24
C
C   Find out which of the DD HH MM SS.S fields we want to write
C   into the label according to TSCALE and make a round off
C   error check.
C
C  Input:
C    DODAY  :  Use day field if true else hours accrue beyond 24
C    MOD24  :  HH field labelled as modulo 24
C    TSCALE :  Dictates which fields appear in labels
C    DD     :  Day of time  (will be 0 if DODAY=F and HH will compensate)
C    HH     :  Hour of time
C    MM     :  Minute of time
C    SS     :  Second of time
C  Output:
C    IVAL(3):  DD HH MM to write into label
C    RVAL   :  SS.S to write into label
C    WRIT(4):  T or F if DD,HH,MM,SS are to be written into the label
C              or not.  IVAL and RVAL fields are set explicitly to
C              zero if the corresponding WRIT field is false.
C              This really is overkill.
C
C  10-Jun-1993 - New routine [nebk]
C  16-Jan-1995 - Add argument MOD24
C-----------------------------------------------------------------------
      LOGICAL T, F
      PARAMETER (T = .TRUE., F = .FALSE.)
      INTEGER WM
C-----------------------------------------------------------------------
      IVAL(1) = DD
      IVAL(2) = HH
      IVAL(3) = MM
      RVAL    = SS
C
C  SS should be 0.0; round off may get us 59.999 or the like but
C  not 60.001 (see PGTBX5)
C
      IF (TSCALE.GT.1) THEN
        WM = NINT(SS/60.0)
        IVAL(3) = IVAL(3) + WM
        IF (IVAL(3).EQ.60) THEN
          IVAL(3) = 0
          IVAL(2) = IVAL(2) + 1
          IF (DODAY .AND. IVAL(2).EQ.24) THEN
            IVAL(2) = 0
            IVAL(1) = IVAL(1) + 1
          END IF
        END IF
      END IF
C
C Make HH field modulo 24 if desired
C
      IF (MOD24) IVAL(2) = MOD(IVAL(2),24)
C
      IF (TSCALE.EQ.1) THEN
C
C  Label contains (DD) HH MM SS.S
C
        WRIT(1) = DODAY
        WRIT(2) = T
        WRIT(3) = T
        WRIT(4) = T
      ELSE IF (TSCALE.EQ.60) THEN
C
C  Label contains (DD) HH MM
C
        WRIT(1) = DODAY
        WRIT(2) = T
        WRIT(3) = T
C        
        RVAL    = 0.0
        WRIT(4) = F
      ELSE IF (TSCALE.EQ.3600) THEN
C
C  Label contains (DD) HH
C
        WRIT(1) = DODAY
        WRIT(2) = T
C
        IVAL(3) = 0
        WRIT(3) = F
C  
        RVAL    = 0.0
        WRIT(4) = F
      ELSE IF (TSCALE.EQ.3600*24) THEN
C
C  Label contains DD
C
        WRIT(1) = T
C
        IVAL(2) = 0
        WRIT(2) = F
C
        IVAL(3) = 0
        WRIT(3) = F
C
        RVAL    = 0.0
        WRIT(4) = F
      END IF
C
      RETURN
      END
      SUBROUTINE PGTBX7 (SUPTYP, SIGNF, ASIGN, IVAL, RVAL, WRIT,
     *                   SPREC, DO2, TEXT, TLEN, LAST)
C
      REAL RVAL
      INTEGER IVAL(3), TLEN, SPREC, LAST
      CHARACTER ASIGN*1, TEXT*(*), SIGNF*1, SUPTYP*4
      LOGICAL WRIT(4), DO2
C
C Write (DD) HH MM SS.S time labels into a string
C
C This is a support routine for PGTBOX and should not be
C called by the user
C
C Inputs
C  SUPTYP :  '    ', 'DHMS', or ' DMS' for no superscript labelling,
C            d,h,m,s   or   o,','' superscripting
C  SIGNF  :  Tells which field the sign is associated with.  
C            One of 'D', 'H', 'M', or 'S'    
C  ASIGN  :  ' ' or '-' for positive or negative times
C  IVAL(3):  Day, hour, minutes of time
C  RVAL   :  Seconds of time
C  WRIT(4):  If .true. then write DD, HH, MM, SS  into label
C  SPREC  :  Number of places after the decimal to write seconds 
C            string to.  Must be in the range 0-3
C  DO2    :  If true, add a leading zero to numbers < 10
C Outputs
C  TEXT   :  Label
C  TLEN   :  Length of label
C  LAST   :  Is the location of the start character of the last 
C            field written into TEXT
C
C  05-Sep-1989 -- New routine (Neil Killeen)
C  20-Apr-1991 -- Complete rewrite; support for new DD (day) field and 
C                 superscripted labels [nebk]
C  14-May-1991 -- Removed BSL as a parameter (Char(92)) and made it
C                 a variable to appease Cray compiler [mjs/nebk]
C  10-Jun-1993 -- Rename from PGTLB1, add code to label superscript 
C                 seconds above the '.' and add DO2 option [nebk/jm]
C-----------------------------------------------------------------------
      INTEGER FLEN, FST, FMAX, TRLEN(3), SUPPNT, TMPNT, TLEN2, 
     *IR1, IR2, IP
      CHARACTER FIELD*30, FRMAT2(3)*2, SUPER(4,3)*11, TMP*100, 
     *BSL*1, FRMAT*30
C
      SAVE FRMAT2
      SAVE TRLEN
C
      DATA FRMAT2 /'I1', 'I2', 'I3'/
      DATA TRLEN /5, 11, 5/
C-----------------------------------------------------------------------
C
C   Initialize
C
      BSL = CHAR(92)
      TLEN = 0
      TEXT = ' '
C
C   Assign superscripting strings.  Use CHAR(92) for backslash as the
C   latter must be escaped on SUNs thus requiring preprocessing.  The
C   concatenator operator precludes the use of a data statement
C
      SUPER(1,1) = BSL//'ud'//BSL//'d'
      SUPER(2,1) = BSL//'uh'//BSL//'d'
      SUPER(3,1) = BSL//'um'//BSL//'d'
      SUPER(4,1) = BSL//'us'//BSL//'d'
C
      SUPER(1,2) = BSL//'u'//BSL//'(2199)'//BSL//'d'
      SUPER(2,2) = BSL//'u'//BSL//'(2729)'//BSL//'d'
      SUPER(3,2) = BSL//'u'//BSL//'(2727)'//BSL//'d'
      SUPER(4,2) = BSL//'u'//BSL//'(2728)'//BSL//'d'
C      
      SUPER(1,3) = BSL//'u'//' '//BSL//'d'
      SUPER(2,3) = BSL//'u'//' '//BSL//'d'
      SUPER(3,3) = BSL//'u'//' '//BSL//'d'
      SUPER(4,3) = BSL//'u'//' '//BSL//'d'
C
C   Point at correct superscript strings
C
      IF (SUPTYP.EQ.'DHMS') THEN
        SUPPNT = 1
      ELSE IF (SUPTYP.EQ.' DMS') THEN
        SUPPNT = 2
      ELSE
        SUPPNT = 3
      END IF
C
CCCC
C   Days field
CCCC
C
      IF (WRIT(1)) THEN
        LAST = TLEN + 1
C
C   Write into temporary field
C
        FIELD = ' '
        CALL PGNPL (0, IVAL(1), FLEN)
        WRITE (FIELD, '(I6)') IVAL(1)
        FMAX = 6
        FST = FMAX - FLEN + 1
C
C   Write output text string with desired superscripting
C
        TMPNT = 2
        IF (SIGNF.EQ.'D' .AND. ASIGN.NE.' ') TMPNT = 1
C
        TMP = ASIGN//FIELD(FST:FMAX)//SUPER(1,SUPPNT)
        TLEN2 = (2 - TMPNT) + FLEN + TRLEN(SUPPNT)
C
        TEXT(TLEN+1:) = TMP(TMPNT:TMPNT+TLEN2-1)
        TLEN = TLEN + TLEN2
      END IF
C
CCCC 
C   Hours field
CCCC
C
      IF (WRIT(2)) THEN
        LAST = TLEN + 1
C
C   Write into temporary field
C
        FIELD = ' '
        CALL PGNPL (0, IVAL(2), FLEN)
        WRITE (FIELD, '(I6)') IVAL(2)
        FMAX = 6
        FST = FMAX - FLEN + 1
C
        IF (DO2 .AND. FLEN.EQ.1) THEN
          FLEN = FLEN + 1
          FST = FST - 1
          FIELD(FST:FST) = '0'
        END IF
C
C   Write output text string with desired superscripting
C
        TMPNT = 2
        IF (SIGNF.EQ.'H' .AND. ASIGN.NE.' ') TMPNT = 1
C
        TMP = ASIGN//FIELD(FST:FMAX)//SUPER(2,SUPPNT)
        TLEN2 = (2 - TMPNT) + FLEN + TRLEN(SUPPNT)
C
        TEXT(TLEN+1:) = TMP(TMPNT:TMPNT+TLEN2-1)
        TLEN = TLEN + TLEN2
      END IF
C
CCCC
C   Minutes field
CCCC
C
      IF (WRIT(3)) THEN
        LAST = TLEN + 1
C
C   Write into temporary field with desired superscripting
C
        FIELD = ' '
        WRITE (FIELD, '(I2, A)') IVAL(3), 
     *                           SUPER(3,SUPPNT)(1:TRLEN(SUPPNT))
        FMAX = 2 + TRLEN(SUPPNT)
C
        FST = 1
        IF (FIELD(FST:FST).EQ.' ') THEN
          IF (DO2) THEN
            FIELD(FST:FST) = '0'
          ELSE
            FST = FST + 1
          END IF
        END IF
        FLEN = FMAX - FST + 1
C
C   Write output text string
C
        TMPNT = 2
        IF (SIGNF.EQ.'M' .AND. ASIGN.NE.' ') TMPNT = 1
C
        TMP = ASIGN//FIELD(FST:FMAX)
        TLEN2 = (2 - TMPNT) + FLEN
C
        TEXT(TLEN+1:) = TMP(TMPNT:TMPNT+TLEN2-1)
        TLEN = TLEN + TLEN2
      END IF
C
CCCC
C   Seconds field
CCCC
C
      IF (WRIT(4)) THEN
        LAST = TLEN + 1
C
C   Write into temporary field
C 
        FIELD = ' '
        FST = 1
        IF (SPREC.GE.1) THEN
C
C   Fractional label.  Upto 3 places after the decimal point allowed
C   Muck around to get the superscript on top of the decimal point
C
          IR1 = INT(RVAL)
          IR2 = NINT((RVAL - IR1) * 10**SPREC)
          FRMAT = '(I2, A1, A, '//FRMAT2(SPREC)//')'
          WRITE (FIELD, FRMAT(1:15)) 
     *                       IR1, '.',
     *                       BSL//'b'//SUPER(4,SUPPNT)(1:TRLEN(SUPPNT)),
     *                       IR2
          IP = 5 + TRLEN(SUPPNT) + 1
          IF (FIELD(IP:IP).EQ.' ') FIELD(IP:IP) = '0'
          IF (FIELD(IP+1:IP+1).EQ.' ') FIELD(IP+1:IP+1) = '0'
          FMAX = 1 + 2 + SPREC
        ELSE
C
C   Integer label.  
C
          WRITE (FIELD, '(I2,A)') NINT(RVAL), 
     *                            SUPER(4,SUPPNT)(1:TRLEN(SUPPNT))
          FMAX = 0
        END IF
        FMAX = FMAX + 2 + TRLEN(SUPPNT)
C
        IF (FIELD(FST:FST).EQ.' ') THEN
          IF (DO2) THEN
            FIELD(FST:FST) = '0'
          ELSE
            FST = FST + 1
          END IF
        END IF
        FLEN = FMAX - FST + 1
C
C   Write output text string
C
        TMPNT = 2
        IF (SIGNF.EQ.'S' .AND. ASIGN.NE.' ') TMPNT = 1
        TMP = ASIGN//FIELD(FST:FMAX)
        TLEN2 = (3 - TMPNT) + FLEN
C
        TEXT(TLEN+1:) = TMP(TMPNT:TMPNT+TLEN2-1)
        TLEN = TLEN + TLEN2
      END IF
C  
C   A trailing blank will occur if no superscripting wanted
C
      IF (TLEN.GE.5 .AND. TEXT(TLEN-4:TLEN).EQ.BSL//'u'//' '//BSL//'d')
     *   TLEN = TLEN - 5
C      
      RETURN
      END
