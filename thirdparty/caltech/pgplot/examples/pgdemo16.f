      PROGRAM PGDE16
C
C Demonstration program for bar charts (subroutine PGBCHT).
C This subroutine may be included in the PGPLOT library in a future
C release of PGPLOT
C
      INTEGER PGOPEN
      INTEGER NCAT, NSET
      PARAMETER (NCAT=5, NSET=2)
      REAL VALS(NCAT, NSET)
      REAL VALS2(NCAT, 3)
      REAL VALS3(12)
      CHARACTER*12 LABS(NCAT), LABS3(12)
      REAL VMIN, VMAX
      DATA VALS /15, 2, 3, 45, 17,
     :           14, 1, 2, 44, 16/
      DATA VALS2/15, -20, -13, 45, 17,
     :           14, -11,  -8, 44, 16,
     :           12,   9, -10, 30, 12/
      DATA LABS /'Antelope', 'Bear', 'Cat', 'Dog', 'Elephant'/
      DATA VALS3/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      DATA LABS3/'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     :           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
C
C Bar charts in PGPLOT
C
      IF (PGOPEN('?').LT.1) STOP
      CALL PGSUBP(2,2)

      VMIN = 0.0
      VMAX = 0.0
      CALL PGPAGE
      CALL PGVSTD
      CALL PGBCHT(NCAT, 1, VALS, LABS, VMIN, VMAX, ' ', 0.7, 2)
      CALL PGLAB(' ', ' ', 'Bar Chart')

      CALL PGPAGE
      CALL PGBCHT(NCAT, 2, VALS, LABS, VMIN, VMAX, 'GN', 0.7, 2)
      CALL PGLAB(' ', ' ', 'Grouped Bar Chart (no box)')

      CALL PGPAGE
      CALL PGBCHT(NCAT, 2, VALS, LABS, VMIN, VMAX, 'GS', 0.7, 11)
      CALL PGLAB(' ', ' ', 'Stacked Bar Chart')

      CALL PGPAGE
      CALL PGBCHT(NCAT, 3, VALS2, LABS, VMIN, VMAX, 'G', 0.8, 5)
      CALL PGLAB(' ', ' ', 'Grouped Bar Chart with Negative Values')
      
      CALL PGPAGE
      CALL PGBCHT(NCAT, 3, VALS2, LABS, VMIN, VMAX, 'GS', 0.7, 5)
      CALL PGLAB(' ', ' ', 'Stacked Bar Chart with Negative Values')
      
      CALL PGPAGE
      CALL PGVSTD
      CALL PGBCHT(NCAT, 1, VALS, LABS, VMIN, VMAX, 'H', 0.7, 2)
      CALL PGLAB(' ', ' ', 'Bar Chart')

      CALL PGPAGE
      CALL PGBCHT(NCAT, 2, VALS, LABS, VMIN, VMAX, 'HG', 0.7, 2)
      CALL PGLAB(' ', ' ', 'Grouped Bar Chart')

      CALL PGPAGE
      CALL PGBCHT(NCAT, 2, VALS, LABS, VMIN, VMAX, 'HGS', 0.7, 11)
      CALL PGLAB(' ', ' ', 'Stacked Bar Chart')

      CALL PGPAGE
      CALL PGBCHT(NCAT, 3, VALS2, LABS, VMIN, VMAX, 'HG', 0.8, 5)
      CALL PGLAB(' ', ' ', 'Grouped Bar Chart with Negative Values')
      
      CALL PGPAGE
      CALL PGBCHT(NCAT, 3, VALS2, LABS, VMIN, VMAX, 'HGS', 0.7, 5)
      CALL PGLAB(' ', ' ', 'Stacked Bar Chart with Negative Values')
      
      CALL PGPAGE
      CALL PGBCHT(NCAT, 3, VALS2, LABS, VMIN, VMAX, 'HGSF', 0.7, 7)
      CALL PGLAB(' ', ' ', 'Stacked Bar Chart (Hatched)')
      
      CALL PGPAGE
      CALL PGBCHT(NCAT, 3, VALS2, LABS, VMIN, VMAX, 'GF', 0.7, -1)
      CALL PGLAB(' ', ' ', 'Grouped Bar Chart (Hatched)')

      CALL PGPAGE
      CALL PGBCHT(12, 1, VALS3, LABS3, VMIN, VMAX, 'GF', 0.9, -1)
      CALL PGLAB(' ', ' ', 'Bar Chart (Hatched)')
      
      CALL PGPAGE
      CALL PGBCHT(12, 1, VALS3, LABS3, VMIN, VMAX, 'G', 0.5, 12)
      CALL PGLAB(' ', ' ', 'Bar Chart')
      
      CALL PGCLOS
      END

C*PGBCHT -- draw a bar or column chart
C+
      SUBROUTINE PGBCHT(NCAT, NSET, VALS, LABS, VMIN, VMAX, OPT,
     :                  WIDTH, CI)
      INTEGER NCAT, NSET
      REAL VALS(NCAT,NSET)
      CHARACTER*(*) LABS(NCAT)
      REAL VMIN, VMAX
      CHARACTER*(*) OPT
      REAL WIDTH
      INTEGER CI
C
C Description to be written.
C
C Arguments:
C  NCAT   (input)  : number of categories, and first dimension of VALS.
C  NSET   (input)  : number of data sets (i.e., number of values to be
C                    plotted for each category).
C  VALS   (input)  : data values: a 2-D array (a 1-D array may be used
C                    if NSET=1). Element VALS(I,J) gives the value in
C                    the Jth data set for category I. The first
C                    dimension of VALS must be equal to NCAT, and the
C                    second should equal or exceed NSET (only the first
C                    NSET elements are used).
C  LABS   (input)  : character array, dimension at least NCAT, giving
C                    names for the NCAT categories.
C  VMIN   (input)  : lower limit for the value axis (i.e., the vertical
C                    axis for a vertical column chart, or the
C                    horizontal axis for a horizontal bar chart).
C  VMAX   (input)  : upper limit for the value axis. If VMIN=VMAX=0.0,
C                    the subroutine chooses limits automatically.
C  OPT    (input)  : a character string containing a list of one-letter
C                    options (in any order, and case-insensitive):
C                     F : if present, the data sets are distinguished
C                         using different hatching styles; colors
C                         are also used unless CI=-1 (see below).
C                     G : if present, grid lines are drawn at major
C                         intervals of the value axis.
C                     H : if present, the subroutine draws a horizontal
C                         bar chart, with categories arranged from 
C                         top to bottom; otherwise it draws a vertical
C                         column chart, with categories arranged from
C                         left to right.
C                     L : if present, the value axis is labelled
C                         logarithmically. The end point of the
C                         bars is at value 1 (10**0) rather than zero.
C                         This is unsatisfactory if negative values
C                         are used.
C                     N : if present, the box around the viewport
C                         is omitted (but not the baseline).
C                     S : if present, the subroutine draws a stacked
C                         bar chart; otherwise it draws a grouped
C                         bar chart (there is no difference between
C                         these for a single data set, NSET=1).
C  WIDTH  (input)  : the fraction of the maximum width available for
C                    each category that is occupied by bars. If
C                    WIDTH=1.0, bars from adjacent categories abut.
C                    Recommended value: 0.7 to 0.8.
C  CI     (input)  : a color index. If CI=-1, all bars are colored
C                    using the current color index (i.e., color index
C                    1 unless PGSCI has been called). If CI is 0 or
C                    positive, bars for the first data set are colored
C                    using this color index, and bars for subsequent
C                    data sets are colored using CI+1, CI+2, etc.
C                    (Axes and labels always use the current color
C                    index.)
C--
C 27-Jan-97 [TJP]
C-----------------------------------------------------------------------
      INTEGER I, J, CCI
      LOGICAL GRID, STACK, HORIZ, LOGAX, HATCH, NOBOX
C      LOGICAL PGNOTO
      REAL DMIN, DMAX, CMIN, CMAX, XMIN, XMAX, YMIN, YMAX
      REAL W, MARG, BWID, V, V1, V2, YMINN, YMINP
      CHARACTER L*1, NB*2
      INTEGER FS(3)
      DATA FS/1, 3, 4/
C
C Check and decode arguments.
C
      IF (NCAT.LT.1 .OR. NSET.LT.1) RETURN
      W = WIDTH
      IF (WIDTH.GT.1.0 .OR. WIDTH.LE.0.0) THEN
C          CALL GRWARN('PGBCHT: WIDTH argument should be <= 1.0, > 0.0')
          W = 1.0
      END IF
C      IF (PGNOTO('PGBCHT')) RETURN
      GRID  = INDEX(OPT,'G').NE.0 .OR. INDEX(OPT,'g').NE.0
      STACK = INDEX(OPT,'S').NE.0 .OR. INDEX(OPT,'s').NE.0
      HORIZ = INDEX(OPT,'H').NE.0 .OR. INDEX(OPT,'h').NE.0
      LOGAX = INDEX(OPT,'L').NE.0 .OR. INDEX(OPT,'l').NE.0
      HATCH = INDEX(OPT,'F').NE.0 .OR. INDEX(OPT,'f').NE.0
      NOBOX = INDEX(OPT,'N').NE.0 .OR. INDEX(OPT,'n').NE.0
C
C Determine the data range if necessary.
C
      DMIN = VMIN
      DMAX = VMAX
      IF (DMIN.EQ.0.0 .AND. DMAX.EQ.0.0) THEN
         IF (.NOT.STACK) THEN
C        -- Grouped bar chart
            DO J=1,NSET
               DO I=1,NCAT
                  IF (VALS(I,J).GT.DMAX) DMAX = VALS(I,J)
                  IF (VALS(I,J).LT.DMIN) DMIN = VALS(I,J)
               END DO
            END DO
         ELSE
C        -- Stacked bar chart
C           (accumulate pos and neg separately)
            DO I=1,NCAT
               V1 = 0.0
               V2 = 0.0
               DO J=1,NSET
                  IF (VALS(I,J).GT.0.0) V1 = V1+VALS(I,J)
                  IF (VALS(I,J).LT.0.0) V2 = V2+VALS(I,J)
               END DO
               IF (V1.GT.DMAX) DMAX = V1
               IF (V2.LT.DMIN) DMIN = V2
            END DO
         END IF
         CALL PGRNGE(DMIN, DMAX, DMIN, DMAX)
      END IF
      CMIN = 0.0
      CMAX = NCAT
C
C Set the window.
C
      CALL PGBBUF
      IF (HORIZ) THEN
         CALL PGSWIN(DMIN, DMAX, CMAX, CMIN)
      ELSE
         CALL PGSWIN(CMIN, CMAX, DMIN, DMAX)
      END IF
C
C Draw a grid if requested.
C
      IF (GRID) THEN
         CALL PGSAVE
         CALL PGSCI(15)
         CALL PGSLW(1)
         CALL PGSLS(2)
         IF (HORIZ) THEN
            CALL PGBOX('G', 0.0, 0, ' ', 0.0, 0)
         ELSE
            CALL PGBOX(' ', 0.0, 0, 'G', 0.0, 0)
         END IF
         CALL PGUNSA
      END IF
      CALL PGSAVE
      CALL PGQCI(CCI)
C
C Draw the bars.
C
      MARG = (1.0-W)*0.5
      IF (.NOT.STACK) THEN
C        -- Grouped bar chart
         BWID = W/REAL(NSET)
         DO I=1,NCAT
            DO J=1,NSET
               V = VALS(I,J)
               IF (V.NE.0.0) THEN
                  IF (CI.GE.0) CALL PGSCI(CI+J-1)
                  CALL PGSFS(1)
                  IF (HATCH) CALL PGSFS(FS(1+MOD(J,3)))
                  XMIN = (I-1)+MARG+(J-1)*BWID
                  XMAX = XMIN+BWID
                  YMIN = 0.0
                  YMAX = V
                  IF (HORIZ) THEN
                     CALL PGRECT(YMIN, YMAX, XMIN, XMAX)
                  ELSE
                     CALL PGRECT(XMIN, XMAX, YMIN, YMAX)
                  END IF
                  CALL PGSCI(CCI)
                  CALL PGSFS(2)
                  IF (HORIZ) THEN
                     CALL PGRECT(YMIN, YMAX, XMIN, XMAX)
                  ELSE
                     CALL PGRECT(XMIN, XMAX, YMIN, YMAX)
                  END IF
               END IF
            END DO
         END DO
      ELSE
C        -- Stacked bar chart
         DO I=1,NCAT
            YMINP = 0.0
            YMINN = 0.0
            DO J=1,NSET
               V = VALS(I,J)
               IF (V.NE.0.0) THEN
                  IF (CI.GE.0) CALL PGSCI(CI+J-1)
                  CALL PGSFS(1)
                  IF (HATCH) CALL PGSFS(FS(1+MOD(J,3)))
                  XMIN = (I-1)+MARG
                  XMAX = XMIN+W
                  IF (V.LT.0.0) THEN
                     YMIN = YMINN
                     YMINN = YMINN+V
                  ELSE
                     YMIN = YMINP
                     YMINP = YMINP+V
                  END IF
                  YMAX = YMIN+V
                  IF (HORIZ) THEN
                     CALL PGRECT(YMIN, YMAX, XMIN, XMAX)
                  ELSE
                     CALL PGRECT(XMIN, XMAX, YMIN, YMAX)
                  END IF
                  CALL PGSCI(CCI)
                  CALL PGSFS(2)
                  IF (HORIZ) THEN
                     CALL PGRECT(YMIN, YMAX, XMIN, XMAX)
                  ELSE
                     CALL PGRECT(XMIN, XMAX, YMIN, YMAX)
                  END IF
               END IF
            END DO
         END DO
      END IF
C
C Draw the axes, and a baseline if necessary.
C
      CALL PGSCI(CCI)
      L = ' '
      IF (LOGAX) L= 'L'
      NB = 'BC'
      IF (NOBOX) NB = '  '
      IF (HORIZ) THEN
         CALL PGBOX('NST'//NB//L, 0.0, 0, 'ATP'//NB, 1.0, 1)
      ELSE
         CALL PGBOX('ATP'//NB, 1.0, 1, 'NSTV'//L//NB, 0.0, 0)
      END IF
C
C Label the categories.
C
      CALL PGUPDT
      DO I=1,NCAT
         IF (HORIZ) THEN
            CALL PGMTXT('LV', 0.5, 1.0-(I-0.5)/REAL(NCAT), 1.0, LABS(I))
         ELSE
            CALL PGMTXT('B', 1.2, (I-0.5)/REAL(NCAT), 0.5, LABS(I))
         END IF
      END DO
C
C Done.
C
      CALL PGUNSA
      CALL PGEBUF
      RETURN
      END
