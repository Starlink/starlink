C*PGENV -- set window and viewport and draw labeled frame
C%void cpgenv(float xmin, float xmax, float ymin, float ymax, \
C% int just, int axis);
C+
      SUBROUTINE PGENV (XMIN, XMAX, YMIN, YMAX, JUST, AXIS)
      REAL XMIN, XMAX, YMIN, YMAX
      INTEGER JUST, AXIS
C
C Set PGPLOT "Plotter Environment".  PGENV establishes the scaling
C for subsequent calls to PGPT, PGLINE, etc.  The plotter is
C advanced to a new page or panel, clearing the screen if necessary.
C If the "prompt state" is ON (see PGASK), confirmation
C is requested from the user before clearing the screen.
C If requested, a box, axes, labels, etc. are drawn according to
C the setting of argument AXIS.
C
C Arguments:
C  XMIN   (input)  : the world x-coordinate at the bottom left corner
C                    of the viewport.
C  XMAX   (input)  : the world x-coordinate at the top right corner
C                    of the viewport (note XMAX may be less than XMIN).
C  YMIN   (input)  : the world y-coordinate at the bottom left corner
C                    of the viewport.
C  YMAX   (input)  : the world y-coordinate at the top right corner
C                    of the viewport (note YMAX may be less than YMIN).
C  JUST   (input)  : if JUST=1, the scales of the x and y axes (in
C                    world coordinates per inch) will be equal,
C                    otherwise they will be scaled independently.
C  AXIS   (input)  : controls the plotting of axes, tick marks, etc:
C      AXIS = -2 : draw no box, axes or labels;
C      AXIS = -1 : draw box only;
C      AXIS =  0 : draw box and label it with coordinates;
C      AXIS =  1 : same as AXIS=0, but also draw the
C                  coordinate axes (X=0, Y=0);
C      AXIS =  2 : same as AXIS=1, but also draw grid lines
C                  at major increments of the coordinates;
C      AXIS = 10 : draw box and label X-axis logarithmically;
C      AXIS = 20 : draw box and label Y-axis logarithmically;
C      AXIS = 30 : draw box and label both axes logarithmically.
C
C For other axis options, use routine PGBOX. PGENV can be persuaded to
C call PGBOX with additional axis options by defining an environment
C parameter PGPLOT_ENVOPT containing the required option codes. 
C Examples:
C   PGPLOT_ENVOPT=P      ! draw Projecting tick marks
C   PGPLOT_ENVOPT=I      ! Invert the tick marks
C   PGPLOT_ENVOPT=IV     ! Invert tick marks and label y Vertically
C--
C  1-May-1983
C 25-Sep-1985 [TJP] - change to use PGWNAD.
C 23-Nov-1985 [TJP] - add PGPLOT_ENVOPT option.
C 31-Dec-1985 [TJP] - remove automatic PGBEG call.
C 29-Aug-1989 [TJP] - remove common block; no longer needed.
C-----------------------------------------------------------------------
      INTEGER      L
      LOGICAL      PGNOTO
      CHARACTER*10 XOPTS, YOPTS, ENVOPT, TEMP
C
      IF (PGNOTO('PGENV')) RETURN
C
C Start a new picture: move to a new panel or page as necessary.
C
      CALL PGPAGE
C
C Redefine the standard viewport.
C
      CALL PGVSTD
C
C If invalid arguments are specified, issue warning and leave window
C unchanged.
C
      IF (XMIN.EQ.XMAX) THEN
          CALL GRWARN('invalid x limits in PGENV: XMIN = XMAX.')
          RETURN
      ELSE IF (YMIN.EQ.YMAX) THEN
          CALL GRWARN('invalid y limits in PGENV: YMIN = YMAX.')
          RETURN
      END IF
C
C Call PGSWIN to define the window.
C If equal-scales requested, adjust viewport.
C
      IF (JUST.EQ.1) THEN
          CALL PGWNAD(XMIN,XMAX,YMIN,YMAX)
      ELSE
          CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
      END IF
C
C Call PGBOX to draw and label frame around viewport.
C
      YOPTS = '*'
      IF (AXIS.EQ.-2) THEN
          XOPTS = ' '
      ELSE IF (AXIS.EQ.-1) THEN
          XOPTS = 'BC'
      ELSE IF (AXIS.EQ.0) THEN
          XOPTS = 'BCNST'
      ELSE IF (AXIS.EQ.1) THEN
          XOPTS = 'ABCNST'
      ELSE IF (AXIS.EQ.2) THEN
          XOPTS = 'ABCGNST'
      ELSE IF (AXIS.EQ.10) THEN
          XOPTS = 'BCNSTL'
          YOPTS = 'BCNST'
      ELSE IF (AXIS.EQ.20) THEN
          XOPTS = 'BCNST'
          YOPTS = 'BCNSTL'
      ELSE IF (AXIS.EQ.30) THEN
          XOPTS = 'BCNSTL'
          YOPTS = 'BCNSTL'
      ELSE
          CALL GRWARN('PGENV: illegal AXIS argument.')
          XOPTS = 'BCNST'
      END IF
      IF (YOPTS.EQ.'*') YOPTS = XOPTS
C
C Additional PGBOX options from PGPLOT_ENVOPT.
C
      CALL GRGENV('ENVOPT', ENVOPT, L)
      IF (L.GT.0 .AND. AXIS.GE.0) THEN
          TEMP = XOPTS
          XOPTS = ENVOPT(1:L)//TEMP
          TEMP = YOPTS
          YOPTS = ENVOPT(1:L)//TEMP
      END IF
      CALL PGBOX(XOPTS, 0.0, 0, YOPTS, 0.0, 0)
C
      END
