      PROGRAM PGDE10
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. 
C This program shows how the default colors can be 
C overridden with PGSCRN (or PGSCR). On some devices (those with a color
C lookup table), changing the background color (color index 0) takes 
C effect immediately, but on others it only affects elements that are
C explicitly drawn in the background color. Thus it is necessary to fill
C the page with the background color, which is here done with PGERAS,
C before drawing anything else (this means that PGENV cannot be used).
C-----------------------------------------------------------------------
      INTEGER PGBEG, I, IER
      REAL XS(9),YS(9), XR(101), YR(101)
C
C Start a new page.
C
      WRITE (*,*) 'This program is intended for use with color displays'
      IF (PGBEG(0,'?',1,1) .NE. 1) STOP
      CALL PGPAGE
C
C Override default colors.
C
      CALL PGSCRN(0, 'DarkSlateGray', IER)
      CALL PGSCRN(1, 'White', IER)
      CALL PGSCRN(2, 'Yellow', IER)
      CALL PGSCRN(3, 'Cyan', IER)
      CALL PGSCRN(4, 'SlateGray', IER)
C
C "Erase" the screen to fill with background color.
C
      CALL PGERAS
C
C Set up window and viewport.
C
      CALL PGSCH(1.5)
      CALL PGVSTD
      CALL PGSWIN(0.,10.,0.,0.65)
C
C Fill the viewport in a different color.
C
      CALL PGSCI(4)
      CALL PGRECT(0., 10., 0., 0.65)
C
C Annotation.
C
      CALL PGSCI(0)
      CALL PGBOX('G', 0.0, 0, 'G', 0.0, 0)
      CALL PGSCI(1)
      CALL PGSLW(3)
      CALL PGSCF(2)
      CALL PGBOX('BCNST', 0.0, 0, 'BCNSTV', 0.0, 0)
      CALL PGLAB('\\fix', ' ', 
     :           '\\frPGPLOT Graph: \\fi y = x\\u\\fr2\\de\\u-\\fix\\d')
C
C Plot the graph.
C
      DO 10 I=1,101
          XR(I) = 0.1*(I-1)
          YR(I) = XR(I)**2*EXP(-XR(I))
 10   CONTINUE
      DO 20 I=1,9
          XS(I) = I
          YS(I) = XS(I)**2*EXP(-XS(I))
 20   CONTINUE
      CALL PGSCI(2)
      CALL PGSLW(4)
      CALL PGLINE(101,XR,YR)
      CALL PGSCI(3)
      CALL PGSCH(3.0)
      CALL PGPT(9,XS,YS,18)
C
C Done.
C
      CALL PGEND
C
      END
