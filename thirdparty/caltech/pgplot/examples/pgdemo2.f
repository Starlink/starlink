      PROGRAM PGDEM2
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. The main program opens the output
C device and calls a series of subroutines, one for each sample plot.
C-----------------------------------------------------------------------
      INTEGER PGOPEN
C
C Call PGOPEN to initiate PGPLOT and open the output device; PGOPEN
C will prompt the user to supply the device name and type.
C
      IF (PGOPEN('?') .LE. 0) STOP
C
C Call the demonstration subroutines.
C
      CALL PGEX21
      CALL PGEX22
      CALL PGEX23
      CALL PGEX24
      CALL PGEX25
      CALL PGEX26
C
C Finally, call PGCLOS to terminate things properly.
C
      CALL PGCLOS
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX21
C-----------------------------------------------------------------------
C Test subroutine for PGPLOT: screen alignment and color palette.
C-----------------------------------------------------------------------
      INTEGER I, L1, L2
      REAL X1, X2, Y1, Y2
      CHARACTER*80 GTYPE, GVER
C
C Get PGPLOT information.
C
      CALL PGQINF('VERSION', GVER, L1)
      CALL PGQINF('TYPE', GTYPE, L2)
      CALL PGBBUF
C
C Alignment test: clear the screen, and draw a box and grid using
C three monochrome intensities (color indices 1, 14, and 15).  The
C plot uses the largest available square viewport and and unit window.
C
      CALL PGPAGE
      CALL PGSVP(0.0,1.0,0.0,1.0)
      CALL PGWNAD(0.0,1.0,0.0,1.0)
      CALL PGSCI(14)
      CALL PGBOX('g',0.02,1,'g',0.02,1)
      CALL PGSCI(15)
      CALL PGBOX('g',0.1,5,'g',0.1,5)
      CALL PGSCI(1)
      CALL PGBOX('bc',0.1,5,'bc',0.1,5)
C
C Color palette test.
C
      DO 20 I=0,15
          CALL PGSCI(I)
          X1 = 0.31 + MOD(I,4)*0.1
          Y1 = 0.61 - (I/4)*0.1
          X2 = X1 + 0.08
          Y2 = Y1 + 0.08
          CALL PGRECT(X1, X2, Y1, Y2)
   20 CONTINUE
C
C Write the device type on the plot.
C
      CALL PGSCI(0)
      CALL PGRECT(0.31, 1.0-0.31, 0.85, 0.97)
      CALL PGSCI(1)
      CALL PGSFS(2)
      CALL PGRECT(0.31, 1.0-0.31, 0.85, 0.97)
      CALL PGPTXT(0.5, 0.91, 0.0, 0.5, 'PGPLOT '//GVER(1:L1))
      CALL PGPTXT(0.5, 0.87, 0.0, 0.5, 'Device '//GTYPE(1:L2))
C
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX22
C-----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.
C Plot a table of the standard PGPLOT graph marker symbols. This
C program also illustrates how windows and viewports may be manipulated.
C-----------------------------------------------------------------------
      CHARACTER*2 LABEL
      INTEGER NX, NY, N, IX, JY, LW
      REAL X, X1, X2, XOFF, Y, Y1, Y2, YOFF, DX, DY
      REAL XPIX1, XPIX2, YPIX1, YPIX2, RES
C
C Determine size of view surface.
C Lower left corner is (X1,Y1), upper right (X2, Y2) [inches].
C
      CALL PGPAGE
      CALL PGSVP(0.0, 1.0, 0.0, 1.0)
      CALL PGQVP(1, X1, X2, Y1, Y2)
      X = X2-X1
      Y = Y2-Y1
C
C Determine device resolution (pixels/inch), and use it to choose
C line width.
C
      CALL PGQVP(3, XPIX1, XPIX2, YPIX1, YPIX2)
      RES = ABS(XPIX2-XPIX1)/ABS(X)
      LW = 1
      IF (RES.GT.166.0) LW = 2
C
C Choose horizontal or vertical arrangement depending on
C device aspect ratio.
C
      IF (X.GT.Y) THEN
          NX = 8
          NY = 5
      ELSE
          NX = 5
          NY = 8
      END IF
      DX = MIN(X/NX, 0.95*Y/NY)
      DY = DX
      IX = NX
      JY = 1
      XOFF = X1 + (X-NX*DX)*0.5
      YOFF = Y1 + (0.95*Y-NY*DY)*0.5
      CALL PGBBUF
C
C Each symbol will be drawn in a standard window; the window is moved
C by manipulating the viewport.
C
      CALL PGSWIN(-1.,1.,-1.,1.)
C
C Loop through all known symbols (N=0-31 and -1 to -8). 
C
      DO 10 N=0,39
          IF (N.LE.31) WRITE (LABEL,'(I2)') N
          IF (N.GT.31) WRITE (LABEL,'(I2)') 31-N
C
C Define window and viewport. The loop allows the plot to extend over
C more than one page if necessary; each page is labelled at the top.
C
          IX = IX+1
          IF (IX.GT.NX) THEN
            IX = 1
            JY = JY-1
          END IF
          IF (JY.LT.1) THEN
            JY = NY
            IF (N.NE.0) CALL PGPAGE
            CALL PGSCH(1.2)
            CALL PGVSIZ(XOFF, XOFF+NX*DX, YOFF, YOFF+NY*DY)
            CALL PGSLW(LW)
            CALL PGMTXT('T', 1.0, 0.5, 0.5,
     1                   '\fiPGPLOT \frMarker Symbols')
          END IF
          CALL PGVSIZ(XOFF+(IX-1)*DX, XOFF+IX*DX,
     1                 YOFF+(JY-1)*DY, YOFF+JY*DY)
C
C Call PGBOX to draw a box and PGMTXT to label it.
C
          CALL PGSLW(1)
          CALL PGBOX('BC',10.0,0,'BC',10.0,0)
          CALL PGSCH(0.5)
          CALL PGMTXT('T',-1.5,0.05,0.0,LABEL)
C
C Call PGPT1 to draw the symbol.
C
          CALL PGSLW(LW)
          CALL PGSCH(1.5)
          IF (N.LE.31) CALL PGPT1(0.0,0.0,N)
          IF (N.GT.31) CALL PGPT1(0.0,0.0,31-N)
   10 CONTINUE
C
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX23
C-----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package. 
C-----------------------------------------------------------------------
      INTEGER N
      PARAMETER (N=9)
      INTEGER I
      REAL X1, Y1
      CHARACTER*80 SAMPLE(N)
      DATA SAMPLE/
     1 'Normal:  \fnABCDQ efgh 1234 \ga\gb\gg\gd \gL\gH\gD\gW',
     2 'Roman:  \frABCDQ efgh 1234 \ga\gb\gg\gd \gL\gH\gD\gW',
     3 'Italic:  \fiABCDQ efgh 1234 \ga\gb\gg\gd \gL\gH\gD\gW',
     4 'Script:  \fsABCDQ efgh 1234 \ga\gb\gg\gd \gL\gH\gD\gW',
     5 '\fif\fr(\fix\fr) = \fix\fr\u2\dcos(2\gp\fix\fr)e\u\fix\fr\u2',
     6 '\fiH\d0\u \fr= 75 \(2233) 25 km s\u-1\d Mpc\u-1\d',
     7 '\fsL/L\d\(2281)\u\fr = 5\.6 \x 10\u6\d (\gl1216\A)',
     8 'Markers: 3=\m3, 8=\m8, 12=\m12, 28=\m28.',
     9 'Cyrillic: \(2830)\(2912)\(2906)\(2911)\(2919)\(2917)\(2915).'/
C
C Call PGENV to initialize the viewport and window.
C Call PGLAB to label the graph.
C
      CALL PGENV(0.,20.,REAL(N),0.,0,-2)
      CALL PGLAB(' ',' ','\fiPGPLOT \frFonts')
C
C Use PGTEXT to write the sample character strings.
C
      CALL PGSCH(1.6)
      DO 10 I=1,N
         X1 = 0.0
         Y1 = REAL(I)-0.5
         CALL PGTEXT(X1, Y1, SAMPLE(I))
 10    CONTINUE
      CALL PGSCH(1.0)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX24
C-----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package. This example
C illustrates the different line widths.
C                              T. J. Pearson  1982 Dec 28
C----------------------------------------------------------------------
      INTEGER IW
      REAL X(2), Y(2)
C
C Call PGENV to initialize the viewport and window.
C
      CALL PGBBUF
      CALL PGENV(0.,15.,0.,15.,0,0)
C
C Call PGLAB to label the graph.
C
      CALL PGLAB('Line Width',' ','\fiPGPLOT \frLine Widths')
C
C Draw 14 oblique lines in different thicknesses.
C
      DO 10 IW=1,14
          X(1) = IW
          Y(1) = 0.0
          X(2) = 0.0
          Y(2) = IW
          CALL PGSLW(IW)
          CALL PGLINE(2,X,Y)
   10 CONTINUE
C
C Draw another set of lines, dashed instead of solid.
C
      CALL PGSLS(2)
      DO 20 IW=1,14
          X(1) = IW
          Y(1) = 15.0
          X(2) = 15.0
          Y(2) = IW
          CALL PGSLW(IW)
          CALL PGLINE(2,X,Y)
   20 CONTINUE
C
      CALL PGSLS(1)
      CALL PGSLW(1)
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX25
C-----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package. This program
C tests polygon clipping on polygons and circles, and tests that
C markers are clipped correctly. Note that markers exactly on the edge
C of the window are supposed to be visible.
C                              T. J. Pearson  1994 Nov 25
C-----------------------------------------------------------------------
      INTEGER I, J
      REAL PX(43), PY(43), SX(5), SY(5), RX(3), RY(3)
      DATA PX / 0.0,2.0,4.0,6.0,8.0,10.0,12.0,14.0,16.4,17.0,17.3,
     1          17.8, 18.5, 20.0, 22.0, 24.0, 26.0, 28.0, 29.0,
     2          28.8,27.2,25.0,23.0,21.5,21.1,21.5,22.8, 24.1, 25.1,
     3          25.2, 24.2, 22.1, 20.0, 18.0, 16.0, 14.0, 12.0,
     4          10.0,  8.0,  6.1,  4.2,  3.0,  1.3 /
      DATA PY / 8.8, 7.6, 7.1, 7.4, 8.0, 8.9, 9.6, 9.9, 9.4,
     1          9.7, 12.0, 14.0, 16.1, 17.0, 17.0, 16.0, 13.9,
     2          13.1, 13.2, 12.3, 11.5, 11.5, 11.5, 11.2, 10.5,
     3          9.0, 8.0, 7.0, 5.1, 3.6, 1.9, 1.1, 0.9, 0.7,
     4          0.8, 1.0, 1.0, 1.2, 1.8, 2.1, 2.9, 4.1, 6.0 /
      DATA SX / 10.0, 10.0, 20.0, 30.0, 15.0 /
      DATA SY /  0.0, -6.0, -6.0,  5.0, -3.5 /
      DATA RX / 26.0, 27.0, 26.0 /
      DATA RY / -4.0, -3.0, -3.0 /
C
      CALL PGPAGE
      CALL PGVSTD
      CALL PGBBUF
      CALL PGSAVE
C
C Set window.
C
      CALL PGWNAD(5.0, 25.0, -5.0, 15.0)
      CALL PGSCI(1)
      CALL PGSLW(1)
      CALL PGBOX('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
C
C Test clipping of polygons and circles
C
      CALL PGSFS(1)
      CALL PGSCI(2)
      CALL PGPOLY(43, PX, PY)
      CALL PGSCI(0)
      CALL PGSFS(3)
      CALL PGSHS(30.0, 2.0, 0.0)
      CALL PGPOLY(43, PX, PY)
      CALL PGSCI(1)
      CALL PGSHS(30.0, 4.0, 0.25)
      CALL PGPOLY(43, PX, PY)
      CALL PGSCI(1)
      CALL PGSFS(2)
      CALL PGPOLY(43, PX, PY)
C
      CALL PGSFS(1)
      CALL PGSCI(4)
      CALL PGPOLY(5, SX, SY)
      CALL PGSCI(0)
      CALL PGSFS(4)
      CALL PGSHS(0.0, 1.6, 0.0)
      CALL PGPOLY(5, SX, SY)
      CALL PGSCI(1)
      CALL PGSFS(2)
      CALL PGPOLY(5, SX, SY)
C     
C       The next polygon should be invisible.
      CALL PGSFS(1)
      CALL PGSCI(4)
      CALL PGPOLY(3, RX, RY)
      CALL PGSCI(1)
      CALL PGSFS(2)
      CALL PGPOLY(3, RX, RY)
C
      CALL PGSFS(1)
      CALL PGSCI(3)
      CALL PGCIRC(8.0, 12.0, 3.5)
      CALL PGSFS(2)
      CALL PGSCI(1)
      CALL PGCIRC(8.0, 12.0, 3.5)
C
C Test clipping of markers: all should be visible.
C
      CALL PGSCI(1)
      CALL PGSLW(1)
      DO 20 I=0,30,5
         DO 10 J=-5,25,5
            CALL PGPT1(REAL(I),REAL(J),9)
 10      CONTINUE
 20   CONTINUE
C
C Draw box.
C
      CALL PGBOX('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
      CALL PGLAB(' ', ' ', 'PGPLOT: clipping polygons and markers')
C
      CALL PGUNSA
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX26
C-----------------------------------------------------------------------
      CHARACTER*128 DEVICE
      CHARACTER*80 GTYPE, GVER
      INTEGER I, J, L, L1, L2
      REAL X, X1, X2, Y, Y1, Y2, R, XI, XP, YP
      REAL PX(43), PY(43)
      DATA PX / 0.0,2.0,4.0,6.0,8.0,10.0,12.0,14.0,16.4,17.0,17.3,
     1          17.8, 18.5, 20.0, 22.0, 24.0, 26.0, 28.0, 29.0,
     2          28.8,27.2,25.0,23.0,21.5,21.1,21.5,22.8, 24.1, 25.1,
     3          25.2, 24.2, 22.1, 20.0, 18.0, 16.0, 14.0, 12.0,
     4          10.0,  8.0,  6.1,  4.2,  3.0,  1.3 /
      DATA PY / 8.8, 7.6, 7.1, 7.4, 8.0, 8.9, 9.6, 9.9, 9.4,
     1          9.7, 12.0, 14.0, 16.1, 17.0, 17.0, 16.0, 13.9,
     2          13.1, 13.2, 12.3, 11.5, 11.5, 11.5, 11.2, 10.5,
     3          9.0, 8.0, 7.0, 5.1, 3.6, 1.9, 1.1, 0.9, 0.7,
     4          0.8, 1.0, 1.0, 1.2, 1.8, 2.1, 2.9, 4.1, 6.0 /
C
      CALL PGQINF('DEV/TYPE', DEVICE, L)
      CALL PGQINF('VERSION', GVER, L1)
      CALL PGQINF('TYPE', GTYPE, L2)
      CALL PGBBUF
C
C Clear the screen; set background and foreground colors.
C
      CALL PGPAGE
      CALL PGSCR(0,0.0,0.0,0.35)
      CALL PGSCR(1,1.0,1.0,1.0)
      CALL PGERAS
C
C Draw a frame at the physical extremities of the plot.
C Dimensions are X by Y (inches).
C
      CALL PGSVP(0.0, 1.0, 0.0, 1.0)
      CALL PGQVP(1, X1, X2, Y1, Y2)
      X = X2-X1
      Y = Y2-Y1
      CALL PGSWIN(0.0, X, 0.0, Y)
      CALL PGSFS(2)
      CALL PGRECT(0.0, X, 0.0, Y)
      CALL PGMOVE(0.5*X, 0.0)
      CALL PGDRAW(0.5*X, Y)
      CALL PGMOVE(0.0, 0.5*Y)
      CALL PGDRAW(X, 0.5*Y)
C
C Draw a circle of diameter 0.5 x min(x,y)
C
      R = 0.25*MIN(X,Y)
      CALL PGCIRC(X*0.5, Y*0.5, R)
C
C Draw some more circles with different line-styles; this tests
C the dashing algorithm on curved lines.
C
      CALL PGSLS(2)
      CALL PGCIRC(X*0.5, Y*0.5, R*1.1)
      CALL PGSLS(3)
      CALL PGCIRC(X*0.5, Y*0.5, R*1.2)
      CALL PGSLS(2)
      CALL PGSLW(3)
      CALL PGCIRC(X*0.5, Y*0.5, R*1.3)
      CALL PGSLS(1)
      CALL PGSLW(1)      
C
C Demonstrate different line-styles
C
      DO 10 I=1,5
          CALL PGSLS(I)
          CALL PGMOVE(I*(X/20.0),0.0)
          CALL PGDRAW(I*(X/20.0),Y)
   10 CONTINUE
      CALL PGSLS(1)
C
C Demonstrate different line-widths
C
      DO 20 I=1,5
          CALL PGSLW(I)
          CALL PGMOVE(0.0, I*(Y/20.0))
          CALL PGDRAW(X, I*(Y/20.0))
   20 CONTINUE
      CALL PGSLW(1)
C
C Demonstrate different line-colors
C
      CALL PGSLW(4)
      DO 40 I=0,15
          CALL PGSCI(I)
          XI = (I+20)*(X/40.0)
          CALL PGMOVE(XI,0.0)
          CALL PGDRAW(XI,Y)
   40 CONTINUE
      CALL PGSCI(1)
      CALL PGSLW(1)
C
C Draw dots in different thicknesses.
C
      XP = (14+20)*(X/40.0)
      DO 30 I=1,21
          YP = I*Y/22.0
          CALL PGSLW(I)
          CALL PGPT1(XP,YP,-1)
   30 CONTINUE
      CALL PGSLW(1)
C
C Demonstrate fill area
C
      DO 50 J=1,43
         PX(J) = (PX(J)+50.0)/100.0*X
         PY(J) = (PY(J)+75.0)/100.0*Y
   50 CONTINUE
      DO 70 I=0,3
          CALL PGSCI(I)
          CALL PGSFS(1)
          CALL PGPOLY(43,PX,PY)
          CALL PGSCI(1)
          CALL PGSFS(2)
          CALL PGPOLY(43,PX,PY)
          DO 60 J=1,43
             PY(J) = PY(J)-0.25*Y
   60     CONTINUE
   70 CONTINUE
C
C Write the device type on the plot.
C
      CALL PGSWIN(0.0, 1.0, 0.0, 1.0)
      CALL PGSFS(1)
      CALL PGSCI(0)
      CALL PGRECT(0.31, 1.0-0.31, 0.85, 0.97)
      CALL PGSCI(1)
      CALL PGSFS(2)
      CALL PGRECT(0.31, 1.0-0.31, 0.85, 0.97)
      CALL PGPTXT(0.5, 0.91, 0.0, 0.5, 'PGPLOT '//GVER(1:L1))
      CALL PGPTXT(0.5, 0.87, 0.0, 0.5, 'Device '//GTYPE(1:L2))
C
      CALL PGEBUF
C-----------------------------------------------------------------------
      END
