      PROGRAM PGDEM1
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. The main program opens the output
C device and calls a series of subroutines, one for each sample plot.
C-----------------------------------------------------------------------
      INTEGER PGOPEN
C
C Call PGOPEN to initiate PGPLOT and open the output device; PGOPEN
C will prompt the user to supply the device name and type. Always
C check the return code from PGOPEN.
C
      IF (PGOPEN('?') .LE. 0) STOP
C
C Print information about device.
C
      CALL PGEX0
C
C Call the demonstration subroutines (4,5 are put on one page)
C
      CALL PGEX1
      CALL PGEX2
      CALL PGEX3
      CALL PGSUBP(2,1)
      CALL PGEX4
      CALL PGEX5
      CALL PGSUBP(1,1)
      CALL PGEX6
      CALL PGEX7
      CALL PGEX8
      CALL PGEX9
      CALL PGEX10
      CALL PGEX11
      CALL PGEX12
      CALL PGEX13
      CALL PGEX14
      CALL PGEX15
C
C Finally, call PGCLOS to terminate things properly.
C
      CALL PGCLOS
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX0
C-----------------------------------------------------------------------
C This subroutine tests PGQINF and displays the information returned on
C the standard output.
C-----------------------------------------------------------------------
      CHARACTER*64 VALUE
      INTEGER LENGTH
      REAL X, Y, X1, X2, Y1, Y2
C
C Information available from PGQINF:
C
      CALL PGQINF('version',  VALUE, LENGTH)
      WRITE (*,*) 'version=', VALUE(:LENGTH)
      CALL PGQINF('state',    VALUE, LENGTH)
      WRITE (*,*) 'state=',   VALUE(:LENGTH)
      CALL PGQINF('user',     VALUE, LENGTH)
      WRITE (*,*) 'user=',    VALUE(:LENGTH)
      CALL PGQINF('now',      VALUE, LENGTH)
      WRITE (*,*) 'now=',     VALUE(:LENGTH)
      CALL PGQINF('device',   VALUE, LENGTH)
      WRITE (*,*) 'device=',  VALUE(:LENGTH)
      CALL PGQINF('file',     VALUE, LENGTH)
      WRITE (*,*) 'file=',    VALUE(:LENGTH)
      CALL PGQINF('type',     VALUE, LENGTH)
      WRITE (*,*) 'type=',    VALUE(:LENGTH)
      CALL PGQINF('dev/type', VALUE, LENGTH)
      WRITE (*,*) 'dev/type=',VALUE(:LENGTH)
      CALL PGQINF('hardcopy', VALUE, LENGTH)
      WRITE (*,*) 'hardcopy=',VALUE(:LENGTH)
      CALL PGQINF('terminal', VALUE, LENGTH)
      WRITE (*,*) 'terminal=',VALUE(:LENGTH)
      CALL PGQINF('cursor',   VALUE, LENGTH)
      WRITE (*,*) 'cursor=',  VALUE(:LENGTH)
C
C Get view surface dimensions:
C
      CALL PGQVSZ(1, X1, X2, Y1, Y2)
      X = X2-X1
      Y = Y2-Y1
      WRITE (*,100) X, Y, X*25.4, Y*25.4
  100 FORMAT (' Plot dimensions (x,y; inches): ',F9.2,', ',F9.2/
     1        '                          (mm): ',F9.2,', ',F9.2)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX1
C-----------------------------------------------------------------------
C This example illustrates the use of PGENV, PGLAB, PGPT, PGLINE.
C-----------------------------------------------------------------------
      INTEGER I
      REAL XS(5),YS(5), XR(100), YR(100)
      DATA XS/1.,2.,3.,4.,5./
      DATA YS/1.,4.,9.,16.,25./
C
C Call PGENV to specify the range of the axes and to draw a box, and
C PGLAB to label it. The x-axis runs from 0 to 10, and y from 0 to 20.
C
      CALL PGENV(0.,10.,0.,20.,0,1)
      CALL PGLAB('(x)', '(y)', 'PGPLOT Example 1:  y = x\u2')
C
C Mark five points (coordinates in arrays XS and YS), using symbol
C number 9.
C
      CALL PGPT(5,XS,YS,9)
C
C Compute the function at 60 points, and use PGLINE to draw it.
C
      DO 10 I=1,60
          XR(I) = 0.1*I
          YR(I) = XR(I)**2
   10 CONTINUE
      CALL PGLINE(60,XR,YR)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX2
C-----------------------------------------------------------------------
C Repeat the process for another graph. This one is a graph of the
C sinc (sin x over x) function.
C-----------------------------------------------------------------------
      INTEGER I
      REAL XR(100), YR(100)
C
      CALL PGENV(-2.,10.,-0.4,1.2,0,1)
      CALL PGLAB('(x)', 'sin(x)/x', 
     $             'PGPLOT Example 2:  Sinc Function')
      DO 20 I=1,100
          XR(I) = (I-20)/6.
          YR(I) = 1.0
          IF (XR(I).NE.0.0) YR(I) = SIN(XR(I))/XR(I)
   20 CONTINUE
      CALL PGLINE(100,XR,YR)
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX3
C----------------------------------------------------------------------
C This example illustrates the use of PGBOX and attribute routines to
C mix colors and line-styles.
C----------------------------------------------------------------------
      REAL PI
      PARAMETER (PI=3.14159265359)
      INTEGER I
      REAL XR(360), YR(360)
      REAL ARG
C
C Call PGENV to initialize the viewport and window; the
C AXIS argument is -2, so no frame or labels will be drawn.
C
      CALL PGENV(0.,720.,-2.0,2.0,0,-2)
      CALL PGSAVE
C
C Set the color index for the axes and grid (index 5 = cyan).
C Call PGBOX to draw first a grid at low brightness, and then a
C frame and axes at full brightness. Note that as the x-axis is
C to represent an angle in degrees, we request an explicit tick 
C interval of 90 deg with subdivisions at 30 deg, as multiples of
C 3 are a more natural division than the default.
C
      CALL PGSCI(14)
      CALL PGBOX('G',30.0,0,'G',0.2,0)
      CALL PGSCI(5)
      CALL PGBOX('ABCTSN',90.0,3,'ABCTSNV',0.0,0)
C
C Call PGLAB to label the graph in a different color (3=green).
C
      CALL PGSCI(3)
      CALL PGLAB('x (degrees)','f(x)','PGPLOT Example 3')
C
C Compute the function to be plotted: a trig function of an
C angle in degrees, computed every 2 degrees.
C
      DO 20 I=1,360
          XR(I) = 2.0*I
          ARG = XR(I)/180.0*PI
          YR(I) = SIN(ARG) + 0.5*COS(2.0*ARG) + 
     1                0.5*SIN(1.5*ARG+PI/3.0)
   20 CONTINUE
C
C Change the color (6=magenta), line-style (2=dashed), and line
C width and draw the function.
C
      CALL PGSCI(6)
      CALL PGSLS(2)
      CALL PGSLW(3)
      CALL PGLINE(360,XR,YR)
C
C Restore attributes to defaults.
C
      CALL PGUNSA
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX4
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT: draw histograms.
C-----------------------------------------------------------------------
      REAL PI
      PARAMETER (PI=3.14159265359)
      INTEGER  I, ISEED
      REAL     DATA(1000), X(620), Y(620)
      REAL     PGRNRM
C
C Call PGRNRM to obtain 1000 samples from a normal distribution.
C
      ISEED = -5678921
      DO 10 I=1,1000
          DATA(I) = PGRNRM(ISEED)
   10 CONTINUE
C
C Draw a histogram of these values.
C
      CALL PGSAVE
      CALL PGHIST(1000,DATA,-3.1,3.1,31,0)
C
C Samples from another normal distribution.
C
      DO 15 I=1,200
          DATA(I) = 1.0+0.5*PGRNRM(ISEED)
   15 CONTINUE
C
C Draw another histogram (filled) on same axes.
C
      CALL PGSCI(15)
      CALL PGHIST(200,DATA,-3.1,3.1,31,3)
      CALL PGSCI(0)
      CALL PGHIST(200,DATA,-3.1,3.1,31,1)
      CALL PGSCI(1)
C
C Redraw the box which may have been clobbered by the histogram.
C
      CALL PGBOX('BST', 0.0, 0, ' ', 0.0, 0)
C
C Label the plot.
C
      CALL PGLAB('Variate', ' ',
     $             'PGPLOT Example 4:  Histograms (Gaussian)')
C
C Superimpose the theoretical distribution.
C
      DO 20 I=1,620
          X(I) = -3.1 + 0.01*(I-1)
          Y(I) = 0.2*1000./SQRT(2.0*PI)*EXP(-0.5*X(I)*X(I))
   20 CONTINUE
      CALL PGLINE(620,X,Y)
      CALL PGUNSA
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX5
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates how to draw a log-log plot.
C PGPLOT subroutines demonstrated:
C    PGENV, PGERRY, PGLAB, PGLINE, PGPT, PGSCI.
C----------------------------------------------------------------------
      INTEGER   RED, GREEN, CYAN
      PARAMETER (RED=2)
      PARAMETER (GREEN=3)
      PARAMETER (CYAN=5)
      INTEGER   NP
      PARAMETER (NP=15)
      INTEGER   I
      REAL      X, YLO(NP), YHI(NP)
      REAL      FREQ(NP), FLUX(NP), XP(100), YP(100), ERR(NP)
      DATA FREQ / 26., 38., 80., 160., 178., 318.,
     1            365., 408., 750., 1400., 2695., 2700.,
     2            5000., 10695., 14900. /
      DATA FLUX / 38.0, 66.4, 89.0, 69.8, 55.9, 37.4,
     1            46.8, 42.4, 27.0, 15.8, 9.09, 9.17,
     2            5.35, 2.56, 1.73 /
      DATA ERR  / 6.0, 6.0, 13.0, 9.1, 2.9, 1.4,
     1            2.7, 3.0, 0.34, 0.8, 0.2, 0.46,
     2            0.15, 0.08, 0.01 /
C
C Call PGENV to initialize the viewport and window; the AXIS argument 
C is 30 so both axes will be logarithmic. The X-axis (frequency) runs 
C from 0.01 to 100 GHz, the Y-axis (flux density) runs from 0.3 to 300
C Jy. Note that it is necessary to specify the logarithms of these
C quantities in the call to PGENV. We request equal scales in x and y
C so that slopes will be correct.  Use PGLAB to label the graph.
C
      CALL PGSAVE
      CALL PGSCI(CYAN)
      CALL PGENV(-2.0,2.0,-0.5,2.5,1,30)
      CALL PGLAB('Frequency, \gn (GHz)',
     1             'Flux Density, S\d\gn\u (Jy)',
     2             'PGPLOT Example 5:  Log-Log plot')
C
C Draw a fit to the spectrum (don't ask how this was chosen). This 
C curve is drawn before the data points, so that the data will write 
C over the curve, rather than vice versa.
C
      DO 10 I=1,100
          X = 1.3 + I*0.03
          XP(I) = X-3.0
          YP(I) = 5.18 - 1.15*X -7.72*EXP(-X)
   10 CONTINUE
      CALL PGSCI(RED)
      CALL PGLINE(100,XP,YP)
C
C Plot the measured flux densities: here the data are installed with a
C DATA statement; in a more general program, they might be read from a
C file. We first have to take logarithms (the -3.0 converts MHz to GHz).
C
      DO 20 I=1,NP
          XP(I) = ALOG10(FREQ(I))-3.0
          YP(I) = ALOG10(FLUX(I))
   20 CONTINUE
      CALL PGSCI(GREEN)
      CALL PGPT(NP, XP, YP, 17)
C
C Draw +/- 2 sigma error bars: take logs of both limits.
C
      DO 30 I=1,NP
          YHI(I) = ALOG10(FLUX(I)+2.*ERR(I))
          YLO(I) = ALOG10(FLUX(I)-2.*ERR(I))
   30 CONTINUE
      CALL PGERRY(NP,XP,YLO,YHI,1.0)
      CALL PGUNSA
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX6
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates the use of PGPOLY, PGCIRC, and PGRECT using SOLID, 
C OUTLINE, HATCHED, and CROSS-HATCHED fill-area attributes.
C----------------------------------------------------------------------
      REAL PI, TWOPI
      PARAMETER (PI=3.14159265359)
      PARAMETER (TWOPI=2.0*PI)
      INTEGER NPOL
      PARAMETER (NPOL=6)
      INTEGER I, J, N1(NPOL), N2(NPOL), K
      REAL X(10), Y(10), Y0, ANGLE
      CHARACTER*32 LAB(4)
      DATA N1 / 3, 4, 5, 5, 6, 8 /
      DATA N2 / 1, 1, 1, 2, 1, 3 /
      DATA LAB(1) /'Fill style 1 (solid)'/
      DATA LAB(2) /'Fill style 2 (outline)'/
      DATA LAB(3) /'Fill style 3 (hatched)'/
      DATA LAB(4) /'Fill style 4 (cross-hatched)'/
C
C Initialize the viewport and window.
C
      CALL PGBBUF
      CALL PGSAVE
      CALL PGPAGE
      CALL PGSVP(0.0, 1.0, 0.0, 1.0)
      CALL PGWNAD(0.0, 10.0, 0.0, 10.0)
C
C Label the graph.
C
      CALL PGSCI(1)
      CALL PGMTXT('T', -2.0, 0.5, 0.5, 
     :     'PGPLOT fill area: routines PGPOLY, PGCIRC, PGRECT')
C
C Draw assorted polygons.
C
      DO 30 K=1,4
         CALL PGSCI(1)
         Y0 = 10.0 - 2.0*K
         CALL PGTEXT(0.2, Y0+0.6, LAB(K))
         CALL PGSFS(K)
         DO 20 I=1,NPOL
            CALL PGSCI(I)
            DO 10 J=1,N1(I)
               ANGLE = REAL(N2(I))*TWOPI*REAL(J-1)/REAL(N1(I))
               X(J) = I + 0.5*COS(ANGLE)
               Y(J) = Y0 + 0.5*SIN(ANGLE)
 10         CONTINUE
            CALL PGPOLY (N1(I),X,Y)
 20      CONTINUE
         CALL PGSCI(7)
         CALL PGCIRC(7.0, Y0, 0.5)
         CALL PGSCI(8)
         CALL PGRECT(7.8, 9.5, Y0-0.5, Y0+0.5)
 30   CONTINUE
C
      CALL PGUNSA
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX7
C-----------------------------------------------------------------------
C A plot with a large number of symbols; plus test of PGERR1.
C-----------------------------------------------------------------------
      INTEGER I, ISEED
      REAL XS(300),YS(300), XR(101), YR(101), XP, YP, XSIG, YSIG
      REAL PGRAND, PGRNRM
C
C Window and axes.
C
      CALL PGBBUF
      CALL PGSAVE
      CALL PGSCI(1)
      CALL PGENV(0.,5.,-0.3,0.6,0,1)
      CALL PGLAB('\fix', '\fiy', 'PGPLOT Example 7: scatter plot')
C
C Random data points.
C
      ISEED = -45678921
      DO 10 I=1,300
          XS(I) = 5.0*PGRAND(ISEED)
          YS(I) = XS(I)*EXP(-XS(I)) + 0.05*PGRNRM(ISEED)
   10 CONTINUE
      CALL PGSCI(3)
      CALL PGPT(100,XS,YS,3)
      CALL PGPT(100,XS(101),YS(101),17)
      CALL PGPT(100,XS(201),YS(201),21)
C
C Curve defining parent distribution.
C
      DO 20 I=1,101
          XR(I) = 0.05*(I-1)
          YR(I) = XR(I)*EXP(-XR(I))
   20 CONTINUE
      CALL PGSCI(2)
      CALL PGLINE(101,XR,YR)
C
C Test of PGERR1/PGPT1.
C
      XP = XS(101)
      YP = YS(101)
      XSIG = 0.2
      YSIG = 0.1
      CALL PGSCI(5)
      CALL PGSCH(3.0)
      CALL PGERR1(5, XP, YP, XSIG, 1.0)
      CALL PGERR1(6, XP, YP, YSIG, 1.0)
      CALL PGPT1(XP,YP,21)
C
      CALL PGUNSA
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX8
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. This program shows some of the
C possibilities for overlapping windows and viewports.
C T. J. Pearson  1986 Nov 28
C-----------------------------------------------------------------------
      INTEGER I
      REAL XR(720), YR(720)
C-----------------------------------------------------------------------
C Color index:
      INTEGER BLACK, WHITE, RED, GREEN, BLUE, CYAN, MAGENT, YELLOW
      PARAMETER (BLACK=0)
      PARAMETER (WHITE=1)
      PARAMETER (RED=2)
      PARAMETER (GREEN=3)
      PARAMETER (BLUE=4)
      PARAMETER (CYAN=5)
      PARAMETER (MAGENT=6)
      PARAMETER (YELLOW=7)
C Line style:
      INTEGER FULL, DASHED, DOTDSH, DOTTED, FANCY
      PARAMETER (FULL=1)
      PARAMETER (DASHED=2)
      PARAMETER (DOTDSH=3)
      PARAMETER (DOTTED=4)
      PARAMETER (FANCY=5)
C Character font:
      INTEGER NORMAL, ROMAN, ITALIC, SCRIPT
      PARAMETER (NORMAL=1)
      PARAMETER (ROMAN=2)
      PARAMETER (ITALIC=3)
      PARAMETER (SCRIPT=4)
C Fill-area style:
      INTEGER SOLID, HOLLOW
      PARAMETER (SOLID=1)
      PARAMETER (HOLLOW=2)
C-----------------------------------------------------------------------
C
      CALL PGPAGE
      CALL PGBBUF
      CALL PGSAVE
C
C Define the Viewport
C
      CALL PGSVP(0.1,0.6,0.1,0.6)
C
C Define the Window
C
      CALL PGSWIN(0.0, 630.0, -2.0, 2.0)
C
C Draw a box
C
      CALL PGSCI(CYAN)
      CALL PGBOX ('ABCTS', 90.0, 3, 'ABCTSV', 0.0, 0)
C
C Draw labels
C
      CALL PGSCI (RED)
      CALL PGBOX ('N',90.0, 3, 'VN', 0.0, 0)
C
C Draw SIN line
C
      DO 10 I=1,360
          XR(I) = 2.0*I
          YR(I) = SIN(XR(I)/57.29577951)
   10 CONTINUE
      CALL PGSCI (MAGENT)
      CALL PGSLS (DASHED)
      CALL PGLINE (360,XR,YR)
C
C Draw COS line by redefining the window
C
      CALL PGSWIN (90.0, 720.0, -2.0, 2.0)
      CALL PGSCI (YELLOW)
      CALL PGSLS (DOTTED)
      CALL PGLINE (360,XR,YR)
      CALL PGSLS (FULL)
C
C Re-Define the Viewport
C
      CALL PGSVP(0.45,0.85,0.45,0.85)
C
C Define the Window, and erase it
C
      CALL PGSWIN(0.0, 180.0, -2.0, 2.0)
      CALL PGSCI(0)
      CALL PGRECT(0.0, 180., -2.0, 2.0)
C
C Draw a box
C
      CALL PGSCI(BLUE)
      CALL PGBOX ('ABCTSM', 60.0, 3, 'VABCTSM', 1.0, 2)
C
C Draw SIN line
C
      CALL PGSCI (WHITE)
      CALL PGSLS (DASHED)
      CALL PGLINE (360,XR,YR)
C
      CALL PGUNSA
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX9
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates curve drawing with PGFUNT; the parametric curve drawn is
C a simple Lissajous figure.
C                              T. J. Pearson  1983 Oct 5
C----------------------------------------------------------------------
      REAL PI, TWOPI
      PARAMETER (PI=3.14159265359)
      PARAMETER (TWOPI=2.0*PI)
      REAL     FX, FY
      EXTERNAL FX, FY
C
C Call PGFUNT to draw the function (autoscaling).
C
      CALL PGBBUF
      CALL PGSAVE
      CALL PGSCI(5)
      CALL PGFUNT(FX,FY,360,0.0,TWOPI,0)
C
C Call PGLAB to label the graph in a different color.
C
      CALL PGSCI(3)
      CALL PGLAB('x','y','PGPLOT Example 9:  routine PGFUNT')
      CALL PGUNSA
      CALL PGEBUF
C
      END

      REAL FUNCTION FX(T)
      REAL T
      FX = SIN(T*5.0)
      RETURN
      END

      REAL FUNCTION FY(T)
      REAL T
      FY = SIN(T*4.0)
      RETURN
      END

      SUBROUTINE PGEX10
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates curve drawing with PGFUNX.
C                              T. J. Pearson  1983 Oct 5
C----------------------------------------------------------------------
      REAL PI
      PARAMETER (PI=3.14159265359)
C The following define mnemonic names for the color indices and
C linestyle codes.
      INTEGER   BLACK, WHITE, RED, GREEN, BLUE, CYAN, MAGENT, YELLOW
      PARAMETER (BLACK=0)
      PARAMETER (WHITE=1)
      PARAMETER (RED=2)
      PARAMETER (GREEN=3)
      PARAMETER (BLUE=4)
      PARAMETER (CYAN=5)
      PARAMETER (MAGENT=6)
      PARAMETER (YELLOW=7)
      INTEGER   FULL, DASH, DOTD
      PARAMETER (FULL=1)
      PARAMETER (DASH=2)
      PARAMETER (DOTD=3)
C
C The Fortran functions to be plotted must be declared EXTERNAL.
C
      REAL     PGBSJ0, PGBSJ1
      EXTERNAL PGBSJ0, PGBSJ1
C
C Call PGFUNX twice to draw two functions (autoscaling the first time).
C
      CALL PGBBUF
      CALL PGSAVE
      CALL PGSCI(YELLOW)
      CALL PGFUNX(PGBSJ0,500,0.0,10.0*PI,0)
      CALL PGSCI(RED)
      CALL PGSLS(DASH)
      CALL PGFUNX(PGBSJ1,500,0.0,10.0*PI,1)
C
C Call PGLAB to label the graph in a different color. Note the
C use of "\f" to change font.  Use PGMTXT to write an additional
C legend inside the viewport.
C
      CALL PGSCI(GREEN)
      CALL PGSLS(FULL)
      CALL PGLAB('\fix', '\fiy',
     2           '\frPGPLOT Example 10: routine PGFUNX')
      CALL PGMTXT('T', -4.0, 0.5, 0.5,
     1     '\frBessel Functions')
C
C Call PGARRO to label the curves.
C
      CALL PGARRO(8.0, 0.7, 1.0, PGBSJ0(1.0))
      CALL PGARRO(12.0, 0.5, 9.0, PGBSJ1(9.0))
      CALL PGSTBG(GREEN)
      CALL PGSCI(0)
      CALL PGPTXT(8.0, 0.7, 0.0, 0.0, ' \fiy = J\d0\u(x)')
      CALL PGPTXT(12.0, 0.5, 0.0, 0.0, ' \fiy = J\d1\u(x)')
      CALL PGUNSA
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX11
C-----------------------------------------------------------------------
C Test routine for PGPLOT: draws a skeletal dodecahedron.
C-----------------------------------------------------------------------
      INTEGER NVERT
      REAL T, T1, T2, T3
      PARAMETER (NVERT=20)
      PARAMETER (T=1.618)
      PARAMETER (T1=1.0+T)
      PARAMETER (T2=-1.0*T)
      PARAMETER (T3=-1.0*T1)
      INTEGER I, J, K
      REAL VERT(3,NVERT), R, ZZ
      REAL X(2),Y(2)
C
C Cartesian coordinates of the 20 vertices.
C
      DATA VERT/ T, T, T,       T, T,T2,
     3           T,T2, T,       T,T2,T2,
     5          T2, T, T,      T2, T,T2,
     7          T2,T2, T,      T2,T2,T2,
     9          T1,1.0,0.0,    T1,-1.0,0.0,
     B          T3,1.0,0.0,    T3,-1.0,0.0,
     D          0.0,T1,1.0,    0.0,T1,-1.0,
     F          0.0,T3,1.0,    0.0,T3,-1.0,
     H          1.0,0.0,T1,    -1.0,0.0,T1,
     J          1.0,0.0,T3,   -1.0,0.0,T3 /
C
C Initialize the plot (no labels).
C
      CALL PGBBUF
      CALL PGSAVE
      CALL PGENV(-4.,4.,-4.,4.,1,-2)
      CALL PGSCI(2)
      CALL PGSLS(1)
      CALL PGSLW(1)
C
C Write a heading.
C
      CALL PGLAB(' ',' ','PGPLOT Example 11:  Dodecahedron')
C
C Mark the vertices.
C
      DO 2 I=1,NVERT
          ZZ = VERT(3,I)
          CALL PGPT1(VERT(1,I)+0.2*ZZ,VERT(2,I)+0.3*ZZ,9)
    2 CONTINUE
C
C Draw the edges - test all vertex pairs to find the edges of the 
C correct length.
C
      CALL PGSLW(3)
      DO 20 I=2,NVERT
          DO 10 J=1,I-1
              R = 0.
              DO 5 K=1,3
                  R = R + (VERT(K,I)-VERT(K,J))**2
    5         CONTINUE
              R = SQRT(R)
              IF(ABS(R-2.0).GT.0.1) GOTO 10
              ZZ = VERT(3,I)
              X(1) = VERT(1,I)+0.2*ZZ
              Y(1) = VERT(2,I)+0.3*ZZ
              ZZ = VERT(3,J)
              X(2) = VERT(1,J)+0.2*ZZ
              Y(2) = VERT(2,J)+0.3*ZZ
              CALL PGLINE(2,X,Y)
   10     CONTINUE
   20 CONTINUE
      CALL PGUNSA
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX12
C-----------------------------------------------------------------------
C Test routine for PGPLOT: draw arrows with PGARRO.
C-----------------------------------------------------------------------
      INTEGER NV, I, K
      REAL A, D, X, Y, XT, YT
C
C Number of arrows.
C
      NV =16
C
C Select a square viewport.
C
      CALL PGBBUF
      CALL PGSAVE
      CALL PGSCH(0.7)
      CALL PGSCI(2)
      CALL PGENV(-1.05,1.05,-1.05,1.05,1,-1)
      CALL PGLAB(' ', ' ', 'PGPLOT Example 12: PGARRO')
      CALL PGSCI(1)
C
C Draw the arrows
C
      K = 1
      D = 360.0/57.29577951/NV
      A = -D
      DO 20 I=1,NV
          A = A+D
          X = COS(A)
          Y = SIN(A)
          XT = 0.2*COS(A-D)
          YT = 0.2*SIN(A-D)
          CALL PGSAH(K, 80.0-3.0*I, 0.5*REAL(I)/REAL(NV))
          CALL PGSCH(0.25*I)
          CALL PGARRO(XT, YT, X, Y)
          K = K+1
          IF (K.GT.2) K=1
   20 CONTINUE
C
      CALL PGUNSA
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX13
C----------------------------------------------------------------------
C This example illustrates the use of PGTBOX.
C----------------------------------------------------------------------
      INTEGER N
      PARAMETER (N=10)
      INTEGER I
      REAL X1(N), X2(N)
      CHARACTER*20 XOPT(N), BSL*1
      DATA X1 /   4*0.0, -8000.0, 100.3, 205.3, -45000.0, 2*0.0/
      DATA X2 /4*8000.0,  8000.0, 101.3, 201.1, 3*-100000.0/
      DATA XOPT / 'BSTN', 'BSTNZ', 'BSTNZH', 'BSTNZD', 'BSNTZHFO', 
     :      'BSTNZD', 'BSTNZHI', 'BSTNZHP', 'BSTNZDY', 'BSNTZHFOY'/
C
      BSL = CHAR(92)
      CALL PGPAGE
      CALL PGSAVE
      CALL PGBBUF
      CALL PGSCH(0.7)
      DO 100 I=1,N
        CALL PGSVP(0.15, 0.85, (0.7+REAL(N-I))/REAL(N), 
     :                         (0.7+REAL(N-I+1))/REAL(N)) 
        CALL PGSWIN(X1(I), X2(I), 0.0, 1.0)
        CALL PGTBOX(XOPT(I),0.0,0,' ',0.0,0)
        CALL PGLAB('Option = '//XOPT(I), ' ', ' ')
        IF (I.EQ.1) THEN
           CALL PGMTXT('B', -1.0, 0.5, 0.5, 
     :                 BSL//'fiAxes drawn with PGTBOX')
        END IF
  100 CONTINUE
      CALL PGEBUF
      CALL PGUNSA
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX14
C-----------------------------------------------------------------------
C Test routine for PGPLOT: polygon fill and color representation.
C-----------------------------------------------------------------------
      INTEGER I, J, N, M
      REAL PI, THINC, R, G, B, THETA
      REAL XI(100),YI(100),XO(100),YO(100),XT(3),YT(3)
      PARAMETER (PI=3.14159265359)
C
      N = 33
      M = 8
      THINC=2.0*PI/N
      DO 10 I=1,N
        XI(I) = 0.0
        YI(I) = 0.0
   10 CONTINUE
      CALL PGBBUF
      CALL PGSAVE
      CALL PGENV(-1.,1.,-1.,1.,1,-2)
      CALL PGLAB(' ', ' ', 'PGPLOT Example 14: PGPOLY and PGSCR')
      DO 50 J=1,M
        R = 1.0
        G = 1.0 - REAL(J)/REAL(M)
        B = G
        CALL PGSCR(J, R, G, B)
        THETA = -REAL(J)*PI/REAL(N)
        R = REAL(J)/REAL(M)
        DO 20 I=1,N
          THETA = THETA+THINC
          XO(I) = R*COS(THETA)
          YO(I) = R*SIN(THETA)
   20   CONTINUE
        DO 30 I=1,N
          XT(1) = XO(I)
          YT(1) = YO(I)
          XT(2) = XO(MOD(I,N)+1)
          YT(2) = YO(MOD(I,N)+1)
          XT(3) = XI(I)
          YT(3) = YI(I)
          CALL PGSCI(J)
          CALL PGSFS(1)
          CALL PGPOLY(3,XT,YT)
          CALL PGSFS(2)
          CALL PGSCI(1)
          CALL PGPOLY(3,XT,YT)
   30   CONTINUE
        DO 40 I=1,N
          XI(I) = XO(I)
          YI(I) = YO(I)
   40   CONTINUE
   50 CONTINUE
      CALL PGUNSA
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX15
C----------------------------------------------------------------------
C This is a line-drawing test; it draws a regular n-gon joining
C each vertex to every other vertex. It is not optimized for pen
C plotters.
C----------------------------------------------------------------------
      INTEGER I, J, NV
      REAL A, D, X(100), Y(100)
C
C Set the number of vertices, and compute the 
C coordinates for unit circumradius.
C
      NV = 17
      D = 360.0/NV
      A = -D
      DO 20 I=1,NV
          A = A+D
          X(I) = COS(A/57.29577951)
          Y(I) = SIN(A/57.29577951)
   20 CONTINUE
C
C Select a square viewport.
C
      CALL PGBBUF
      CALL PGSAVE
      CALL PGSCH(0.5)
      CALL PGSCI(2)
      CALL PGENV(-1.05,1.05,-1.05,1.05,1,-1)
      CALL PGLAB(' ', ' ', 'PGPLOT Example 15: PGMOVE and PGDRAW')
      CALL PGSCR(0,0.2,0.3,0.3)
      CALL PGSCR(1,1.0,0.5,0.2)
      CALL PGSCR(2,0.2,0.5,1.0)
      CALL PGSCI(1)
C
C Draw the polygon.
C
      DO 40 I=1,NV-1
          DO 30 J=I+1,NV
            CALL PGMOVE(X(I),Y(I))
            CALL PGDRAW(X(J),Y(J))
   30     CONTINUE
   40 CONTINUE
C
C Flush the buffer.
C
      CALL PGUNSA
      CALL PGEBUF
C-----------------------------------------------------------------------
      END

      REAL FUNCTION PGBSJ0(XX)
      REAL XX
C-----------------------------------------------------------------------
C Bessel function of order 0 (approximate).
C Reference: Abramowitz and Stegun: Handbook of Mathematical Functions.
C-----------------------------------------------------------------------
      REAL X, XO3, T, F0, THETA0
C     
      X = ABS(XX)
      IF (X .LE. 3.0) THEN
         XO3 = X/3.0
         T   = XO3*XO3
         PGBSJ0 = 1.0 + T*(-2.2499997 +
     1                  T*( 1.2656208 +
     2                  T*(-0.3163866 +
     3                  T*( 0.0444479 +
     4                  T*(-0.0039444 +
     5                  T*( 0.0002100))))))
      ELSE
         T = 3.0/X
         F0 =     0.79788456 +
     1        T*(-0.00000077 + 
     2        T*(-0.00552740 +
     3        T*(-0.00009512 +
     4        T*( 0.00137237 +
     5        T*(-0.00072805 +
     6        T*( 0.00014476))))))
         THETA0 = X - 0.78539816 +
     1            T*(-0.04166397 +
     2            T*(-0.00003954 +
     3            T*( 0.00262573 +
     4            T*(-0.00054125 +
     5            T*(-0.00029333 +
     6            T*( 0.00013558))))))
         PGBSJ0 = F0*COS(THETA0)/SQRT(X)
      END IF
C-----------------------------------------------------------------------
      END

      REAL FUNCTION PGBSJ1(XX)
      REAL XX
C-----------------------------------------------------------------------
C Bessel function of order 1 (approximate).
C Reference: Abramowitz and Stegun: Handbook of Mathematical Functions.
C-----------------------------------------------------------------------
      REAL X, XO3, T, F1, THETA1
C
      X = ABS(XX)
      IF (X .LE. 3.0) THEN
         XO3 = X/3.0
         T = XO3*XO3
         PGBSJ1 = 0.5 + T*(-0.56249985 +
     1                  T*( 0.21093573 +
     2                  T*(-0.03954289 +
     3                  T*( 0.00443319 +
     4                  T*(-0.00031761 +
     5                  T*( 0.00001109))))))
         PGBSJ1 = PGBSJ1*XX
      ELSE
         T = 3.0/X
         F1 =    0.79788456 +
     1       T*( 0.00000156 +
     2       T*( 0.01659667 + 
     3       T*( 0.00017105 +
     4       T*(-0.00249511 +
     5       T*( 0.00113653 + 
     6       T*(-0.00020033))))))
         THETA1 = X   -2.35619449 + 
     1             T*( 0.12499612 +
     2             T*( 0.00005650 +
     3             T*(-0.00637879 +
     4             T*( 0.00074348 +
     5             T*( 0.00079824 +
     6             T*(-0.00029166))))))
         PGBSJ1 = F1*COS(THETA1)/SQRT(X)
      END IF
      IF (XX .LT. 0.0) PGBSJ1 = -PGBSJ1
C-----------------------------------------------------------------------
      END

      REAL FUNCTION PGRNRM (ISEED)
      INTEGER ISEED
C-----------------------------------------------------------------------
C Returns a normally distributed deviate with zero mean and unit 
C variance. The routine uses the Box-Muller transformation of uniform
C deviates. For a more efficient implementation of this algorithm,
C see Press et al., Numerical Recipes, Sec. 7.2.
C
C Arguments:
C  ISEED  (in/out) : seed used for PGRAND random-number generator.
C
C Subroutines required:
C  PGRAND -- return a uniform random deviate between 0 and 1.
C
C History:
C  1995 Dec 12 - TJP.
C-----------------------------------------------------------------------
      REAL R, X, Y, PGRAND
C
 10   X = 2.0*PGRAND(ISEED) - 1.0
      Y = 2.0*PGRAND(ISEED) - 1.0
      R = X**2 + Y**2
      IF (R.GE.1.0) GOTO 10
      PGRNRM = X*SQRT(-2.0*LOG(R)/R)
C-----------------------------------------------------------------------
      END

      REAL FUNCTION PGRAND(ISEED)
      INTEGER ISEED
C-----------------------------------------------------------------------
C Returns a uniform random deviate between 0.0 and 1.0.
C
C NOTE: this is not a good random-number generator; it is only
C intended for exercising the PGPLOT routines.
C
C Based on: Park and Miller's "Minimal Standard" random number
C   generator (Comm. ACM, 31, 1192, 1988)
C
C Arguments:
C  ISEED  (in/out) : seed.
C-----------------------------------------------------------------------
      INTEGER   IM, IA, IQ, IR
      PARAMETER (IM=2147483647)
      PARAMETER (IA=16807, IQ=127773, IR= 2836)
      REAL      AM
      PARAMETER (AM=128.0/IM)
      INTEGER   K
C-
      K = ISEED/IQ
      ISEED = IA*(ISEED-K*IQ) - IR*K
      IF (ISEED.LT.0) ISEED = ISEED+IM
      PGRAND = AM*(ISEED/128)
      RETURN
      END
