      PROGRAM PGDEM3
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT contouring routines.
C-----------------------------------------------------------------------
      INTEGER PGBEG
      WRITE (*,'(A)') ' Demonstration of PGPLOT contouring routines'
C
C Call PGBEG to initiate PGPLOT and open the output device; PGBEG
C will prompt the user to supply the device name and type.
C
      IF (PGBEG(0,'?',1,1) .NE. 1) STOP
C
C Call the demonstration subroutines.
C
      WRITE (*,'(A)') ' Routine PGCONT'
      CALL PGEX31
      WRITE (*,'(A)') ' Routine PGCONS'
      CALL PGEX32
      WRITE (*,'(A)') ' Routine PGCONT with PGCONL labels'
      CALL PGEX36
      WRITE (*,'(A)') ' Routine PGCONB'
      CALL PGEX33
      WRITE (*,'(A)') ' Routine PGCONX with arrow labels'
      CALL PGEX37
      WRITE (*,'(A)') ' Routine PGCONX'
      CALL PGEX34
      WRITE (*,'(A)') ' Routine PGVECT'
      CALL PGEX35
C
C Finally, call PGEND to terminate things properly.
C
      CALL PGEND
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX31
C-----------------------------------------------------------------------
C Demonstration of contouring routine PGCONT.
C-----------------------------------------------------------------------
      INTEGER I,J
      REAL F(40,40),FMIN,FMAX,ALEV,TR(6)
      DATA TR/0.,1.,0.,0.,0.,1./
C
C Compute a suitable function.
C
      FMIN = 0.0
      FMAX = 0.0
      DO 20 I=1,40
          DO 10 J=1,40
              F(I,J) = COS(0.3*SQRT(I*2.)-0.4*J/3.)*COS(0.4*I/3)+
     1                     (I-J)/40.0
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
C
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL PGSVP(0.05,0.95,0.05,0.95)
      CALL PGSWIN(1.0,40.0,1.0,40.0)
      CALL PGBOX('bcts',0.0,0,'bcts',0.0,0)
      CALL PGMTXT('t',1.0,0.0,0.0,'Contouring using PGCONT')
C
C Draw the map.  PGCONT is called once for each contour, using
C different line attributes to distinguish contour levels.
C
      CALL PGBBUF
      DO 30 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSCI(2)
              CALL PGSLS(2)
          ELSE
              CALL PGSCI(3)
              CALL PGSLS(1)
          END IF
          CALL PGCONT(F,40,40,1,40,1,40,ALEV,-1,TR)
   30 CONTINUE
      CALL PGSLW(1)
      CALL PGSLS(1)
      CALL PGSCI(1)
      CALL PGEBUF
      END

      SUBROUTINE PGEX32
C-----------------------------------------------------------------------
C Demonstration of contouring routine PGCONS.
C-----------------------------------------------------------------------
      INTEGER I,J
      REAL F(40,40),FMIN,FMAX,ALEV,TR(6)
      DATA TR/0.,1.,0.,0.,0.,1./
C
C Compute a suitable function.
C
      FMIN = 0.0
      FMAX = 0.0
      DO 20 I=1,40
          DO 10 J=1,40
              F(I,J) = COS(0.3*SQRT(I*2.)-0.4*J/3.)*COS(0.4*I/3)+
     1                     (I-J)/40.0
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
C
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL PGBOX('bcts',0.0,0,'bcts',0.0,0)
      CALL PGMTXT('t',1.0,0.0,0.0,'Contouring using PGCONS')
C
C Draw the map.  PGCONS is called once for each contour, using
C different line attributes to distinguish contour levels.
C
      CALL PGBBUF
      DO 40 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSCI(2)
              CALL PGSLS(2)
          ELSE
              CALL PGSCI(3)
              CALL PGSLS(1)
          END IF
          CALL PGCONS(F,40,40,1,40,1,40,ALEV,-1,TR)
   40 CONTINUE
      CALL PGSLW(1)
      CALL PGSLS(1)
      CALL PGSCI(1)
      CALL PGEBUF
      END

      SUBROUTINE PGEX33
C-----------------------------------------------------------------------
C Demonstration of contouring routine PGCONB.
C-----------------------------------------------------------------------
      REAL BLANK
      PARAMETER (BLANK=-1.2E20)
      INTEGER I,J
      REAL F(40,40),FMIN,FMAX,ALEV,TR(6),X,Y,R
      DATA TR/0.,1.,0.,0.,0.,1./
C
C Compute a suitable function.
C
      FMIN = 0.0
      FMAX = 0.0
      DO 20 I=1,40
          DO 10 J=1,40
              F(I,J) = COS(0.3*SQRT(I*2.)-0.4*J/3.)*COS(0.4*I/3)+
     1                     (I-J)/40.0
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
C
C "Blank" the data outside an annulus.
C
      DO 60 I=1,40
          DO 50 J=1,40
              R = SQRT((I-20.5)**2 + (J-20.5)**2)
              IF (R.GT.20.0 .OR. R.LT.3.0) F(I,J) = BLANK
   50     CONTINUE
   60 CONTINUE
C
      CALL PGPAGE
      CALL PGBOX('bcts',0.0,0,'bcts',0.0,0)
      CALL PGMTXT('t',1.0,0.0,0.0,'Contouring using PGCONB')
      CALL PGBBUF
      DO 80 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSCI(2)
              CALL PGSLS(2)
          ELSE
              CALL PGSCI(3)
              CALL PGSLS(1)
          END IF
          CALL PGCONB(F,40,40,1,40,1,40,ALEV,-1,TR,BLANK)
   80 CONTINUE
      CALL PGEBUF
C
C Mark the blanked points for easy identification.
C
      CALL PGBBUF
      CALL PGSCI(1)
      DO 100 I=1,40
          DO 90 J=1,40
              IF (F(I,J).EQ.BLANK) THEN
                  X = TR(1) + REAL(I)*TR(2) + REAL(J)*TR(3)
                  Y = TR(4) + REAL(I)*TR(5) + REAL(J)*TR(6)
                  CALL PGPT(1, X, Y, -1)
              END IF
   90     CONTINUE
  100 CONTINUE
      CALL PGEBUF
      END

      SUBROUTINE PGEX34
C-----------------------------------------------------------------------
C This program is intended to demonstrate the use of the PGPLOT routine
C PGCONX. As an example, we take data defined on a sphere. We want to
C draw a contour map of the data on an equal-area projection of the
C surface of the sphere; we choose the Hammer-Aitoff equal-area
C projection centered on Declination (latitude) 0, Right Ascension
C (longitude) 0. The data are defined at 2-degree intervals in both
C coordinates. We thus need a data array dimensioned 181 by 91; the
C array index runs from -90 to +90 in declination (91 elements) and
C from -180 to +180 in right ascension (181 elements). The data at -180
C and +180 must be identical, of course, but they need to be duplicated
C in the array as these two longitudes appear on opposite sides of the
C map.
C-----------------------------------------------------------------------
      REAL  RPDEG
      PARAMETER (RPDEG=3.1415926/180.0)
      INTEGER I, J
      REAL RA, DEC, B, L, XC(181), YC(181)
      REAL Q(181,91), C(9)
      EXTERNAL PLCAIT
C
C Call PGENV to create a rectangular window of 4 x 2 units. This is
C the bounding rectangle of the plot. The JUST argument is 1
C to get equal scales in x and y.
C
      CALL PGBBUF
      CALL PGENV(-2.0, 2.0, -1.0, 1.0, 1, -2)
      CALL PGLAB('Right Ascension', 'Declination', ' ')
      CALL PGMTXT('t',2.0,0.0,0.0,
     1           'Contouring on a non-Cartesian grid using PGCONX')
      CALL PGSCH(0.6)
      CALL PGMTXT('b',8.0,0.0,0.0,
     1            'Hammer-Aitoff Equal-Area Projection of the Sphere')
      CALL PGSCH(1.0)
C
C Draw 7 lines of constant longitude at longitude 0, 60, 120, ...,
C 360 degrees. Each line is made up of 90 straight-line segments.
C
      DO 20 J=1,7
          RA = (-180.+(J-1)*60.)*RPDEG
          DO 10 I=1,91
              DEC = 2*(I-46)*RPDEG
              CALL AITOFF(DEC,RA,XC(I),YC(I))
   10     CONTINUE
          CALL PGLINE(91,XC,YC)
   20 CONTINUE
C
C Draw 5 lines of constant latitude at latitudes -60, -30, 0, 30,
C 60 degrees. Each line is made up of 360 straight-line segments.
C
      DO 40 J=1,5
          DEC = (-60.+(J-1)*30.)*RPDEG
          DO 30 I=1,181
              RA = 2*(I-91)*RPDEG
              CALL AITOFF(DEC,RA,XC(I),YC(I))
   30     CONTINUE
          CALL PGLINE(181,XC,YC)
   40 CONTINUE
      CALL PGEBUF
C
C Compute the data to be contoured. In practice the data might be read
C in from an external file. In this example the data are computed: they
C are the galactic latitudes of the points on the sphere. Thus the
C contours will be lines of constant galactic latitude.
C
      DO 60 J=1,91
          DEC = 2*(J-46)*RPDEG
          DO 50 I=1,181
              RA = 2*(I-91)*RPDEG
              CALL GALACT(RA, DEC, B,L)
              Q(I,J) = B
   50     CONTINUE
   60 CONTINUE
C
C Draw the contour map using PGCONX. Contours at 0, 20, 40, 60, 80.
C
      DO 70 I=1,9
          C(I) = -100.0 +I*20.0
   70 CONTINUE
      CALL PGBBUF
      CALL PGSCI(2)
      CALL PGCONX(Q, 181, 91, 1, 181, 1, 91, C, 9, PLCAIT)
      CALL PGSCI(1)
      CALL PGEBUF
      END

      SUBROUTINE PLCAIT(VISBLE, X, Y, Z)
      INTEGER VISBLE
      REAL X,Y,Z
C-----------------------------------------------------------------------
C Plotting subroutine for PGCONX. This routine converts the input
C coordinates (latitude and longitude) into the projected coordinates
C (x and y), and moves or draws as requested by VISBLE.
C-----------------------------------------------------------------------
      REAL  RPDEG
      PARAMETER (RPDEG=3.1415926/180.0)
      REAL B, L, XWORLD, YWORLD
      B = 2.0*(Y-46.0)*RPDEG
      L = 2.0*(X-91.0)*RPDEG
      CALL AITOFF(B, L, XWORLD, YWORLD)
      IF (VISBLE.EQ.0) THEN
          CALL PGMOVE(XWORLD, YWORLD)
      ELSE
          CALL PGDRAW(XWORLD, YWORLD)
      END IF
      END

      SUBROUTINE AITOFF(B,L,X,Y)
C-----------------------------------------------------------------------
C Hammer-Aitoff projection.
C
C       Input: latitude and longitude (B,L) in radians
C       Output: cartesian (X,Y) in range +/-2, +/-1
C-----------------------------------------------------------------------
      REAL L,B,X,Y,L2,DEN
C
      L2 = L/2.0
      DEN = SQRT(1.0+COS(B)*COS(L2))
      X = 2.0*COS(B)*SIN(L2)/DEN
      Y = SIN(B)/DEN
      END

      SUBROUTINE GALACT(RA,DEC,GLAT,GLONG)
C-----------------------------------------------------------------------
C Convert 1950.0 equatorial coordinates (RA, DEC) to galactic
C latitude and longitude (GLAT, GLONG).
C
C Arguments:
C  RA, DEC (input): 1950.0 RA and Dec (radians).
C  GLAT, GLONG (output): galactic latitude and longitude
C      (degrees).
C
C Reference: e.g., D. R. H. Johnson and D. R. Soderblom, A. J. v93,
C  p864 (1987).
C-----------------------------------------------------------------------
      REAL RA, RRA, DEC, RDEC, CDEC, R(3,3), E(3), G(3)
      REAL RADDEG, GLAT, GLONG
      INTEGER I, J
      DATA R/-.066988740D0, .492728466D0,-.867600811D0,-.872755766D0,
     $       -.450346958D0,-.188374601D0,-.483538915D0, .744584633D0,
     $        .460199785D0/
      DATA RADDEG/57.29577951D0/
C-----------------------------------------------------------------------
      RRA = RA
      RDEC = DEC
      CDEC = COS(RDEC)
      E(1) = CDEC*COS(RRA)
      E(2) = CDEC*SIN(RRA)
      E(3) = SIN(RDEC)
      DO 20 I=1,3
          G(I) = 0.0
          DO 10 J=1,3
              G(I) = G(I) + E(J)*R(I,J)
   10     CONTINUE
   20 CONTINUE
      GLAT  = ASIN(G(3))*RADDEG
      GLONG = ATAN2(G(2),G(1))*RADDEG
      IF (GLONG.LT.0.0) GLONG = GLONG+360.0
      RETURN
C-----------------------------------------------------------------------
      END

      SUBROUTINE PGEX35
C-----------------------------------------------------------------------
C Program to demonstrate the use of PGVECT along with
C PGCONB by illustrating the flow around a cylinder with circulation.
C
C          NX      total # of axial stations
C          NY      total # of grid pts in y (or r) direction
C-----------------------------------------------------------------------
      INTEGER MAXX, MAXY
      PARAMETER (MAXX=101,MAXY=201)
      INTEGER NX, NY, I, J, NC
      REAL PI, A, GAMMA, VINF, XMAX, XMIN, YMAX, YMIN, DX, DY
      REAL CPMIN, R2, BLANK
      REAL CP(MAXX,MAXY),X(MAXX),Y(MAXY),U(MAXX,MAXY),V(MAXX,MAXY),
     1   PSI(MAXX,MAXY)
      REAL TR(6),C(10)
      DATA BLANK/-1.E10/
C
C compute the flow about a cylinder with circulation
C
C define various quantities
C
C pi
      PI = ACOS(-1.)
C number of points in the x and y directions
      NX = 31
      NY = 31
C cylinder radius
      A = 1.
C circulation strength
      GAMMA = 2.
C freestream velocity
      VINF = 1.
C max and min x and y
      XMAX = 3.*A
      XMIN = -3.*A
      YMAX = 3.*A
      YMIN = -3.*A
C point spacing
      DX = (XMAX-XMIN)/(NX-1)
      DY = (YMAX-YMIN)/(NY-1)
C compute the stream function, Cp, and u and v velocities
      CPMIN =1.E10
      DO 20 I=1,NX
         X(I) = XMIN+DX*(I-1)
         DO 10 J=1,NY
            Y(J) = YMIN+DY*(J-1)
            R2 = X(I)**2+Y(J)**2
            IF (R2.GT.0.) THEN
               PSI(I,J) = VINF*Y(J)*(1.-A**2/R2)
     1            +GAMMA/(2.*PI)*0.5*ALOG(R2/A)
               U(I,J) = VINF*(1.+A**2/R2-2.*A**2*X(I)**2/R2**2)
     1            +GAMMA/(2.*PI)*Y(J)/R2
               V(I,J) = VINF*X(I)*(-2.*A**2*Y(J)/R2**2)
     1            +GAMMA/(2.*PI)*X(I)/R2
               CP(I,J) = 1.-(U(I,J)**2+V(I,J)**2)/VINF**2
            ELSE
               PSI(I,J) = 0.
               U(I,J) = 0.
               V(I,J) = 0.
               CP(I,J) = 0.
            END IF
            IF (R2.LT.A**2) THEN
               U(I,J) = BLANK
               V(I,J) = BLANK
            ELSE
               CPMIN = MIN(CPMIN,CP(I,J))
            END IF
   10    CONTINUE
   20 CONTINUE
C
C grid to world transformation
C
      TR(1)=X(1)-DX
      TR(2)=DX
      TR(3)=0.0
      TR(4)=Y(1)-DY
      TR(5)=0.0
      TR(6)=DY
C
      CALL PGENV (X(1),X(NX),Y(1),Y(NY),1,0)
      CALL PGIDEN
      CALL PGLAB ('X','Y','Flow About a Cylinder with Circulation')
C
C contour plot of the stream function (streamlines)
C
      NC=5
      C(1)=1.
      C(2)=.5
      C(3)=0.
      C(4)=-.5
      C(5)=-1.
      CALL PGCONT (PSI,MAXX,MAXY,1,NX,1,NY,C,NC,TR)
C
C draw cylinder
C
      CALL PGBBUF
      CALL PGSCI (0)
      CALL PGSFS (1)
      CALL PGCIRC (0.,0.,A*1.1)
      CALL PGSFS (2)
      CALL PGSCI (14)
      CALL PGCIRC (0.0, 0., A)
      CALL PGSCI (1)
      CALL PGEBUF
C
C vector plot
C
      CALL PGSAH (2, 45.0, 0.7)
      CALL PGSCH (0.3)
      CALL PGVECT (U,V,MAXX,MAXY,2,NX-1,2,NY-1,0.0,0,TR,-1.E10)
      CALL PGSCH(1.0)
C
C finished
C
      RETURN
C----------------------------------------------------------------------
      END

      SUBROUTINE PGEX37
C-----------------------------------------------------------------------
C Demonstration of contouring routine PGCONX.
C-----------------------------------------------------------------------
      INTEGER I,J
      REAL F(40,40),FMIN,FMAX,ALEV
      EXTERNAL PLCARO
C
C Compute a suitable function.
C
      FMIN = 0.0
      FMAX = 0.0
      DO 20 I=1,40
          DO 10 J=1,40
              F(I,J) = COS(0.3*SQRT(I*2.)-0.4*J/3.)*COS(0.4*I/3)+
     1                     (I-J)/40.0
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
C
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL PGSVP(0.05,0.95,0.05,0.95)
      CALL PGSWIN(1.0,40.0,1.0,40.0)
      CALL PGBOX('bcts',0.0,0,'bcts',0.0,0)
      CALL PGMTXT('t',1.0,0.0,0.0,
     :            'Contouring using PGCONX with arrows')
C
C Draw the map.  PGCONX is called once for each contour, using
C different line attributes to distinguish contour levels.
C
      CALL PGBBUF
      DO 30 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSCI(2)
              CALL PGSLS(2)
          ELSE
              CALL PGSCI(3)
              CALL PGSLS(1)
          END IF
          CALL PGCONX(F,40,40,1,40,1,40,ALEV,-1,PLCARO)
   30 CONTINUE
      CALL PGSLW(1)
      CALL PGSLS(1)
      CALL PGSCI(1)
      CALL PGEBUF
      END

      SUBROUTINE PGEX36
C-----------------------------------------------------------------------
C Demonstration of contouring routine PGCONT and PGCONL.
C-----------------------------------------------------------------------
      INTEGER I,J
      REAL F(40,40),FMIN,FMAX,ALEV,TR(6)
      CHARACTER*32 LABEL
      DATA TR /0.0, 1.0, 0.0, 0.0, 0.0, 1.0/
C
C Compute a suitable function.
C
      FMIN = 0.0
      FMAX = 0.0
      DO 20 I=1,40
          DO 10 J=1,40
              F(I,J) = COS(0.3*SQRT(I*2.)-0.4*J/3.)*COS(0.4*I/3)+
     1                     (I-J)/40.0
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
C
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL PGBOX('bcts',0.0,0,'bcts',0.0,0)
      CALL PGMTXT('t',1.0,0.0,0.0,
     1            'Contouring using PGCONT and PGCONL labels')
C
C Draw the map.  PGCONT is called once for each contour, using
C different line attributes to distinguish contour levels.
C
      CALL PGBBUF
      DO 40 I=1,21
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          IF (MOD(I,5).EQ.0) THEN
              CALL PGSLW(3)
          ELSE
              CALL PGSLW(1)
          END IF
          IF (I.LT.10) THEN
              CALL PGSCI(2)
              CALL PGSLS(2)
          ELSE
              CALL PGSCI(3)
              CALL PGSLS(1)
          END IF
          CALL PGCONT(F,40,40,1,40,1,40,ALEV,-1,TR)
   40 CONTINUE
      CALL PGSLW(1)
      CALL PGSLS(1)
      CALL PGEBUF
C
C Label the contours with PGCONL. Only even-numbered contours
C are labelled.
C
      CALL PGBBUF
      DO 50 I=2,21,2
          ALEV = FMIN + (I-1)*(FMAX-FMIN)/20.0
          WRITE (LABEL,'(I2)') I
C         WRITE (LABEL,'(F8.2)') ALEV
          IF (I.LT.10) THEN
              CALL PGSCI(2)
          ELSE
              CALL PGSCI(3)
          END IF
          CALL PGCONL(F,40,40,1,40,1,40,ALEV,TR,LABEL,16,8)
 50   CONTINUE
      CALL PGSCI(1)
      CALL PGEBUF
      END

      SUBROUTINE PLCARO(VISBLE, X, Y, Z)
      INTEGER VISBLE
      REAL X,Y,Z
C-----------------------------------------------------------------------
C Plotting subroutine for PGCONX. This routine labels contours with
C arrows. Arrows point clockwise around minima, anticlockwise around
C maxima. Arrows are drawn on 1/16 of contour line segments.
C-----------------------------------------------------------------------
      REAL XP, YP
      INTEGER I
      SAVE I
      DATA I /0/
C
      I = MOD(I+1,16)
      IF (VISBLE.EQ.0) THEN
          I = 0
          CALL PGMOVE(X, Y)
      ELSE IF (I.EQ.8) THEN
C         -- Draw line segment with arrow at midpoint
          CALL PGQPOS(XP,YP)
          CALL PGARRO(XP, YP, (X+XP)*0.5, (Y+YP)*0.5)
          CALL PGDRAW(X, Y)
      ELSE
C         -- Draw plain line segment
          CALL PGDRAW(X, Y)
      END IF
      END
