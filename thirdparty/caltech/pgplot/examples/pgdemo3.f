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
      WRITE (*,'(A)') ' Routine PGCONF'
      CALL PGEXX1
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
      REAL F(40,40),FMIN,FMAX,ALEV(1),TR(6)
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
          ALEV(1) = FMIN + (I-1)*(FMAX-FMIN)/20.0
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
      REAL F(40,40),FMIN,FMAX,ALEV(1),TR(6)
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
          ALEV(1) = FMIN + (I-1)*(FMAX-FMIN)/20.0
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
      REAL F(40,40),FMIN,FMAX,ALEV(1),TR(6),X,Y,R
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
          ALEV(1) = FMIN + (I-1)*(FMAX-FMIN)/20.0
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
                  CALL PGPT1(X, Y, -1)
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
      REAL  PI, RPDEG
      PARAMETER (PI=3.14159265359)
      PARAMETER (RPDEG=PI/180.0)
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
      REAL  PI, RPDEG
      PARAMETER (PI=3.14159265359)
      PARAMETER (RPDEG=PI/180.0)
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

      SUBROUTINE PGEX37
C-----------------------------------------------------------------------
C Demonstration of contouring routine PGCONX.
C-----------------------------------------------------------------------
      INTEGER I,J
      REAL F(40,40),FMIN,FMAX,ALEV(1)
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
          ALEV(1) = FMIN + (I-1)*(FMAX-FMIN)/20.0
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
      REAL F(40,40),FMIN,FMAX,ALEV(1),TR(6)
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
          ALEV(1) = FMIN + (I-1)*(FMAX-FMIN)/20.0
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
          ALEV(1) = FMIN + (I-1)*(FMAX-FMIN)/20.0
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

      SUBROUTINE PGEXX1
C-----------------------------------------------------------------------
C Demonstration of contouring routine PGCONF.
C-----------------------------------------------------------------------
      INTEGER NX, NY, NC
      PARAMETER (NX=51, NY=51, NC=9)
      INTEGER I, J
      REAL Z(NX,NY),TR(6), R
      REAL X, Y, XMIN, XMAX, YMIN, YMAX, DX, DY, MU, C(NC)
      DATA C /3.0, 3.2, 3.5, 3.6, 3.766413, 4.0 ,5.0, 10.0, 100.0/     
C
C Compute a suitable function. This is the test function used by
C W. V. Snyder, Algorithm 531, Contour Plotting, ACM Trans. Math.
C Softw. v.4, pp.290-294 (1978).
C
      XMIN = -2.0
      XMAX = 2.0
      YMIN =-2.0
      YMAX = 2.0
      MU = 0.3
      DX = (XMAX-XMIN)/FLOAT(NX-1)                                      
      DY = (YMAX-YMIN)/FLOAT(NY-1)
      TR(1) = XMIN - DX
      TR(2) = DX
      TR(3) = 0.0
      TR(4) = YMIN - DY
      TR(5) = 0.0
      TR(6) = DY
      DO 20 I=1,NX
         X = TR(1) + I*TR(2)
         DO 10 J=1,NY     
            Y = TR(4) + J*TR(6)
            Z(I,J) = (1.0-MU)*(2.0/SQRT((X-MU)**2+Y**2)+(X-MU)**2+Y**2)   
     *           + MU*(2.0/SQRT((X+1.0-MU)**2+Y**2)+(X+1.0-MU)**2+Y**2)      
 10      CONTINUE                                   
   20 CONTINUE                                                          
C
C Clear the screen. Set up window and viewport.
C
      CALL PGPAGE
      CALL PGVSTD(0.05,0.95,0.05,0.95)
      CALL PGWNAD(XMIN, XMAX, YMIN, YMAX)
C
C Fill contours with PGCONF.
C
      CALL PGSFS(1)
      DO 30 I=1, NC-1
         R = 0.5+0.5*REAL(I-1)/REAL(NC-1)
         CALL PGSCR(I+10, R, R, R)
         CALL PGSCI(I+10)
         CALL PGCONF(Z,NX,NY,1,NX,1,NY,C(I),C(I+1),TR)
 30   CONTINUE
C
C Draw the contour lines with PGCONT.
C
      CALL PGSCI(3)
      CALL PGCONT(Z,NX,NY,1,NX,1,NY,C,NC,TR)
C
C Labels and box.
C
      CALL PGSCI(1)
      CALL PGSCH(0.6)
      CALL PGBOX('bctsin',1.0,10,'bctsinv',1.0,10)
      CALL PGSCH(1.0)
      CALL PGMTXT('t',1.0,0.0,0.0,'Contour filling using PGCONF')
C
      END
