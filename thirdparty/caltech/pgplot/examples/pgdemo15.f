      PROGRAM PGDEM3
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT vector field plot.
C-----------------------------------------------------------------------
      INTEGER PGOPEN
      WRITE (*,'(A)') ' Demonstration of routine PGVECT'
C
C Call PGBEG to initiate PGPLOT and open the output device; PGBEG
C will prompt the user to supply the device name and type.
C
      IF (PGOPEN('?') .LE. 0) STOP
      CALL PGEX35
      CALL PGEND
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
      PARAMETER (PI=3.14159265359)
      DATA BLANK/-1.E10/
C
C compute the flow about a cylinder with circulation
C
C define various quantities
C
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
