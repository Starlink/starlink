      PROGRAM PGDEM9
C-----------------------------------------------------------------------
C Test program for PGPLOT: test of imaging routine PGPIXL.
C-----------------------------------------------------------------------
      INTEGER PGBEG
      INTEGER N, NCOL, NLEV
      PARAMETER (N=64, NCOL=32, NLEV=9)
      INTEGER I,J,CI1,CI2
      REAL F(N,N),FMIN,FMAX,R,G,B,CLEV(NLEV),TR(6)
      INTEGER IA(N,N)
C
C Compute a suitable function.
C
      FMIN = F(1,1)
      FMAX = F(1,1)
      DO 20 I=1,N
          DO 10 J=1,N
              F(I,J) = COS(0.6*SQRT(I*2.)-0.4*J/3.)*COS(0.4*I/3)+
     1                     (I-J)/REAL(N)
              FMIN = MIN(F(I,J),FMIN)
              FMAX = MAX(F(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
      DO 25 I=1,N
          DO 24 J=1,N
              IA(I,J) = (F(I,J)-FMIN)/(FMAX-FMIN)*(NCOL-1)+16
   24     CONTINUE
   25 CONTINUE
C
C Open plot device and set up coordinate system. We will plot the
C image within a unit square.
C
      IF (PGBEG(0,'?',1,1) .NE. 1) STOP
      CALL PGQCOL(CI1, CI2)
      IF (CI2.LT. 15+NCOL) THEN
          WRITE (*,*) 'This program requires a device with at least',
     1                15+NCOL,' colors'
          STOP
      END IF
      CALL PGPAGE
      CALL PGSCR(0, 0.0, 0.3, 0.2)
      CALL PGSVP(0.05,0.95,0.05,0.95)
      CALL PGWNAD(0.0, 1.0, 0.0, 1.0)
C
C Set up a color palette using NCOL indices from 16 to 15+NCOL.
C
      DO 30 I=1,NCOL
          R = REAL(I-1)/REAL(NCOL-1)*0.8 + 0.2
          G = MAX(0.0, 2.0*REAL(I-1-NCOL/2)/REAL(NCOL-1))
          B = 0.2 + 0.4*REAL(NCOL-I)/REAL(NCOL)
          CALL PGSCR(I+15, R, G, B)
   30 CONTINUE
C
C Use PGPIXL to plot the image.
C
      CALL PGPIXL(IA,N,N, 1, N, 1, N, 0.0, 1.0, 0.0, 1.0)
C
C Annotation.
C
      CALL PGSCI(1)
      CALL PGMTXT('t',1.0,0.0,0.0,'Test of PGPIXL')
      CALL PGBOX('bcnts',0.0,0,'bcnts',0.0,0)
C
C Overlay a contour map.
C
      TR(1) = -1.0/REAL(N-1)
      TR(2) = 1.0/REAL(N-1)
      TR(3) = 0.0
      TR(4) = -1.0/REAL(N-1)
      TR(5) = 0.0
      TR(6) = 1.0/REAL(N-1)
      DO 40 I=1,NLEV
          CLEV(I) = FMIN + (FMAX-FMIN)*REAL(I)/REAL(NLEV)
   40 CONTINUE
      CALL PGCONT(F, N, N, 1, N, 1, N, CLEV, NLEV, TR)
C
C Done.
C
      CALL PGEND
      END
