      PROGRAM PGDEM8
C
C From: Philip Palmer <philip@maths.qmw.ac.uk>
C Date: Mon, 26 Nov 90 12:27:22 GMT
C
C This program plots a 3d surface using PGSURF.
C
      INTEGER NCURVE
      PARAMETER(NCURVE=21)
      INTEGER I,J, PGBEG
      REAL A(NCURVE,NCURVE),XLIMS(3,2)
      REAL AA1,AA2,AA3,AA4
      REAL X,Y,DX,DY,THD,PHD,Q
      REAL POT
      COMMON/COEFS/AA1,AA2,AA3,AA4
C
      THD=40.
      PHD=-35.
      Q=0.1
      AA1=60./17.-24.*Q/17.
      AA2=12./17.
      AA3=52./17.
      AA4=24./17.
      XLIMS(1,1)=0.
      XLIMS(1,2)=0.43
      XLIMS(2,1)=-1.5
      XLIMS(2,2)=1.5
      XLIMS(3,1)=0.
      XLIMS(3,2)=1.
      DX=(XLIMS(1,2)-XLIMS(1,1))/(NCURVE-1)
      DY=(XLIMS(2,2)-XLIMS(2,1))/(NCURVE-1)
      DO 1 I=1,NCURVE
          X=XLIMS(1,1)+(I-1)*DX
          DO 2 J=1,NCURVE
              Y=XLIMS(2,1)+(J-1)*DY
              A(I,J)=POT(X,Y)
    2     CONTINUE
    1 CONTINUE
      IF (PGBEG(0,'?',1,1) .NE. 1) STOP
      CALL PGSURF(A,NCURVE,NCURVE,XLIMS,THD,PHD)
      CALL PGEND
      END

      REAL FUNCTION POT(X,Y)
      REAL X,Y
C
      REAL X2,Y2,XX,YY
      REAL AA1,AA2,AA3,AA4
      COMMON/COEFS/AA1,AA2,AA3,AA4
C
      Y2=Y*Y
      X2=12.*X*X
      XX=.5*(Y2+X2)
      YY=Y2/3.+Y*X2
      POT=1.-AA1*XX-AA2*YY+AA3*XX*XX+AA4*XX*YY
      RETURN
      END

      SUBROUTINE PGSURF(A,NX,NY,XLIMS,THD,PHD)
      INTEGER NX,NY
      REAL A(NX,NY),XLIMS(3,2),THD,PHD
C
C This routine plots a 3d surface projected onto a 2D plane.
C The underside of the surface appears dotted or in blue, on
C clour terminals. This routine does the projection for you,
C you just need to specify the viewing direction in terms of
C spherical polar angles.  As a result, this routine calls 
C pgwind for you.
C
C Arguments:
C  a (input) 	: data array, equally spaced in x and equally
C		  spaced in y, although steps in x and y may
C		  differ.
C  nx (input)	: dimension of data array in x direction.
C  ny (input)	: dimension of data array in y direction.
C  xlims (input): array of min and max values in x, y and z
C		  respectively.
C  thd (input) 	: viewing direction given as spherical theta angle
C		  in degrees. 0<= thd <= 90.
C  phd (input)	: viewing direction given as spherical phi angle
C		  in degrees. -180 <= phd <= 180.
C
      INTEGER I,J,K,N,IFLAG,I0,I1,NSTART,NLINE,ISGN,JSGN
      INTEGER IMAX,JMAX,K0,K1
      REAL DTOR, TH,PH,CTH,STH,CPH,SPH
      REAL XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,Z1,Z2,Q1,Q2,Q3
      REAL A1,A2,A3,A4,B1,B2,B3,B4,XSTART,YSTART
      REAL XPMIN,XPMAX,YPMIN,YPMAX,DX,DY,ZZ,T
      REAL XA(2),YA(2),ZA(2)
      COMMON/PGSRF1/CTH,STH,CPH,SPH,NLINE,NSTART,IFLAG
C
      IFLAG=0
      CALL PGQCOL(I0,I1)
      IF(I1.NE.1) IFLAG=1
      CALL PGBBUF
      DTOR=0.0174533
      TH=THD*DTOR
      PH=PHD*DTOR
      CTH=COS(TH)
      STH=SIN(TH)
      CPH=COS(PH)
      SPH=SIN(PH)
      XMIN=XLIMS(1,1)
      XMAX=XLIMS(1,2)
      YMIN=XLIMS(2,1)
      YMAX=XLIMS(2,2)
      ZMIN=XLIMS(3,1)
      ZMAX=XLIMS(3,2)
C
C Calculate plotting order from
C viewing orientation
C
      Z1=ZMIN*STH
      Z2=ZMAX*STH
      IF(PHD.GT.0.) THEN
        A1=-XMAX*SPH
        A2=-XMIN*SPH
        B3=-YMAX*SPH*CTH
        B4=-YMIN*SPH*CTH
        YSTART=YMAX
        ISGN=-1
      ELSE
        A1=-XMIN*SPH
        A2=-XMAX*SPH
        B3=-YMIN*SPH*CTH
        B4=-YMAX*SPH*CTH
        YSTART=YMIN
        ISGN=1
      ENDIF
      IF(ABS(PHD).LT.90.) THEN
        B1=YMIN*CPH
        B2=YMAX*CPH
        A3=-XMAX*CPH*CTH
        A4=-XMIN*CPH*CTH
        XSTART=XMAX
        JSGN=-1
      ELSE
        B1=YMAX*CPH
        B2=YMIN*CPH
        A3=-XMIN*CPH*CTH
        A4=-XMAX*CPH*CTH
        XSTART=XMIN
        JSGN=1
      ENDIF
      XPMIN=MIN(A1,B1)
      XPMAX=MAX(A2,B2)
      YPMAX=MAX(A4,B4,Z2)
      YPMIN=MIN(A3,B3,Z1)
      CALL WINDOW(XPMIN,XPMAX,YPMIN,YPMAX)
C
C Draw coordinate axes
C
      NSTART=0
      NLINE=1
      XA(1)=0.
      XA(2)=0.
      YA(1)=YMIN
      YA(2)=YMAX
      CALL PROJ(XA,YA,XA,1,1)
      NLINE=1
      YA(1)=XMIN
      YA(2)=XMAX
      CALL PROJ(YA,XA,XA,1,1)
      NLINE=1
      YA(1)=ZMIN
      YA(2)=ZMAX
      CALL PROJ(XA,XA,YA,1,1)
C
C Draw curves stepped in x
C
      IMAX=NX-1
      JMAX=NY-1
      DX=JSGN*(XMAX-XMIN)/IMAX
      DY=(YMAX-YMIN)/JMAX
      Q1=DX*STH*SPH
      Q2=DY*STH*CPH
      Q3=DX*DY*CTH
      IF(JSGN.EQ.1) THEN
         K0=1
      ELSE
         K0=NX
      ENDIF
      DO 1 I=0,IMAX
          NSTART=0
          NLINE=1
          XA(1)=XSTART+I*DX
          XA(2)=XA(1)
          K=K0+JSGN*I
          DO 2 J=0,JMAX-1
              YA(1)=YMIN+J*DY
              YA(2)=YA(1)+DY
              ZA(1)=A(K,J+1)
              ZA(2)=A(K,J+2)
              K1=K+JSGN
              IF(K1.GE.1.AND.K1.LE.NX) THEN
                  ZZ=A(K1,J+1)
              ELSE
                  K1=K-JSGN
                  ZZ=2.*ZA(1)-A(K1,J+1)
              ENDIF
          T=Q3-Q2*(ZZ-ZA(1))-Q1*(ZA(2)-ZA(1))
          T=JSGN*T
          IF(T.GT.0.) THEN
              N=1+IFLAG
          ELSE
              N=4
          ENDIF
          CALL PROJ(XA,YA,ZA,N,NX)
    2     CONTINUE
    1 CONTINUE
C
C Draw curves stepped in y
C
      DY=ISGN*DY
      DX=JSGN*DX
      Q1=JSGN*Q1
      Q2=ISGN*Q2
      Q3=ISGN*JSGN*Q3
      IF(ISGN.EQ.1) THEN
         K0=1
      ELSE
         K0=NY
      ENDIF
      DO 3 J=0,JMAX
          NSTART=0
          NLINE=1
          YA(1)=YSTART+J*DY
          YA(2)=YA(1)
          K=K0+ISGN*J
          DO 4 I=0,IMAX-1
              XA(1)=XMIN+I*DX
              XA(2)=XA(1)+DX
              ZA(1)=A(I+1,K)
              ZA(2)=A(I+2,K)
              K1=K+ISGN
              IF(K1.GE.1.AND.K1.LE.NY) THEN
                  ZZ=A(I+1,K1)
              ELSE
                  K1=K-ISGN
                  ZZ=2.*ZA(1)-A(I+1,K1)
              ENDIF
              T=Q3-Q1*(ZZ-ZA(1))-Q2*(ZA(2)-ZA(1))
              T=ISGN*T
              IF(T.GT.0.) THEN
                  N=1+IFLAG
              ELSE
                  N=4
              ENDIF
              CALL PROJ(XA,YA,ZA,N,NY)
    4     CONTINUE
    3 CONTINUE
      CALL PGEBUF
      RETURN
      END

      SUBROUTINE PROJ(X,Y,Z,N,NCURVE)
      REAL X(2),Y(2),Z(2)
      INTEGER N, NCURVE
C
      REAL XP(100),YP(100)
      REAL CTH,STH,CPH,SPH
      INTEGER NLINE,NSTART,IFLAG,M
      COMMON/PGSRF1/CTH,STH,CPH,SPH,NLINE,NSTART,IFLAG
      SAVE M
      IF(NLINE.EQ.1) THEN
          XP(1)=Y(1)*CPH-X(1)*SPH
          YP(1)=Z(1)*STH-(X(1)*CPH+Y(1)*SPH)*CTH
          XP(2)=Y(2)*CPH-X(2)*SPH
          YP(2)=Z(2)*STH-(X(2)*CPH+Y(2)*SPH)*CTH
          IF(N.NE.M) THEN
              M=N
              IF(IFLAG.EQ.0) THEN
                  CALL PGSLS(N)
              ELSE
                  CALL PGSCI(N)
              ENDIF
          ENDIF
          NLINE=2
      ELSE
          IF(N.NE.M) THEN
              CALL PGLINE(NLINE,XP,YP)
              NSTART=NSTART+NLINE-1
              M=N
              IF(IFLAG.EQ.0) THEN
                  CALL PGSLS(N)
              ELSE
                  CALL PGSCI(N)
              ENDIF
              XP(1)=XP(NLINE)
              YP(1)=YP(NLINE)
              NLINE=1
          ENDIF
          NLINE=NLINE+1
          XP(NLINE)=Y(2)*CPH-X(2)*SPH
          YP(NLINE)=Z(2)*STH-(X(2)*CPH+Y(2)*SPH)*CTH
      ENDIF
      IF(NLINE+NSTART.GE.NCURVE) CALL PGLINE(NLINE,XP,YP)
      RETURN
      END

      SUBROUTINE WINDOW(XMIN,XMAX,YMIN,YMAX)
      REAL XMIN, XMAX, YMIN, YMAX
C
C This subroutine sets up the standard pgwind, but with a
C cream background. It can be ported to any program.
C
      CALL PGPAGE
      CALL PGSCR(0,216/255.,216/255.,191/255.)
      CALL PGERAS
      CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
      CALL PGSCI(1)
      RETURN
      END
