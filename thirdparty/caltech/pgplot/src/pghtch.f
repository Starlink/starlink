C.PGHTCH -- hatch a polygonal area (internal routine)
C.
      SUBROUTINE PGHTCH(N, X, Y, DA)
      INTEGER N
      REAL X(*), Y(*), DA
C
C Hatch a polygonal area using equi-spaced parallel lines. The lines
C are drawn using the current line attributes: line style, line width,
C and color index. Cross-hatching can be achieved by calling this
C routine twice.
C
C Limitations: the hatching will not be done correctly if the
C polygon is so complex that a hatch line intersects more than
C 32 of its sides.
C
C Arguments:
C  N      (input)  : the number of vertices of the polygonal.
C  X,Y    (input)  : the (x,y) world-coordinates of the vertices
C                    (in order).
C  DA      (input) : 0.0 for normal hatching, 90.0 for perpendicular
C                    hatching.
C--
C Reference: I.O. Angel and G. Griffith "High-resolution computer
C graphics using Fortran 77", Halsted Press, 1987.
C
C 18-Feb-1995 [TJP].
C-----------------------------------------------------------------------
C
C MAXP is the maximum number of intersections any hatch line may make 
C with the sides of the polygon.
C
      INTEGER MAXP
      PARAMETER (MAXP=32)
      INTEGER NP(MAXP), I,J, II,JJ, NMIN,NMAX, NX, NI, NNP
      REAL ANGLE, SEPN, PHASE
      REAL RMU(MAXP), DX,DY, C, CMID,CMIN,CMAX, SX,SY, EX,EY, DELTA
      REAL QX,QY, R, RMU1, RMU2, XI,YI, BX,BY
      REAL DH, XS1, XS2, YS1, YS2, XL, XR, YT, YB, DINDX, DINDY
C
C Check arguments.
C
      IF (N.LT.3) RETURN
      CALL PGQHS(ANGLE, SEPN, PHASE)
      ANGLE = ANGLE + DA
      IF (SEPN.EQ.0.0) RETURN
C
C The unit spacing is 1 percent of the smaller of the height or
C width of the view surface. The line-spacing (DH), in inches, is
C obtained by multiplying this by argument SEPN.
C
      CALL PGQVSZ(1, XS1, XS2, YS1, YS2)
      DH = SEPN*MIN(ABS(XS2-XS1),ABS(YS2-YS1))/100.0
C
C DINDX and DINDY are the scales in inches per world-coordinate unit.
C
      CALL PGQVP(1, XS1, XS2, YS1, YS2)
      CALL PGQWIN(XL, XR, YB, YT)
      IF (XR.NE.XL .AND. YT.NE.YB) THEN
         DINDX = (XS2 - XS1) / (XR - XL)
         DINDY = (YS2 - YS1) / (YT - YB)
      ELSE
         RETURN
      END IF
C
C Initialize.
C
      CALL PGBBUF
C
C The vector (SX,SY) is a vector length DH perpendicular to
C the hatching lines, which have vector (DX,DY).
C
      DX = COS(ANGLE/57.29578)
      DY = SIN(ANGLE/57.29578)
      SX = (-DH)*DY
      SY = DH*DX
C
C The hatch lines are labelled by a parameter C, the distance from
C the coordinate origin. Calculate CMID, the C-value of the line
C that passes through the hatching reference point (BX,BY), and
C CMIN and CMAX, the range of C-values spanned by lines that intersect
C the polygon.
C
      BX = PHASE*SX
      BY = PHASE*SY
      CMID = DX*BY - DY*BX
      CMIN = DX*Y(1)*DINDY - DY*X(1)*DINDX
      CMAX = CMIN
      DO 10 I=2,N
         C = DX*Y(I)*DINDY - DY*X(I)*DINDX
         CMIN = MIN(C,CMIN)
         CMAX = MAX(C,CMAX)
 10   CONTINUE
C
C Compute integer labels for the hatch lines; N=0 is the line
C which passes through the reference point; NMIN and NMAX define
C the range of labels for lines that intersect the polygon.
C [Note that INT truncates towards zero; we need FLOOR and CEIL
C functions.]
C
      CMIN = (CMIN-CMID)/DH
      CMAX = (CMAX-CMID)/DH
      NMIN = INT(CMIN)
      IF (REAL(NMIN).LT.CMIN) NMIN = NMIN+1
      NMAX = INT(CMAX)
      IF (REAL(NMAX).GT.CMAX) NMAX = NMAX-1
C
C Each iteration of the following loop draws one hatch line.
C
      DO 60 J=NMIN,NMAX
C
C The parametric representation of this hatch line is
C (X,Y) = (QX,QY) + RMU*(DX,DY).
C
         QX = BX + REAL(J)*SX
         QY = BY + REAL(J)*SY
C
C Find the NX intersections of this line with the edges of the polygon.
C
         NX = 0
         NI = N
         DO 20 I=1,N
            EX = (X(I) - X(NI))*DINDX
            EY = (Y(I) - Y(NI))*DINDY
            DELTA = EX*DY - EY*DX
            IF (ABS(DELTA).LT.1E-5) THEN
C                 -- lines are parallel
            ELSE
C                 -- lines intersect in (XI,YI)
               R = ((QX-X(NI)*DINDX)*DY - (QY-Y(NI)*DINDY)*DX)/DELTA
               IF (R.GT.0.0 .AND. R.LE.1.0) THEN
                  IF (NX.LT.MAXP) NX = NX+1
                  NP(NX) = NX
                  IF (ABS(DX).GT.0.5) THEN
                     XI = X(NI)*DINDX + R*EX
                     RMU(NX) = (XI-QX)/DX
                  ELSE
                     YI = Y(NI)*DINDY + R*EY
                     RMU(NX) = (YI-QY)/DY
                  END IF
               END IF
            END IF
            NI = I
 20      CONTINUE
C     
C The RMU array now contains the intersections. Sort them into order.
C
         DO 40 II=1,NX-1
            DO 30 JJ=II+1,NX
               IF (RMU(NP(II)).LT.RMU(NP(JJ))) THEN
                  NNP = NP(II)
                  NP(II) = NP(JJ)
                  NP(JJ) = NNP
               END IF
 30         CONTINUE
 40      CONTINUE
C
C Join the intersections in pairs.
C
         NI = 1
C         -- do while NI < NX
 50      IF (NI .LT. NX) THEN
            RMU1 = RMU(NP(NI))
            RMU2 = RMU(NP(NI+1))
            CALL PGMOVE((QX+RMU1*DX)/DINDX, (QY+RMU1*DY)/DINDY)
            CALL PGDRAW((QX+RMU2*DX)/DINDX, (QY+RMU2*DY)/DINDY)
            NI = NI+2
            GOTO 50
         END IF
 60   CONTINUE
C
C Tidy up.
C
      CALL PGEBUF
C
      END
