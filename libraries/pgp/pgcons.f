C*PGCONS -- contour map of a 2D data array (fast algorithm)
C%void cpgcons(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, const float *c, int nc, const float *tr);
C+
      SUBROUTINE PGCONS (A, IDIM, JDIM, I1, I2, J1, J2, C, NC, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, NC
      REAL    A(IDIM,JDIM), C(*), TR(6)
C
C Draw a contour map of an array. The map is truncated if
C necessary at the boundaries of the viewport.  Each contour line is
C drawn with the current line attributes (color index, style, and
C width).  This routine, unlike PGCONT, does not draw each contour as a
C continuous line, but draws the straight line segments composing each
C contour in a random order.  It is thus not suitable for use on pen
C plotters, and it usually gives unsatisfactory results with dashed or
C dotted lines.  It is, however, faster than PGCONT, especially if
C several contour levels are drawn with one call of PGCONS.
C
C Arguments:
C  A      (input)  : data array.
C  IDIM   (input)  : first dimension of A.
C  JDIM   (input)  : second dimension of A.
C  I1,I2  (input)  : range of first index to be contoured (inclusive).
C  J1,J2  (input)  : range of second index to be contoured (inclusive).
C  C      (input)  : array of contour levels (in the same units as the
C                    data in array A); dimension at least NC.
C  NC     (input)  : number of contour levels (less than or equal to
C                    dimension of C). The absolute value of this
C                    argument is used (for compatibility with PGCONT,
C                    where the sign of NC is significant).
C  TR     (input)  : array defining a transformation between the I,J
C                    grid of the array and the world coordinates. The
C                    world coordinates of the array point A(I,J) are
C                    given by:
C                      X = TR(1) + TR(2)*I + TR(3)*J
C                      Y = TR(4) + TR(5)*I + TR(6)*J
C                    Usually TR(3) and TR(5) are zero - unless the
C                    coordinate transformation involves a rotation
C                    or shear.
C--
C 27-Aug-1984 - [TJP].
C 21-Sep-1989 - Better treatment of the 'ambiguous' case [A. Tennant];
C               compute world coordinates internally and eliminate
C               dependence on common block [TJP].
C-----------------------------------------------------------------------
      INTEGER  I, IC, ICORN, IDELT(6), J, K, NPT
      INTEGER  IOFF(8), JOFF(8), IENC, ITMP, JTMP, ILO, ITOT
      LOGICAL  PGNOTO
      REAL     CTR, DELTA, DVAL(5), XX, YY, X(4), Y(4)
      INTRINSIC ABS
      DATA     IDELT/0,-1,-1,0,0,-1/
      DATA     IOFF/-2,-2,-1,-1, 0, 0, 1, 1/
      DATA     JOFF/ 0,-1,-2, 1,-2, 1,-1, 0/
C
C Check arguments.
C
      IF (PGNOTO('PGCONS')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2) RETURN
      IF (NC.EQ.0) RETURN
      CALL PGBBUF
C
      DO 130 J=J1+1,J2
      DO 130 I=I1+1,I2
          DVAL(1) = A(I-1,J)
          DVAL(2) = A(I-1,J-1)
          DVAL(3) = A(I,J-1)
          DVAL(4) = A(I,J)
          DVAL(5) = DVAL(1)
      DO 110 IC=1,ABS(NC)
          CTR = C(IC)
          NPT = 0
          DO 120 ICORN=1,4
          IF( (DVAL(ICORN).LT.CTR .AND. DVAL(ICORN+1).LT.CTR)
     1    .OR.(DVAL(ICORN).GE.CTR .AND. DVAL(ICORN+1).GE.CTR) ) GOTO 120
            NPT=NPT+1
            DELTA = (CTR-DVAL(ICORN))/(DVAL(ICORN+1)-DVAL(ICORN))
            GOTO (60,70,60,70), ICORN
C
   60       XX = I+IDELT(ICORN+1)
            YY = REAL(J+IDELT(ICORN)) + 
     1           DELTA*REAL(IDELT(ICORN+1)-IDELT(ICORN))
            GOTO 80
C
   70       XX = REAL(I+IDELT(ICORN+1)) +
     1           DELTA*REAL(IDELT(ICORN+2)-IDELT(ICORN+1))
            YY  = J+IDELT(ICORN)
C
   80       X(NPT) = TR(1) + TR(2)*XX + TR(3)*YY
            Y(NPT) = TR(4) + TR(5)*XX + TR(6)*YY
C
  120     CONTINUE
          IF (NPT.EQ.2) THEN
C             -- Contour crosses two sides of cell. Draw line-segment.
              CALL PGMOVE(X(1),Y(1))
              CALL PGDRAW(X(2),Y(2))
          ELSE IF (NPT.EQ.4) THEN
C             -- The 'ambiguous' case.  The routine must draw two line
C             segments here and there are two ways to do so.  The
C             following 4 lines would implement the original PGPLOT
C             method:
C            CALL PGCP(0,X(1),Y(1),CTR)
C            CALL PGCP(1,X(2),Y(2),CTR)
C            CALL PGCP(0,X(3),Y(3),CTR)
C            CALL PGCP(1,X(4),Y(4),CTR)
C            -- Choose between \\ and // based on the 8 points just
C            outside the current box.  If half or more of these points
C            lie below the contour level, then draw the lines such that
C            the high corners lie between the lines, otherwise, draw
C            the lines such that the low corners are enclosed.  Care is
C            taken to avoid going off the edge.
            ITOT=0
            ILO=0
            DO 140 K=1,8
               ITMP=I+IOFF(K)
               JTMP=J+JOFF(K)
               IF(ITMP.LT.I1 .OR. ITMP.GT.I2) GOTO 140
               IF(JTMP.LT.J1 .OR. JTMP.GT.J2) GOTO 140
               ITOT=ITOT+1
               IF(A(ITMP,JTMP).LT.CTR) ILO=ILO+1
  140       CONTINUE
            IENC=+1
            IF(ILO.LT.ITOT/2) IENC=-1
            IF(IENC.LT.0 .AND. DVAL(1).LT.CTR .OR.
     :         IENC.GT.0 .AND. DVAL(1).GE.CTR) THEN
               CALL PGMOVE(X(1),Y(1))
               CALL PGDRAW(X(2),Y(2))
               CALL PGMOVE(X(3),Y(3))
               CALL PGDRAW(X(4),Y(4))
            ELSE
               CALL PGMOVE(X(1),Y(1))
               CALL PGDRAW(X(4),Y(4))
               CALL PGMOVE(X(3),Y(3))
               CALL PGDRAW(X(2),Y(2))
            END IF
          END IF
  110     CONTINUE
  130 CONTINUE
C
      CALL PGEBUF
      END
