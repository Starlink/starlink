C*PGCONF -- fill between two contours
C%void cpgconf(const float *a, int idim, int jdim, int i1, int i2, \
C% int j1, int j2, float c1, float c2, const float *tr);
C+
      SUBROUTINE PGCONF (A, IDIM, JDIM, I1, I2, J1, J2, C1, C2, TR)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      REAL    A(IDIM,JDIM), C1, C2, TR(6)
C
C Shade the region between two contour levels of a function defined on
C the nodes of a rectangular grid. The routine uses the current fill
C attributes, hatching style (if appropriate), and color index.
C
C If you want to both shade between contours and draw the contour
C lines, call this routine first (once for each pair of levels) and 
C then CALL PGCONT (or PGCONS) to draw the contour lines on top of the
C shading.
C
C Note 1: This routine is not very efficient: it generates a polygon
C fill command for each cell of the mesh that intersects the desired
C area, rather than consolidating adjacent cells into a single polygon.
C
C Note 2: If both contours intersect all four edges of a particular
C mesh cell, the program behaves badly and may consider some parts
C of the cell to lie in more than one contour range.
C
C Note 3: If a contour crosses all four edges of a cell, this
C routine may not generate the same contours as PGCONT or PGCONS
C (these two routines may not agree either). Such cases are always
C ambiguous and the routines use different approaches to resolving
C the ambiguity.
C
C Arguments:
C  A      (input)  : data array.
C  IDIM   (input)  : first dimension of A.
C  JDIM   (input)  : second dimension of A.
C  I1,I2  (input)  : range of first index to be contoured (inclusive).
C  J1,J2  (input)  : range of second index to be contoured (inclusive).
C  C1, C2 (input)  : contour levels; note that C1 must be less than C2.
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
C 03-Oct-1996 - new routine [TJP].
C-----------------------------------------------------------------------
      INTEGER  I, J, IC, NPT, LEV
      LOGICAL  PGNOTO
      REAL     DVAL(5), X(8), Y(8), DELTA, XX, YY, C, R
      INTEGER  IDELT(6)
      DATA     IDELT/0,-1,-1,0,0,-1/
C
C Check arguments.
C
      IF (PGNOTO('PGCONF')) RETURN
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     :    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2) RETURN
      IF (C1.GE.C2) RETURN
      CALL PGBBUF
C
      DO 140 J=J1+1,J2
         DO 130 I=I1+1,I2
            DVAL(1) = A(I-1,J)
            DVAL(2) = A(I-1,J-1)
            DVAL(3) = A(I,J-1)
            DVAL(4) = A(I,J)
            DVAL(5) = DVAL(1)
C
            NPT = 0
            DO 120 IC=1,4
               IF (DVAL(IC).GE.C1 .AND. DVAL(IC).LT.C2) THEN
                  NPT = NPT+1
                  XX = I+IDELT(IC+1)
                  YY = J+IDELT(IC)
                  X(NPT) = TR(1) + TR(2)*XX + TR(3)*YY
                  Y(NPT) = TR(4) + TR(5)*XX + TR(6)*YY
               END IF
               R = DVAL(IC+1)-DVAL(IC)
               IF (R.EQ.0.0) GOTO 120
               DO 110 LEV=1,2
                  IF (R.GT.0.0) THEN
                     C = C1
                     IF (LEV.EQ.2) C = C2
                  ELSE
                     C = C2
                     IF (LEV.EQ.2) C = C1
                  END IF
                  DELTA = (C-DVAL(IC))/R
                  IF (DELTA.GT.0.0 .AND. DELTA.LT.1.0) THEN
                     IF (IC.EQ.1 .OR. IC.EQ.3) THEN
                        XX = I+IDELT(IC+1)
                        YY = REAL(J+IDELT(IC)) + 
     :                       DELTA*REAL(IDELT(IC+1)-IDELT(IC))
                     ELSE
                        XX = REAL(I+IDELT(IC+1)) +
     :                       DELTA*REAL(IDELT(IC+2)-IDELT(IC+1))
                        YY = J+IDELT(IC)
                     END IF
                     NPT = NPT+1
                     X(NPT) = TR(1) + TR(2)*XX + TR(3)*YY
                     Y(NPT) = TR(4) + TR(5)*XX + TR(6)*YY
                  END IF
 110           CONTINUE
 120        CONTINUE
            IF (NPT.GE.3) CALL PGPOLY(NPT, X, Y)
 130     CONTINUE
 140  CONTINUE
      CALL PGEBUF
      END
