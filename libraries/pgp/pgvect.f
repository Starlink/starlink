C*PGVECT -- vector map of a 2D data array, with blanking
C%void cpgvect(const float *a, const float *b, int idim, int jdim, \
C% int i1, int i2, int j1, int j2, float c, int nc, \
C% const float *tr, float blank);
C+
      SUBROUTINE PGVECT (A, B, IDIM, JDIM, I1, I2, J1, J2, C, NC, TR,
     1                   BLANK)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, NC
      REAL    A(IDIM,JDIM), B(IDIM, JDIM), TR(6), BLANK, C
C
C Draw a vector map of two arrays.  This routine is similar to
C PGCONB in that array elements that have the "magic value" defined by
C the argument BLANK are ignored, making gaps in the vector map.  The
C routine may be useful for data measured on most but not all of the
C points of a grid. Vectors are displayed as arrows; the style of the
C arrowhead can be set with routine PGSAH, and the the size of the
C arrowhead is determined by the current character size, set by PGSCH.
C
C Arguments:
C  A      (input)  : horizontal component data array.
C  B      (input)  : vertical component data array.
C  IDIM   (input)  : first dimension of A and B.
C  JDIM   (input)  : second dimension of A and B.
C  I1,I2  (input)  : range of first index to be mapped (inclusive).
C  J1,J2  (input)  : range of second index to be mapped (inclusive).
C  C      (input)  : scale factor for vector lengths, if 0.0, C will be
C                    set so that the longest vector is equal to the
C                    smaller of TR(2)+TR(3) and TR(5)+TR(6).
C  NC     (input)  : vector positioning code.
C                    <0 vector head positioned on coordinates
C                    >0 vector base positioned on coordinates
C                    =0 vector centered on the coordinates
C  TR     (input)  : array defining a transformation between the I,J
C                    grid of the array and the world coordinates. The
C                    world coordinates of the array point A(I,J) are
C                    given by:
C                      X = TR(1) + TR(2)*I + TR(3)*J
C                      Y = TR(4) + TR(5)*I + TR(6)*J
C                    Usually TR(3) and TR(5) are zero - unless the
C                    coordinate transformation involves a rotation
C                    or shear.
C  BLANK   (input) : elements of arrays A or B that are exactly equal to
C                    this value are ignored (blanked).
C--
C  4-Sep-1992: derived from PGCONB [J. Crane].
C 26-Nov-1992: revised to use PGARRO [TJP].
C 25-Mar-1994: correct error for NC not =0 [G. Gonczi].
C  5-Oct-1996: correct error in computing max vector length [TJP;
C              thanks to David Singleton].
C-----------------------------------------------------------------------
      INTEGER  I, J
      REAL X, Y, X1, Y1, X2, Y2
      REAL CC
      INTRINSIC SQRT, MAX, MIN
C
C Define grid to world transformation
C
      X(I,J) = TR(1) + TR(2)*I + TR(3)*J
      Y(I,J) = TR(4) + TR(5)*I + TR(6)*J
C
C Check arguments.
C
      IF (I1.LT.1 .OR. I2.GT.IDIM .OR. I1.GE.I2 .OR.
     1    J1.LT.1 .OR. J2.GT.JDIM .OR. J1.GE.J2) THEN
C        CALL GRWARN('PGVECT: invalid range I1:I2, J1:J2')
         RETURN
      END IF
C
C Check for scale factor C.
C
      CC = C
      IF (CC.EQ.0.0) THEN
         DO 20 J=J1,J2
            DO 10 I=I1,I2
               IF (A(I,J).NE.BLANK .AND. B(I,J).NE.BLANK)
     1              CC = MAX(CC,SQRT(A(I,J)**2+B(I,J)**2))
 10         CONTINUE
 20      CONTINUE
         IF (CC.EQ.0.0) RETURN
         CC = SQRT(MIN(TR(2)**2+TR(3)**2,TR(5)**2+TR(6)**2))/CC
      END IF
C
      CALL PGBBUF
C
      DO 40 J=J1,J2
         DO 30 I=I1,I2
C
C Ignore vector if element of A and B are both equal to BLANK
C
            IF (.NOT.(A(I,J).EQ.BLANK .AND. B(I,J).EQ.BLANK)) THEN
 
C
C Define the vector starting and end points according to NC.
C
               IF (NC.LT.0) THEN
                  X2 = X(I,J)
                  Y2 = Y(I,J)
                  X1 = X2 - A(I,J)*CC
                  Y1 = Y2 - B(I,J)*CC
               ELSE IF (NC.EQ.0) THEN
                  X2 = X(I,J) + 0.5*A(I,J)*CC
                  Y2 = Y(I,J) + 0.5*B(I,J)*CC
                  X1 = X2 - A(I,J)*CC
                  Y1 = Y2 - B(I,J)*CC
               ELSE
                  X1 = X(I,J)
                  Y1 = Y(I,J)
                  X2 = X1 + A(I,J)*CC
                  Y2 = Y1 + B(I,J)*CC
               END IF
C     
C Draw vector.
C
               CALL PGARRO(X1, Y1, X2, Y2)
            END IF
 30      CONTINUE
 40   CONTINUE
C
      CALL PGEBUF
      END
