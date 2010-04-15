C+
      SUBROUTINE FIG_SFIT2D(XV,YV,ZV,N,SAME,MATINV,COEFFS)
C
C     F I G _ S F I T 2 D
C
C     Fits a 2D parabola to a set of X,Y,Z points.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (>) XV     (Real array X(N)) The X values for the points.
C     (>) YV     (Real array Y(N)) The Y values for the points.
C     (>) ZV     (Real array Z(N)) The Z values for the points.
C     (>) N      (Integer) The number of X,Y,Z points to be
C                fitted.
C     (>) SAME   (Logical) If true, indicates that the X and Y
C                values have been used in a previous fit, and
C                that MATINV represents the inverse matrix
C                obtained as a result of that fit.  If only the
C                Z values have changed, the matrix inversion can
C                be bypassed, giving a substantial speed increase.
C     (!) MATINV (Double precision MATINV(6,6)) If SAME is true,
C                MATINV should contain the inverse matrix from a
C                previous fit.  If SAME is false, then on return
C                MATINV will contain the calculated inverse matrix.
C     (>) COEFFS (Double precision COEFFS(6)) The calculated
C                coefficients for the fit, with COEFFS(6) being
C                the constant term.
C
C     Method -
C
C     The function fitted is
C
C     Z = A*X**2 + B*Y**2 + C*X*Y + D*X + E*Y + F
C
C     Giving a matrix equation (for a least squares solution) -
C
C     ( X4   X2Y2   X3Y   X3   X2Y   X2  )  ( A )      ( X2Z )
C     ( X2Y2 Y4     XY3   XY2  Y3    Y2  )  ( B )      ( Y2Z )
C     ( X3Y  XY3    X2Y2  X2Y  XY2   XY  )  ( C )      ( XYZ )
C     ( X3   XY2    X2Y   X2   XY    X1  )  ( D )  =   ( XZ  )
C     ( X2Y  Y3     XY2   XY   Y2    Y1  )  ( E )      ( YZ  )
C     ( X2   Y2     XY    X1   Y1    X   )  ( F )      ( Z1  )
C
C     - within the typographical limits of a set of comment cards -
C
C     where  X4 reresents the sum of all the X**4 terms, X2Y2 represents
C     the sum of the X**2*Y**2 terms, etc..
C
C                                             KS / CIT  20th Dec 1982
C     Modified -
C
C     20 Nov 1984  KS / AAO.  Now uses NAG routine instead of Caltech
C                  routine BNDINV.
C     08 Apr 1987  KS / AAO.  Renamed (from GEN_SFIT2D), because its
C                  use of a NAG routine means it really should not
C                  be a GEN_ routine.
C     20 Apr 1995  HME / UoE, Starlink.  No longer use NAG.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL SAME
      INTEGER N
      REAL XV(N),YV(N),ZV(N)
      DOUBLE PRECISION COEFFS(6),MATINV(6,6)
C
C     Local variables
C
      DOUBLE PRECISION MATRIX (6,6),WKSPCE(6)
      DOUBLE PRECISION X4,X2Y2,X3Y,X3,X2,Y4,XY3,XY2,Y3,Y2
      DOUBLE PRECISION X2Y,XY,X1,Y1,X,Y
      DOUBLE PRECISION X2Z,Y2Z,XYZ,XZ,YZ,Z1,Z
      INTEGER I,J,ITEST
      INTEGER IPVT(6)
C
C     Equivalence is used to speed up the filling of the matrix
C
      EQUIVALENCE (MATRIX(1,1),X4),(MATRIX(2,1),X2Y2),(MATRIX(3,1),X3Y)
      EQUIVALENCE (MATRIX(4,1),X3),(MATRIX(5,1),X2Y), (MATRIX(6,1),X2)
      EQUIVALENCE (MATRIX(2,2),Y4),(MATRIX(3,2),XY3), (MATRIX(4,2),XY2)
      EQUIVALENCE (MATRIX(5,2),Y3),(MATRIX(6,2),Y2),  (MATRIX(6,3),XY)
      EQUIVALENCE (MATRIX(6,4),X1),(MATRIX(6,5),Y1)
C
C     Do we have to form the matrix?
C
      IF (.NOT.SAME) THEN
C
C        Yes, we do.  Initialise the sums.
C
         X4=0.
         X2Y2=0.
         X3Y=0.
         X3=0.
         X2Y=0.
         X2=0.
         Y4=0.
         XY3=0.
         XY2=0.
         Y3=0.
         Y2=0.
         XY=0.
         X1=0.
         Y1=0.
C
C        Form the sums
C
         DO I=1,N
            X=XV(I)
            Y=YV(I)
            X4=X4+X*X*X*X
            X2Y2=X2Y2+X*X*Y*Y
            X3Y=X3Y+X*X*X*Y
            X3=X3+X*X*X
            X2Y=X2Y+X*X*Y
            X2=X2+X*X
            Y4=Y4+Y*Y*Y*Y
            XY3=XY3+X*Y*Y*Y
            XY2=XY2+X*Y*Y
            Y3=Y3+Y*Y*Y
            Y2=Y2+Y*Y
            XY=XY+X*Y
            X1=X1+X
            Y1=Y1+Y
         END DO
C
C        Fill out the matrix terms that are not automaticaly
C        set by the equivalence statements.  (This is not as
C        elegant as using loops and the symmetry of the matrix,
C        but it's faster.)
C
         MATRIX(1,2)=X2Y2
         MATRIX(1,3)=X3Y
         MATRIX(2,3)=XY3
         MATRIX(3,3)=X2Y2
         MATRIX(4,3)=X2Y
         MATRIX(5,3)=XY2
         MATRIX(1,4)=X3
         MATRIX(2,4)=XY2
         MATRIX(3,4)=X2Y
         MATRIX(4,4)=X2
         MATRIX(5,4)=XY
         MATRIX(1,5)=X2Y
         MATRIX(2,5)=Y3
         MATRIX(3,5)=XY2
         MATRIX(4,5)=XY
         MATRIX(5,5)=Y2
         MATRIX(1,6)=X2
         MATRIX(2,6)=Y2
         MATRIX(3,6)=XY
         MATRIX(4,6)=X1
         MATRIX(5,6)=Y1
         MATRIX(6,6)=N
C
C        Invert the matrix
C
         DO J=1,6
            DO I=1,6
               MATINV(I,J)=MATRIX(I,J)
            END DO
         END DO
         CALL PDA_DGEFA(MATINV,6,6,IPVT,ITEST)
         IF (ITEST.EQ.0) CALL PDA_DGEDI(MATINV,6,6,IPVT,0D0,WKSPCE,1)
C
      END IF
C
C     Now, we have the inverse matrix.
C     Form the terms of the result column matrix
C
      X2Z=0.
      Y2Z=0.
      XYZ=0.
      XZ=0.
      YZ=0.
      Z1=0.
      DO I=1,N
         X=XV(I)
         Y=YV(I)
         Z=ZV(I)
         X2Z=X2Z+X*X*Z
         Y2Z=Y2Z+Y*Y*Z
         XYZ=XYZ+X*Y*Z
         XZ=XZ+X*Z
         YZ=YZ+Y*Z
         Z1=Z1+Z
      END DO
C
C     Determine the coefficients
C
      DO I=1,6
         COEFFS(I)=MATINV(1,I)*X2Z+MATINV(2,I)*Y2Z+MATINV(3,I)*XYZ
     :                +MATINV(4,I)*XZ+MATINV(5,I)*YZ+MATINV(6,I)*Z1
      END DO
C
      END
