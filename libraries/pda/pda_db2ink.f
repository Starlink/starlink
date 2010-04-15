      SUBROUTINE PDA_DB2INK(X,NX,Y,NY,FCN,LDF,
     :                      KX,KY,TX,TY,BCOEF,
     :                      WORK,IFLAG,STATUS)

C***BEGIN PROLOGUE  DB2INK
C***DATE WRITTEN   25 MAY 1982
C***REVISION DATE  25 MAY 1982
C***CATEGORY NO.  E1A
C***KEYWORDS  INTERPOLATION, TWO-DIMENSIONS, GRIDDED DATA, SPLINES,
C             PIECEWISE POLYNOMIALS
C***AUTHOR  BOISVERT, RONALD, NBS
C             SCIENTIFIC COMPUTING DIVISION
C             NATIONAL BUREAU OF STANDARDS
C             WASHINGTON, DC 20234
C***PURPOSE  DOUBLE PRECISION VERSION OF B2INK.
C            DB2INK DETERMINES A PIECEWISE POLYNOMIAL FUNCTION THAT
C            INTERPOLATES TWO-DIMENSIONAL GRIDDED DATA. USERS SPECIFY
C            THE POLYNOMIAL ORDER (DEGREE+1) OF THE INTERPOLANT AND
C            (OPTIONALLY) THE KNOT SEQUENCE.
C***DESCRIPTION
C
C   DB2INK determines the parameters of a  function  that  interpolates
C   the two-dimensional gridded data (X(i),Y(j),FCN(i,j)) for i=1,..,NX
C   and j=1,..,NY. The interpolating function and its  derivatives  may
C   subsequently be evaluated by the function DB2VAL.
C
C   The interpolating  function  is  a  piecewise  polynomial  function
C   represented as a tensor product of one-dimensional  B-splines.  The
C   form of this function is
C
C                          NX   NY
C              S(x,y)  =  SUM  SUM  a   U (x) V (y)
C                         i=1  j=1   ij  i     j
C
C   where the functions U(i)  and  V(j)  are  one-dimensional  B-spline
C   basis functions. The coefficients a(i,j) are chosen so that
C
C         S(X(i),Y(j)) = FCN(i,j)   for i=1,..,NX and j=1,..,NY
C
C   Note that  for  each  fixed  value  of  y  S(x,y)  is  a  piecewise
C   polynomial function of x alone, and for each fixed value of x  S(x,
C   y) is a piecewise polynomial function of y alone. In one  dimension
C   a piecewise polynomial may  be  created  by  partitioning  a  given
C   interval into subintervals and defining a distinct polynomial piece
C   on each one. The points where adjacent subintervals meet are called
C   knots. Each of the functions U(i) and V(j)  above  is  a  piecewise
C   polynomial.
C
C   Users of DB2INK choose  the  order  (degree+1)  of  the  polynomial
C   pieces used to define the piecewise polynomial in each of the x and
C   y directions (KX and KY). Users also  may  define  their  own  knot
C   sequence in x and y separately (TX and TY).  If  IFLAG=0,  however,
C   DB2INK will choose sequences of knots that result  in  a  piecewise
C   polynomial interpolant with KX-2 continuous partial derivatives  in
C   x and KY-2 continuous partial derivatives in y. (KX knots are taken
C   near each endpoint in the x direction,  not-a-knot  end  conditions
C   are used, and the remaining knots are placed at data points  if  KX
C   is even or at midpoints between data points if KX  is  odd.  The  y
C   direction is treated similarly.)
C
C   After a call to DB2INK, all information  necessary  to  define  the
C   interpolating function are contained in the parameters NX, NY,  KX,
C   KY, TX, TY, and BCOEF. These quantities should not be altered until
C   after the last call of the evaluation routine DB2VAL.
C
C
C   I N P U T
C   ---------
C
C   X       Double precision 1D array (size NX)
C           Array of x abcissae. Must be strictly increasing.
C
C   NX      Integer scalar (.GE. 3)
C           Number of x abcissae.
C
C   Y       Double precision 1D array (size NY)
C           Array of y abcissae. Must be strictly increasing.
C
C   NY      Integer scalar (.GE. 3)
C           Number of y abcissae.
C
C   FCN     Double precision 2D array (size LDF by NY)
C           Array of function values to interpolate. FCN(I,J) should
C           contain the function value at the point (X(I),Y(J))
C
C   LDF     Integer scalar (.GE. NX)
C           The actual leading dimension of FCN used in the calling
C           calling program.
C
C   KX      Integer scalar (.GE. 2, .LT. NX)
C           The order of spline pieces in x.
C           (Order = polynomial degree + 1)
C
C   KY      Integer scalar (.GE. 2, .LT. NY)
C           The order of spline pieces in y.
C           (Order = polynomial degree + 1)
C
C
C   I N P U T   O R   O U T P U T
C   -----------------------------
C
C   TX      Double precision 1D array (size NX+KX)
C           The knots in the x direction for the spline interpolant.
C           If IFLAG=0 these are chosen by DB2INK.
C           If IFLAG=1 these are specified by the user.
C                      (Must be non-decreasing.)
C
C   TY      Double precision 1D array (size NY+KY)
C           The knots in the y direction for the spline interpolant.
C           If IFLAG=0 these are chosen by DB2INK.
C           If IFLAG=1 these are specified by the user.
C                      (Must be non-decreasing.)
C
C
C   O U T P U T
C   -----------
C
C   BCOEF   Double precision 2D array (size NX by NY)
C           Array of coefficients of the B-spline interpolant.
C           This may be the same array as FCN.
C
C
C   M I S C E L L A N E O U S
C   -------------------------
C
C   WORK    Double precision 1D array (size NX*NY + max( 2*KX*(NX+1),
C                                             2*KY*(NY+1) ))
C           Array of working storage.
C
C   IFLAG   Integer scalar.
C           On input:  0 == knot sequence chosen by DB2INK
C                      1 == knot sequence chosen by user.
C           On output: 1 == successful execution - Starlink modification
C                      2 == IFLAG out of range
C                      3 == NX out of range
C                      4 == KX out of range
C                      5 == X not strictly increasing
C                      6 == TX not non-decreasing
C                      7 == NY out of range
C                      8 == KY out of range
C                      9 == Y not strictly increasing
C                     10 == TY not non-decreasing
C
C   STATUS   Integer. Starlink error status.
C
C***REFERENCES  CARL DE BOOR, A PRACTICAL GUIDE TO SPLINES,
C                 SPRINGER-VERLAG, NEW YORK, 1978.
C               CARL DE BOOR, EFFICIENT COMPUTER MANIPULATION OF TENSOR
C                 PRODUCTS, ACM TRANSACTIONS ON MATHEMATICAL SOFTWARE,
C                 VOL. 5 (1979), PP. 173-182.
C***ROUTINES CALLED  DBTPCF,DBKNOT
C***END PROLOGUE  DB2INK
C
C  ------------
C  DECLARATIONS
C  ------------

C  Starlink error status.
      INTEGER STATUS

C
C  PARAMETERS
C

      INTEGER NX, NY, LDF, KX, KY, IFLAG
      DOUBLE PRECISION X(NX), Y(NY), FCN(LDF,NY), TX(1), TY(1)
      DOUBLE PRECISION BCOEF(NX,NY),WORK(1)
C
C  LOCAL VARIABLES
C
      INTEGER I, IW, NPK
C
C  -----------------------
C  CHECK VALIDITY OF INPUT
C  -----------------------
C
C***FIRST EXECUTABLE STATEMENT

*   Check the inherited global status.
      IF (STATUS.NE.0) RETURN

      IF ((IFLAG .LT. 0) .OR. (IFLAG .GT. 1))  GO TO 920
      IF (NX .LT. 3)  GO TO 930
      IF (NY .LT. 3)  GO TO 970
      IF ((KX .LT. 2) .OR. (KX .GE. NX))  GO TO 940
      IF ((KY .LT. 2) .OR. (KY .GE. NY))  GO TO 980
      DO 10 I=2,NX
         IF (X(I) .LE. X(I-1))  GO TO 950
   10 CONTINUE
      DO 20 I=2,NY
         IF (Y(I) .LE. Y(I-1))  GO TO 990
   20 CONTINUE
      IF (IFLAG .EQ. 0)  GO TO 50
         NPK = NX + KX
         DO 30 I=2,NPK
            IF (TX(I) .LT. TX(I-1))  GO TO 960
   30    CONTINUE
         NPK = NY + KY
         DO 40 I=2,NPK
            IF (TY(I) .LT. TY(I-1))  GO TO 1000
   40    CONTINUE
   50 CONTINUE
C
C  ------------
C  CHOOSE KNOTS
C  ------------
C
      IF (IFLAG .NE. 0)  GO TO 100
         CALL PDA_DBKNOT(X,NX,KX,TX,IFLAG)
         CALL PDA_DBKNOT(Y,NY,KY,TY,IFLAG)
  100 CONTINUE
C
C  -------------------------------
C  CONSTRUCT B-SPLINE COEFFICIENTS
C  -------------------------------
C
      IFLAG = 1
      IW = NX*NY + 1
      CALL PDA_DBTPCF(X,NX,FCN,LDF,NY,TX,KX,WORK,WORK(IW),IFLAG)
      CALL PDA_DBTPCF(Y,NY,WORK,NY,NX,TY,KY,BCOEF,WORK(IW),IFLAG)

*****************************
*   Starlink modification   *
*                           *
*   Ensures STATUS okay     *
*    when routine ends      *
*                           *
*****************************
      IF (IFLAG.NE.1) STATUS=1
      GO TO 9999

C
C  -----
C  EXITS
C  -----
C
  920 CONTINUE
      IFLAG = 2
      STATUS=1
      GO TO 9999
C
  930 CONTINUE
      IFLAG = 3
      STATUS=1
      GO TO 9999
C
  940 CONTINUE
      IFLAG = 4
      STATUS=1
      GO TO 9999
C
  950 CONTINUE
      IFLAG = 5
      STATUS=1
      GO TO 9999
C
  960 CONTINUE
      IFLAG = 6
      STATUS=1
      GO TO 9999
C
  970 CONTINUE
      IFLAG = 7
      STATUS=1
      GO TO 9999
C
  980 CONTINUE
      IFLAG = 8
      STATUS=1
      GO TO 9999
C
  990 CONTINUE
      IFLAG = 9
      STATUS=1
      GO TO 9999
C
 1000 CONTINUE
      IFLAG = 10
      STATUS=1
      GO TO 9999
C
 9999 CONTINUE

      END
