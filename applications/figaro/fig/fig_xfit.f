C+
      SUBROUTINE FIG_XFIT (ARRAY,POINTS,MAXDEG,COEFFS,DEGREE)
C
C     F I G _ X F I T
C
C     Figaro utility routine.  Given an array whose values can
C     be expected to be well fit by a polynomial - such as the
C     X data array for a Figaro data structure - this routine
C     performs a fit to a number of evenly spaced points taken
C     from the data and returns the polynomial coefficients and
C     the degree giving the best fit.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) ARRAY    (Real array ARRAY(POINTS)) The data array.
C     (>) POINTS   (Integer) The number of points in ARRAY.
C     (>) MAXDEG   (Integer) The maximum order for the fit -
C                  should not be greater than 10.
C     (<) COEFFS   (Real array COEFFS(MAXDEG+1))
C                  The coefficients for the fitted polynomial.
C                  The constant term is returned in COEFFS(1).
C     (<) DEGREE   (Integer) The degree for the fit.  If a good
C                  fit can be obtained at a lower degree than
C                  MAXDEG, this fit will be used and the higher
C                  order coefficients will be set to zero.
C
C     Common variables used -  None
C
C     Subroutines used -
C
C     FIG_NAGERR   Returns a NAG error message to the user.
C     GEN_CHB2NO   Converts Chebyshev coefficients to power series.
C     E02ADF       (NAG routine) Fits data points with Chebyshev poly.
C
C                                        KS / CIT 5th May 1983
C
C     Modified:
C
C     1st Apr 1985.  KS / AAO.  Modified to use NAG routines.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER POINTS,MAXDEG,DEGREE
      REAL ARRAY(POINTS),COEFFS(MAXDEG+1)
C
C     Maximum number of selected points, maximum degree of
C     fit allowed.
C
      INTEGER FITPTS, MAXFIT
      PARAMETER (FITPTS=30, MAXFIT=10)
C
C     Local variables
C
      INTEGER IAPTR, ID, IEC, IFAIL, IP, MD, NPFIT
      REAL APTR, STAT, STEP, STWAS
      DOUBLE PRECISION A(MAXFIT+1,MAXFIT+1), AK(MAXFIT+1), C(MAXFIT+1),
     :       S(0:MAXFIT), W(FITPTS), WORK1(3*FITPTS), WORK2(2*MAXFIT+2),
     :       X(FITPTS), Y(FITPTS)
C
C     Set a dummy result in case of error
C
      DEGREE=1
      DO ID=1,MAXDEG+1
         COEFFS(ID)=0.
      END DO
      COEFFS(2)=1.
C
C     Select points from the data.
C
      IF (POINTS.LE.FITPTS) THEN
         NPFIT=POINTS
         STEP=1.
      ELSE
         STEP=FLOAT(POINTS)/FLOAT(FITPTS)
         NPFIT=FITPTS
      END IF
      APTR=1.
      DO IP=1,NPFIT
         IAPTR=MIN(POINTS,INT(APTR))
         Y(IP)=ARRAY(IAPTR)
         X(IP)=DBLE(IAPTR)
         W(IP)=1.0D0
         APTR=APTR+STEP
      END DO
C
C     Perform fit to orthogonal polynomials
C
      MD=MIN(MAXDEG,NPFIT-1,MAXFIT)
      IFAIL=1
      CALL E02ADF(NPFIT,MD+1,MAXFIT+1,X,Y,W,WORK1,WORK2,A,S,IFAIL)
      IF (IFAIL.NE.0) THEN
         CALL FIG_NAGERR(IFAIL,'E02ADF')
         GO TO 400
      END IF
C
C     See which is the best degree to use (look for an increase in
C     the statistic (sum of squares)/(points-degree of fit)).  Note
C     S is declared S(0:MAXFIT).
C
      DO ID=1,MD
         IF (S(ID).GT.S(ID-1)) THEN
            DEGREE=ID-1
            GO TO 320
         END IF
      END DO
      DEGREE=MD
  320 CONTINUE
C
C     Decode the orthogonal polynomials 
C
      DO ID=1,DEGREE+1
         AK(ID)=A(DEGREE+1,ID)
      END DO
      CALL GEN_CHB2NO(DEGREE,X(1),X(NPFIT),AK,C)
C
C     Return the coefficients as single precision.
C
      DO IP=1,DEGREE+1
         COEFFS(IP)=C(IP)
      END DO
C
C     NAG error bail out is to here
C
  400 CONTINUE
C
      END
