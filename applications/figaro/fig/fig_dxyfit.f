C+
      SUBROUTINE FIG_DXYFIT (X,Y,NPTS,MAXDEG,IW,W,WORK1,COEFFS,DEGREE)
C
C     F I G _ D X Y F I T
C
C     Returns the best polynomial fit to a set of data points.
C
C     Parameters -  (">" input, "<" output, "!" modified, "W" workspace)
C
C     (>) X      (Double precision array X(NPTS)) The X values for the
C                points to be fitted.
C     (>) Y      (Double precision array Y(NPTS)) The Y values for the
C                points to be fitted.
C                X and Y are returned sorted into increasing X order.
C     (>) NPTS   (Integer) The number of points to be fitted.
C     (>) MAXDEG (Integer) The maximum degree to be attempted.  Should
C                not be more than 10.
C     (W) IW     (Integer array IW(*)) Unused.
C     (W) W      (Double precision array W(NPTS)) Workspace.
C     (W) WORK1  (Double precision array WORK1(4*NPTS+3*(MAXDEG+1))) Workspace.
C     (<) COEFFS (Double precision array COEFFS(MAXDEG+1)) The coefficients
C                for the fitted polynomial.  The constant term is COEFFS(1).
C     (<) DEGREE (Integer) The degree for the fit.  If a good fit can be
C                obtained at a lower degree than MAXDEG, this fit will be
C                used and the higher order coefficients will be set to 0.
C
C     Common variables used - None
C
C                                                KS / CIT 3rd Feb 1984
C     Modified:
C
C      2nd Apr 1985  KS / AAO.  Modified to use NAG routines.  Calling
C                    sequence modified.  Note that X and Y may now be
C                    modified.
C      2nd Feb 1994  HME / UoE, Starlink.  In an attempt to discard
C                    GEN_QDISORT, use GEN_QFISORT on a single
C                    precision copy of X. This copy is generated in
C                    the workpace WORK1. In order that it can be used
C                    as a single precision work space, WORK1 is
C                    changed from DOUBLE(3*NPTS) to REAL(6*NPTS). The
C                    traditional use of WORK1 is not affected by the
C                    changed declaration, since it was used only as
C                    argument in subroutine calls.
C     18th Apr 1995  HME / UoE, Starlink.  No longer use NAG. Then
C                    sorting is not necessary. But the workspace must be
C                    bigger and measured by MAXDEG as well as NPTS.
C
C     Note: The original version of this routine attempted to see what
C     degree of fit was optimum (as implied by the comments for DEGREE
C     above).  This proved a little tricky when a large number of points
C     were involved, so the idea was shelved.  This routine simply fits
C     to degree MAXDEG and returns DEGREE=MAXDEG, unless there are too
C     few points for that degree.  Maybe someday..
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NPTS, MAXDEG, DEGREE, IW(1)
      DOUBLE PRECISION X(NPTS), Y(NPTS), W(NPTS)
      DOUBLE PRECISION WORK1(4*NPTS+3*(MAXDEG+1))
      DOUBLE PRECISION COEFFS(MAXDEG+1)
C
C     Maximum degree of fit allowed.
C
      INTEGER MAXFIT
      PARAMETER (MAXFIT=10)
C
C     Local variables
C
      INTEGER I, IFAIL, IFAIL2, MD, NDEG
      DOUBLE PRECISION EPS
C
C     Set coeffs array to dummy values
C
      DO I=1,MAXDEG+1
         COEFFS(I)=0.0D0
      END DO
      COEFFS(2)=1.0D0
C
C     Set weight array.
C
      DO I=1,NPTS
         W(I)=1.0D0
      END DO
C
C     Perform fit to orthogonal polynomials
C
      MD=MIN(MAXDEG,NPTS-1,MAXFIT)
      IFAIL2=0
      EPS=0D0
      CALL PDA_DPOLFT(NPTS,X,Y,W,MD,NDEG,EPS,WORK1,
     :   IFAIL,WORK1(NPTS+1),IFAIL2)
      IF (NDEG.NE.MD .OR. IFAIL.NE.1 .OR. IFAIL2.NE.0) THEN
         CALL PAR_WRUSER('Error in PDA_DPOLFT',I)
         GO TO 500
      END IF
C
      DEGREE=MD
C
C     Decode the orthogonal polynomials
C
      IFAIL2=0
      CALL PDA_DPCOEF(DEGREE,0D0,COEFFS,WORK1(NPTS+1),IFAIL2)
      IF (IFAIL2.NE.0) THEN
         CALL PAR_WRUSER('Error in PDA_DPCOEF',I)
         GO TO 500
      END IF
C
C     Bail out on NAG error comes to here
C
  500 CONTINUE
C
      END
