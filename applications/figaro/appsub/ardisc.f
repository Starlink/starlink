C+
      SUBROUTINE ARDISC(CHANS,WAVES,WEIGHTS,CLASS,NLID,COEFFS,
     :                                      ORDER,NX,WARRAY,DARRAY)
C
C     A R D I S C
C
C     Displays the dispersion curve for the current fit.  This is a plot
C     of difference between current fit and a linear fit, against wavelength.
C
C     Parameters  (">" input, (W) workspace)
C
C     (>) CHANS    (Real array CHANS(NLID)) The channel numbers for
C                  the lines.
C     (>) WAVES    (Real array WAVES(NLID)) The wavelengths of the
C                  lines.
C     (>) WEIGHTS  (Real array WEIGHTS(NLID)) The weights for the
C                  identified arc lines.
C     (>) CLASS    (Integer array CLASS(NLID)) The class codes for
C                  the identified arc lines.
C     (>) NLID     (Integer) The number of the lines identified.  This
C                  must be >2.
C     (>) COEFFS   (Double precision COEFFS(ORDER)) The current
C                  wavelength coefficients.
C     (>) ORDER    (Integer) The number of coefficients used.
C     (>) NX       (Integer) Number of pixels in the arc.
C     (W) WARRAY   (Real array WARRAY(NX)) Used to hold the wavelengths
C                  for each pixel.
C     (W) DARRAY   (Real array DARRAY(NX)) Used to hold the fit-linear
C                  values for each pixel.
C
C                                             KS / AAO 5th Sept 1985
C     Modified:
C
C     30th June 1986.  KS / AAO. Now allows for possibility ORDER>NLID
C     11th March 1988  KS / AAO. GRPCKG calls replaced.
C     31st Aug   1992  HME / UoE, Starlink. Changed declaration to not
C                      contain MAX/MIN functions.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NLID,ORDER,NX,CLASS(*)
      REAL    CHANS(*),WAVES(*)
      REAL    WEIGHTS(*),WARRAY(NX),DARRAY(NX)
      DOUBLE PRECISION COEFFS(*)
C
C     Functions
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      INTEGER I,NORDER
      REAL    DMAX, DMIN, DVALUE, X(2), Y(2)
      DOUBLE PRECISION LCOEFF(2)
C
C     PGPLOT colour values
C
      INTEGER GREEN, RED, WHITE, BLUE
      PARAMETER (WHITE=1, RED=2, GREEN=3, BLUE=4)
C
C     Perform the linear fit
C
      CALL FIG_WXYFIT(CHANS,WAVES,WEIGHTS,NLID,LCOEFF,1)
C
C     Work out the values for the curve.
C
      NORDER=MIN(ORDER,NLID)
      DMIN=0.0
      DMAX=DMIN
      DO I=1,NX
         WARRAY(I)=GEN_EPOLYD(DBLE(I),COEFFS,NORDER)
         DVALUE=GEN_EPOLYD(DBLE(I),LCOEFF,2)-WARRAY(I)
         IF (DVALUE.GT.DMAX) DMAX=DVALUE
         IF (DVALUE.LT.DMIN) DMIN=DVALUE
         DARRAY(I)=DVALUE
      END DO
C
C     Set Y range for plot so points will fit as well, and allow ~10%
C     headroom, and for case where everything lies on a straight line.
C
      DO I=1,NLID
         DVALUE=WAVES(I)-GEN_EPOLYD(DBLE(CHANS(I)),LCOEFF,2)
         IF (DVALUE.GT.DMAX) DMAX=DVALUE
         IF (DVALUE.LT.DMIN) DMIN=DVALUE
      END DO
      DMAX=DMAX+(DMAX-DMIN)*0.1
      DMIN=DMIN-(DMAX-DMIN)*0.1
      IF (DMIN.EQ.DMAX) THEN
         DMIN=-0.5
         DMAX=0.5
      END IF
C
C     Draw the plot of the dispersion curve
C
      CALL PGADVANCE
      CALL PGSCI(WHITE)
      CALL PGENV(WARRAY(1),WARRAY(NX),DMIN,DMAX,0,1)
      CALL PGLABEL('Wavelength','Deviation, in A',
     :                  'Deviation of fit from a linear fit')
      CALL PGSCI(GREEN)
      CALL PGLINE(NX,WARRAY,DARRAY)
C
C     Now add the various fitted lines, as error bars
C
      CALL PGSCI(RED)
      DO I=1,NLID
         DVALUE=GEN_EPOLYD(DBLE(CHANS(I)),LCOEFF,2)
         Y(1)=DVALUE-WAVES(I)
         Y(2)=DVALUE-GEN_EPOLYD(DBLE(CHANS(I)),COEFFS,NORDER)
         X(1)=WAVES(I)
         X(2)=WAVES(I)
         IF (CLASS(I).NE.0) CALL PGSCI(BLUE)
         CALL PGLINE(2,X,Y)
         IF (CLASS(I).NE.0) CALL PGSCI(RED)
      END DO
      CALL PGSCI(WHITE)
C
      END
