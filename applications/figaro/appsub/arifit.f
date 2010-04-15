C+
      SUBROUTINE ARIFIT(CHANS,WAVES,WEIGHTS,NLID,COEFFS,ORDER,RMS)
C
C     A R I F I T
C
C     Performs an 'interim' arc fit.  This is just the fit and
C     calculation of the RMS error, as opposed to the full
C     listing of results provided by subroutine ARFIT.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) CHANS    (Real array CHANS(NLID)) The centers of the
C                  identified lines, in pixel numbers.
C     (>) WAVES    (Real array WAVES(NLID)) The wavelengths of the
C                  identified lines.
C     (>) WEIGHTS  (Real array WEIGHTS(NLMAX)) The weights for the
C                  identified arc lines.
C     (>) NLID     (Integer) The number of identified lines.
C     (>) ORDER    (Integer) The number of parameters used for the
C                  fit. (Note: this is the usual meaning of 'order'
C                  plus 1)
C     (<) COEFFS   (Double precision array COEFFS(NC)) The
C                  coefficients of the final fit.
C     (<) RMS      (Real) The RMS error from the fit, in the wavelength
C                  units being used.
C
C                                      KS / CIT 20th Jan 1983
C     Modified:
C
C     5th Sept 1985.  KS / AAO WEIGHTS parameter added, WXYFIT used
C                     instead of XYFIT.
C     30th June 1986. KS / AAO No longer modifies ORDER if not enough
C                     lines have been selected.
C     11th March 1988 KS / AAO.  Now uses GKD_ routine for output.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NLID,ORDER
      REAL CHANS(NLID),WAVES(NLID),WEIGHTS(NLID),RMS
      DOUBLE PRECISION COEFFS(ORDER)
C
C     Functions
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      INTEGER I,NORDER
      REAL ERR
      CHARACTER CHARS*64
C
C     Calculate coefficients
C
      IF (NLID.LT.ORDER) THEN
         WRITE(CHARS,'(A,I3,A)') 'Not enough points for',
     :                             ORDER-1,'th order fit.'
         IF (ORDER.EQ.3) CHARS(26:27)='nd'
         CALL GKD_WRITE_LINE(CHARS)
         NORDER=NLID
      ELSE
         NORDER=ORDER
      END IF
      CALL FIG_WXYFIT(CHANS,WAVES,WEIGHTS,NLID,COEFFS,NORDER-1)
C
C     Calculate rms
C
      RMS=0.
      DO I=1,NLID
         ERR=GEN_EPOLYD(DBLE(CHANS(I)),COEFFS,NORDER)-WAVES(I)
         RMS=RMS+ERR*ERR
      END DO
      RMS=SQRT(RMS/FLOAT(NLID))
C
      END
