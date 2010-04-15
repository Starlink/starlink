C+
      SUBROUTINE ARC_ARIFIT(CHANS,WAVES,WEIGHTS,NLID,COEFFS,NCOEFF,RMS)
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
C     (>) NCOEFF   (Integer) The number of parameters used for the
C                  fit.
C     (<) COEFFS   (Double precision array COEFFS(NC)) The
C                  coefficients of the final fit.
C     (<) RMS      (Real) The RMS error from the fit, in angstroms.
C
C                                      KS / CIT 20th Jan 1983
C     Modified:
C
C     5th Sept 1985.  KS / AAO WEIGHTS parameter added, WXYFIT used
C                     instead of XYFIT.
C     30th June 1986. KS / AAO No longer modifies NCOEFF if not enough
C                     lines have been selected.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NLID,NCOEFF
      REAL CHANS(NLID),WAVES(NLID),WEIGHTS(NLID),RMS
      DOUBLE PRECISION COEFFS(NCOEFF)
C
C     Functions
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      INTEGER I,NEWNCF,STATUS
      REAL ERR
      CHARACTER CHARS*64
C
C     Calculate coefficients
C
      IF (NLID.LT.NCOEFF) THEN
         WRITE(CHARS,'(A,I3,A)') 'Not enough points for',
     :                             NCOEFF-1,'th order fit.'
         IF (NCOEFF.EQ.2) CHARS(26:27)='st'
         IF (NCOEFF.EQ.3) CHARS(26:27)='nd'
         IF (NCOEFF.EQ.4) CHARS(26:27)='rd'
         CALL PAR_WRUSER(CHARS,STATUS)
         NEWNCF=NLID
      ELSE
         NEWNCF=NCOEFF
      END IF
      CALL FIG_WXYFIT(CHANS,WAVES,WEIGHTS,NLID,COEFFS,NEWNCF-1)
C
C     Calculate rms
C
      RMS=0.
      DO I=1,NLID
         ERR=GEN_EPOLYD(DBLE(CHANS(I)),COEFFS,NEWNCF)-WAVES(I)
         RMS=RMS+ERR*ERR
      END DO
      RMS=SQRT(RMS/FLOAT(NLID))
C
      RETURN
      END
