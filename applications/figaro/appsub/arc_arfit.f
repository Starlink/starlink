C+
      SUBROUTINE ARC_ARFIT(CHANS,WAVES,WEIGHTS,CLASS,NLID,NX,NC,DRMS,
     :                                                 NCOEFF,COEFFS)
C
C     A R F I T
C
C     Performs a fit to the identified lines and list the results
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) CHANS    (Real array CHANS(NLID)) The centers of the
C                  identified lines, in pixel numbers.
C     (>) WAVES    (Real array WAVES(NLID)) The wavelengths of the
C                  identified lines.
C     (>) WEIGHTS  (Real array WEIGHTS(NLMAX)) The weights for the
C                  identified arc lines.
C     (>) CLASS    (Integer array CLASS(NLMAX)) The class codes for
C                  the identified arc lines.
C     (>) NLID     (Integer) The number of identified lines.
C     (>) NX       (Integer) The number of pixels in the arc data.
C     (>) NC       (Integer) The maximum number of polynomial
C                  coefficients.
C     (>) DRMS     (Logical) True if the RMS if omitted statistic
C                  is to be output for each line.
C     (>) NCOEFF   (Integer) Passed as the initial number of parameters
C                  used for the fit, returned as the final number used.
C     (<) COEFFS   (Double precision array COEFFS(NC)) The
C                  coefficients of the final fit.
C
C                                            KS / CIT 14th June 1983
C     Modified:
C
C     KS / AAO  4th Sept 1985 Option to repeat with a different NCOEFF
C               removed.  Now incorporated in main routine.  WEIGHTS
C               parameter added, and 'fit without this line' figure
C               added. DRMS parameter added. CLASS parameter added.
C     KS / AAO  30th June 1986. No longer modifies NCOEFF if not enough
C               lines have been selected.
C     WFL / AAO 31st May 1988. Avoid crash if NLID is too small.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      LOGICAL DRMS
      INTEGER NLID,NX,NC,NCOEFF,CLASS(NLID)
      REAL CHANS(NLID),WAVES(NLID),WEIGHTS(NLID)
      DOUBLE PRECISION COEFFS(NC)
C
C     Functions
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      LOGICAL WARNING
      INTEGER I,IGNORE,NC1,NC3,NEWNCF,STATUS
      REAL ADISP,END,RMS,RMSX,ST,VALUE,VALUE1
      CHARACTER CHARS*72
C
C     Perform fit.
C
      DO I=1,NC
         COEFFS(I)=0.
      END DO
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
C     Display results
C
      CALL PAR_WRUSER(' ',STATUS)
      WRITE (CHARS,'(I3,A)') NEWNCF-1,'th order polynomial fit'
      IF (NEWNCF.EQ.2) CHARS(4:5)='st'
      IF (NEWNCF.EQ.3) CHARS(4:5)='nd'
      IF (NEWNCF.EQ.4) CHARS(4:5)='rd'
      CALL PAR_WRUSER(CHARS,STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('Coefficients of fit are -',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
C
C     Print coefficents
C
      NC3=0
      DO WHILE (NC3.LT.NEWNCF)
         NC1=NC3+1
         NC3=MIN(NC3+3,NEWNCF)
         WRITE(CHARS,'(3(1PE13.5))',ERR=320)(COEFFS(I),I=NC1,NC3)
  320    CALL PAR_WRUSER(CHARS,STATUS)
      END DO
      CALL PAR_WRUSER(' ',STATUS)
C
C     Evaluate mean dispersion
C
      VALUE=FLOAT(NX)
      ST=GEN_EPOLYD(1.D0,COEFFS,NEWNCF)
      END=GEN_EPOLYD(DBLE(VALUE),COEFFS,NEWNCF)
      ADISP=(END-ST)/VALUE
      WRITE(CHARS,'(A,F8.2,A)',ERR=330) 'Mean dispersion    =',ADISP,
     +                                      ' angstroms/channel'
  330 CALL PAR_WRUSER(CHARS,STATUS)
C
C     And also give start, end and central wavelengths
C
      VALUE=VALUE/2.
      VALUE=GEN_EPOLYD(DBLE(VALUE),COEFFS,NEWNCF)
      WRITE(CHARS,'(A,F8.2,A)',ERR=340) 'Start wavelength   =',
     :                                             ST,' angstroms'
  340 CALL PAR_WRUSER(CHARS,STATUS)
      WRITE(CHARS,'(A,F8.2,A)',ERR=350) 'End wavelength     =',
     :                                            END,' angstroms'
  350 CALL PAR_WRUSER(CHARS,STATUS)
      WRITE(CHARS,'(A,F8.2,A)',ERR=360) 'Central wavelength =',
     :                                           VALUE,' angstroms'
  360 CALL PAR_WRUSER(CHARS,STATUS)
C
C     Produce title for table
C
      CALL PAR_WRUSER(' ',STATUS)
      IF (DRMS) THEN
         CALL PAR_WRUSER(
     1     '           Line    Wavelength  Calculated'//
     2     ' Discrepancy    RMS if',STATUS)
         CALL PAR_WRUSER(
     1     '                               Wavelength'//
     2     '                omitted',STATUS)
      ELSE
         CALL PAR_WRUSER(
     1     '           Line    Wavelength  Calculated'//
     2     ' Discrepancy',STATUS)
         CALL PAR_WRUSER(
     1     '                               Wavelength',STATUS)
      END IF
      CALL PAR_WRUSER(' ',STATUS)
C
C     Now print table and work out RMS error on the way
C
      WARNING=.FALSE.
      RMS=0.
      ADISP=1./(ADISP*ADISP*.25)
      DO I=1,NLID
         VALUE=GEN_EPOLYD(DBLE(CHANS(I)),COEFFS,NEWNCF)
         VALUE1=VALUE-WAVES(I)
         RMS=RMS+VALUE1*VALUE1
         IF (DRMS) THEN
            IF (NLID.LE.2) THEN
               RMSX = 0.0
            ELSE
               CALL ARC_ARFITX(I,CHANS,WAVES,WEIGHTS,NLID,NEWNCF,RMSX)
            END IF
            WRITE(CHARS,'(I4,4F12.3,F10.3)',IOSTAT=IGNORE) I,CHANS(I),
     :                                      WAVES(I),VALUE,VALUE1,RMSX
         ELSE
            WRITE(CHARS,'(I4,4F12.3)',IOSTAT=IGNORE) I,CHANS(I),
     :                                      WAVES(I),VALUE,VALUE1
         END IF
         IF (I.GT.1) THEN
            IF (WAVES(I).LT.WAVES(I-1)) THEN
               WARNING=.TRUE.
               CHARS(1:1)='*'
            END IF
         END IF
         IF (CLASS(I).NE.0) CHARS(2:2)='+'
         CALL PAR_WRUSER(CHARS,STATUS)
      END DO
      IF (WARNING) THEN
         CALL PAR_WRUSER(' ',STATUS)
         CALL PAR_WRUSER('* WARNING - Wavelengths out of order',STATUS)
      END IF
C
C     And print out RMS error
C
      CALL PAR_WRUSER(' ',STATUS)
      RMS=SQRT(RMS/FLOAT(NLID))
      WRITE(CHARS,'(A,F10.3)') 'RMS error: ',RMS
      CALL PAR_WRUSER(CHARS,STATUS)
      CALL PAR_WRUSER(' ',STATUS)
C
      RETURN
      END
