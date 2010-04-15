C
       SUBROUTINE LWRCOR (NPTS,YR,IIAPER,WAVE,FLUX,OK)
C
C    Corrects IUE low dispersion LWR fluxes for degradation of the
C    LWR camera sensitivity using the sensitivity loss curve appearing
C    in Table 1 of J. Clavel, R. Gilmozzi & A. Prieto, IUE ESA Newsletter
C    26:65 (1986).
C
C    Fluxes are corrected to epoch 1978.90
C    Correction is not valid for LWR spectra taken after epoch 1983.75
C    because the rate of camera degradation increased after LWR ceased
C    to be used routinely.
C
C    No sensitivity loss values are available above 3275 A, so the longest
C    wavelength fluxes are not corrected.
C
       IMPLICIT NONE
       INTEGER NPTS
       REAL YR
       INTEGER IIAPER

       REAL WAVE(*)
       REAL FLUX(*)

       LOGICAL OK
       CHARACTER*1 BLEEP
       COMMON /BLEEP/ BLEEP

       INTEGER I
       REAL SENLOS(58)
       INTEGER IND
       REAL FWAVE
       REAL SENLWAVE
       REAL YREP
       REAL YRFAC

*  Local Data:
       DATA (SENLOS(I),I=1,58)/3.79, 2.15, 1.79, 1.51, 1.56, 1.73, 1.62,
     :       1.51, 1.86, 2.17, 1.75, 1.41, 1.48, 2.02, 2.27, 2.24, 2.40,
     :       2.67, 2.85, 2.90, 2.88, 2.61, 1.68, 1.71, 1.51, 0.72, 0.98,
     :       1.40, 1.04, 0.88, 0.94, 0.82, 0.65, 1.14, 1.13, 0.74, 1.18,
     :       1.71, 1.60, 1.22, 1.42, 1.22, 0.98, 0.88, 0.94, 0.84, 0.83,
     :       1.32, 1.16, 0.59, 1.03, 1.22, 0.56, 1.29, 2.81, 1.32, 1.02,
     :       3.01/

C    Make sure wavelength ranges are consistent

       IF (NPTS.GT.0) THEN
          IF (WAVE(1).LT.1850.) THEN
             WRITE (*,
     :       '(''   IUECOR:  camera 2 (LWR) data extend below '',
     :       ''1850A'',A)') BLEEP
             OK = .FALSE.
          ENDIF
          IF (WAVE(NPTS).GT.3350.) THEN
             WRITE (*,
     :       '(''   IUECOR:  camera 2 (LWR) data extend above '',
     :       ''3350A'',A)') BLEEP
             OK = .FALSE.
          ENDIF
       ELSE
          WRITE (*,'(''   IUECOR:  no data'')')
          OK = .FALSE.
       ENDIF
       IF (.NOT.OK) GOTO 999

C    Make sure dates are consistent

       IF (YR.GT.1983.75) THEN
          WRITE (*,'(''   IUECOR:  correction undefined for camera '',
     :    ''2 (LWR) after 1983.75''/
     :    ''   Data corrected to that date'',A)') BLEEP
          YR = 1983.75
       ENDIF

       YREP = YR - 78.90
       DO 100 I = 1, NPTS
          IF (WAVE(I).LE.3275.0) THEN
             IND = INT((WAVE(I)-1850.0)/25.0) + 1
             FWAVE = WAVE(I)/25. - AINT(WAVE(I)/25.)
             SENLWAVE = SENLOS(IND) + (SENLOS(IND+1)-SENLOS(IND))*FWAVE
             YRFAC = (100.0-SENLWAVE)/100.0
             FLUX(I) = FLUX(I)*YRFAC**YREP
          ENDIF
  100  CONTINUE
  999  CONTINUE
       END
