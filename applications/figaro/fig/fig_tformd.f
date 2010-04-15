C+
      SUBROUTINE FIG_TFORMD(RX,WAVES,WAVESR,IX,X)
C
C     F I G _ T F O R M D
C
C     Rebin utility routine.  Given an x-value (RX) in the
C     rebinned data, calculates the corresponding wavelength
C     and returns (X) the corresponding x-value in the original
C     data.
C
C     History :               This performs the same function
C     as the routine REBIN_TFORM written by M. Ashley for
C     the Lolita system, but uses tables of wavelength values
C     instead of wavelength coefficients.  This makes the routine
C     quite different, and perhaps a little cruder.  This routine
C     This is a double precision version of the routine FIG_TFORM,
C     intended for use by FIG_REBIND.
C
C     Parameters -    (">" input, "!" modified, "<" output)
C
C     (>) RX     (Double precision) The x-value in the rebinned data.
C                This is a bin number.
C     (>) WAVES  (Double precision  array) The wavelength values for
C                the centers of the bins of the original data.
C     (>) WAVESR (Double precision array) The wavelength values for
C                the centers of the bins of the rebinned data.
C     (!) IX     (Integer) Passed as an initial bin number at which to
C                start searching for X, so must be a valid bin number.
C                Returned as the number of one of the bins used in
C                the interpolation for X, and so probably a good
C                starting place for the next call.
C     (<) X      (Double precision) The x-value in the original data
C                corresponding to the wavelength at bin RX in the
C                rebinned data.  This is also a bin number.
C
C     Common variables used -
C
C     (>) UP     (Logical) True if the wavelength values in WAVES
C                increase with bin number.
C     (>) LOGS   (Logical) True if the values in WAVES increase
C                logarithmically.  False otherwise.
C     (>) LOGSR  (Logical) True if the values in WAVESR increase
C                logarithmically.  False otherwise.
C     (>) NBIN   (Integer) The number of bins in the original array
C                - ie the dimension of WAVES
C     (>) NBINR  (Integer) The number of bins in the rebinned array
C                - ie the dimension of WAVESR
C
C     All common variables in
C
C     COMMON /REBIN_INFO/ UP,LOGS,LOGSR,NBIN,NBINR
C
C     Method -
C
C     The wavelength corresponding to RX is found by linear
C     interpolation between the closest array elements in WAVESR.
C     A search through WAVES, starting from IX, finds the nearest
C     two values to that wavelength and X is then calculated by
C     linear interpolation.  If the values are logarithmic, the
C     interpolation is still linear, but the logs of the array values
C     are used.  This is cruder than the Newton Raphson solution used
C     by Michael Ashley's version, but does not require that the
C     functional form of the wavelength values be known.
C
C                                    KS / AAO 13th Sept 1985
C
C     26th Jul 1994.   Make common blocks SAVE. HME / UoE, Starlink.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER IX
      DOUBLE PRECISION RX,WAVES(*),WAVESR(*),X
C
C     Common with main rebin routine
C
      LOGICAL UP,LOGS,LOGSR
      INTEGER NBIN,NBINR
C
      COMMON /REBIN_INFO/ UP,LOGS,LOGSR,NBIN,NBINR
      SAVE /REBIN_INFO/
C
C     Local variables
C
      INTEGER IRX,IRX2,IX2
      DOUBLE PRECISION LAM1,LAMBDA
C
C     Find the wavelength (LAMBDA) corresponding to RX
C
      IRX=RX
      IF (IRX.LT.1) THEN
         IRX=1
         IRX2=2
      ELSE IF (IRX.GE.NBINR) THEN
         IRX=NBINR-1
         IRX2=NBINR
      ELSE
         IRX2=IRX+1
      END IF
      IF (.NOT.LOGSR) THEN
         LAMBDA=WAVESR(IRX)+
     :          (RX-FLOAT(IRX))*(WAVESR(IRX2)-WAVESR(IRX))
      ELSE
         LAMBDA=EXP(LOG(WAVESR(IRX))+
     :           (RX-FLOAT(IRX))*(LOG(WAVESR(IRX2))-LOG(WAVESR(IRX))))
      END IF
C
C     Now find the two elements in WAVES1 that straddle LAMBDA - or
C     the end values, if we run off one end.  The following code is
C     long-winded rather than clever, sacrificing compactness for
C     simplicity and execution speed.
C
      LAM1=WAVES(IX)
      IF (UP) THEN
C
C        Values increase with bin #
C
         IF (LAM1.GT.LAMBDA) THEN
C
C           Initial guess is too high
C
            DO WHILE ((IX.GT.1).AND.(LAM1.GT.LAMBDA))
               IX=IX-1
               LAM1=WAVES(IX)
            END DO
            IX2=IX+1
         ELSE
C
C           Initial guess is too low
C
            DO WHILE ((IX.LT.NBIN).AND.(LAM1.LT.LAMBDA))
               IX=IX+1
               LAM1=WAVES(IX)
            END DO
            IX2=IX-1
         END IF
      ELSE
C
C        Wavelength values decrease with bin #.
C
         IF (LAM1.GT.LAMBDA) THEN
C
C           Initial guess is too high
C
            DO WHILE ((IX.LT.NBIN).AND.(LAM1.GT.LAMBDA))
               IX=IX+1
               LAM1=WAVES(IX)
            END DO
            IX2=IX-1
         ELSE
C
C           Initial guess is too low
C
            DO WHILE ((IX.GT.1).AND.(LAM1.LT.LAMBDA))
               IX=IX-1
               LAM1=WAVES(IX)
            END DO
            IX2=IX+1
         END IF
      END IF
C
C     Check for having run off the end
C
      IF (IX.EQ.1) THEN
         IX2=2
      ELSE IF (IX.EQ.NBIN) THEN
         IX2=NBIN-1
      END IF
C
C     Interpolate to find X
C
      IF (.NOT.LOGS) THEN
         X=(LAMBDA-LAM1)/(WAVES(IX2)-LAM1)*FLOAT(IX2-IX)+FLOAT(IX)
      ELSE
         X=(LOG(LAMBDA)-LOG(LAM1))/(LOG(WAVES(IX2))-LOG(LAM1))
     :                                    *FLOAT(IX2-IX)+FLOAT(IX)
      END IF
C
      END
