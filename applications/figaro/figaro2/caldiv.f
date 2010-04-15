C+
      SUBROUTINE CALDIV
C
C     C A L D I V
C
C     Divides the interpolated continuum spectrum of a standard
C     star (eg a Filippenko/Greenstein standard, probably created
C     by GSPIKE and INTERP) by the observed continuum of the same
C     star, in order to generate the instrumental response calibration
C     spectrum.
C
C     Command parameters -
C
C     STANDARD    (Character) The interpolated continuum spectrum of
C                 the standard star.  Note: This should not be in
C                 magnitude units, and should probably not contain a
C                 .TABLE.BANDWIDTH data object, since this would
C                 indicate that this was generated from published
C                 data giving average observed flux density over a
C                 wavelength range (eg Oke/Gunn data) rather than a
C                 fitted continuum.
C
C     SPECTRUM    (Character) The observed continuum spectrum of the
C                 standard star.  Note that both STANDARD and SPECTRUM
C                 should be on the same wavelength scale (given by
C                 a .X.DATA array) and ideally this will be a linear
C                 scale.
C
C     OUTPUT      (Character) The resulting calibration spectrum.
C
C     Command keywords -  None
C
C     User variables used -  None
C
C                                        KS / CIT 28th May 1984
C     Modified:
C
C     24th Aug 1987  DJA/AAO. Revised DSA_ routines - some specs
C                    changed. Now uses DYN_ routines for dynamic-memory
C                    handling.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected
C                    in mapping calls.
C     5th  Oct 1992  HME / UoE, Starlink. Changed INCLUDE. TABs
C                    removed.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL FIG_SCRCHK, FIG_WCHECK
      INTEGER ICH_ENCODE
C
C     Local variables
C
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      CHARACTER    DUMMY*1       ! Dummy string arguement
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      INVOKE        ! Used to invoke functions
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NEXT          ! Dummy arguement for ICH_ENCODE
      INTEGER      NX            ! Number of elements in data
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      SPDPTR        ! Pointer to spectrum data
      INTEGER      SPDSLOT       ! Map slot number for spectrum data
      INTEGER      SPXPTR        ! Pointer to spectrum x-axis data
      INTEGER      SPXSLOT       ! Map slot number for spectrum x-axis
                                 ! data
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    STRING*64     ! Output message text
      INTEGER      STXPTR        ! Pointer to standard x-axis data
      INTEGER      STXSLOT       ! Map slot number for standard x-axis
                                 ! data
      REAL         TIME          ! The exposure time
C
      DOUBLE PRECISION  MAGNITUDE ! Non zero if data is in magnitudes
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of STANDARD and open it
C
      CALL DSA_INPUT('STAND','STANDARD',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check for magnitude data and reject it
C
      CALL DSA_GET_DATA_INFO('STAND',0,DUMMY,1,MAGNITUDE,STATUS)
      IF ((STATUS.EQ.0).AND.(MAGNITUDE.NE.0)) THEN
         CALL PAR_WRUSER(
     :         'Cannot use a standard whose data is in magnitudes',
     :                                                      IGNORE)
         CALL PAR_WRUSER(
     :         'You must calibrate in Janskys or suchlike and then'
     :         //' convert using ABCONV',IGNORE)
         GO TO 500
      END IF
C
C     Get name of SPECTRUM and open it
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get the sizes of the data
C
      CALL DSA_DATA_SIZE('STAND',1,NDIM,DIMS,NX,STATUS)
      CALL DSA_MATCH_AXES('STAND','SPECT',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the X data arrays
C
      CALL DSA_MAP_AXIS_DATA('STAND',1,'READ','FLOAT',STXPTR,STXSLOT,
     :                       STATUS)
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',SPXPTR,SPXSLOT,
     :                       STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check that the data arrays match
C
      IF (.NOT.FIG_WCHECK(NX,%VAL(CNF_PVAL(SPXPTR)),
     :                       %VAL(CNF_PVAL(STXPTR)))) THEN
         CALL PAR_WRUSER(
     :     'Spectrum and standard have different wavelength scales',
     :                                                       IGNORE)
         GO TO 500
      END IF
      IF (.NOT.FIG_SCRCHK(NX,%VAL(CNF_PVAL(SPXPTR)))) THEN
         CALL PAR_WRUSER(
     :      'Warning: Data is not on a linear wavelength scale',IGNORE)
         CALL PAR_WRUSER(
     :      'This is allowed, but can cause confusion later.',IGNORE)
      END IF
C
C     Get name of OUTPUT
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','STAND',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the data arrays
C
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',SPDPTR,SPDSLOT,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get the exposure time for the spectrum.
C
      CALL DSA_GET_EXPOSURE('SPECT',1.0,0.0,TIME,STATUS)
      STRING='Spectrum observation time given as '
      INVOKE=ICH_ENCODE(STRING,TIME,36,2,NEXT)
      STRING(NEXT:)=' secs.'
      CALL PAR_WRUSER(' ',IGNORE)
      CALL PAR_WRUSER(STRING(:NEXT+5),IGNORE)
C
C     Calculate the calibration spectrum
C
      CALL FIG_CALDIV(NX,%VAL(CNF_PVAL(SPXPTR)),%VAL(CNF_PVAL(SPDPTR)),
     :                %VAL(CNF_PVAL(OPTR)),TIME,%VAL(CNF_PVAL(OPTR)))
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_CALDIV (NX,XDATA,SDATA,ZDATA,TIME,OUTPUT)
C
C     F I G _ C A L D I V
C
C     Calculates the instrumental response of a detector, given
C     an observed continuum standard spectrum - ie a continuum
C     fitted to an observed standard - and a spectrum that gives
C     the actual flux density over the same wavelength range for
C     that standard star.  The result is a spectrum where all the
C     elements contain the response in 'units per (count per second
C     per angstrom)'.  'Units' is just the units used for the actual
C     flux density spectrum, and it doesn't matter what they are, so
C     long as they are not magnitudes.
C
C     Parameters -  (">" input, "!" modified)
C
C     (>) NX      (Integer) Number of elements in the spectra
C     (>) XDATA   (Real array XDATA(NX)) The X data - ie the wavelengths
C                 corresponding to the centers of each of the elements
C                 of the other data arrays.
C     (>) SDATA   (Real array SDATA(NX)) The observed spectrum data
C     (>) ZDATA   (Real array ZDATA(NX)) The actual flux density spectrum
C     (>) TIME    (Real) The observation time for the standard in seconds
C     (<) OUTPUT  (Real array OUTPUT(NX)) The resulting calibration
C                 spectrum.
C
C                 Note: OUTPUT may be the same array as SDATA or ZDATA.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     FIG_WAVEST    (FIG_ package) Get wavelength of left edge of pixel
C
C                                               KS / CIT 28th MAy 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX
      REAL    XDATA(NX), SDATA(NX), ZDATA(NX), TIME, OUTPUT(NX)
C
C     Functions / subroutines
C
      REAL FIG_WAVEST
C
C     Local variables
C
      INTEGER IX
      REAL    WAVEL, WAVER
C
      WAVEL=FIG_WAVEST(1,NX,XDATA)
      DO IX=1,NX
         WAVER=FIG_WAVEST(IX+1,NX,XDATA)
         IF (SDATA(IX).LT.1.0E-9) THEN
            OUTPUT(IX)=0.
         ELSE
            OUTPUT(IX)=ZDATA(IX)*TIME*ABS(WAVER-WAVEL)/SDATA(IX)
         END IF
         WAVEL=WAVER
      END DO
C
      END
