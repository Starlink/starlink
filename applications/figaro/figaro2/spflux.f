C+
      SUBROUTINE SPFLUX
C
C     S P F L U X
C
C     Applies a flux calibration spectrum (typically generated
C     by the sequence CSPIKE, INTERP) to a spectrum, generating
C     a flux calibrated spectrum.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The spectrum to be calibrated.
C     CALSPECT    (Character) The calibration spectrum.
C     OUTPUT      (Character) The resulting calibrated spectrum.
C
C     Commnad keywords -  None
C
C     User variables used -  None
C
C                                     KS / CIT 16th May 1984
C     Modified:
C
C     22nd Feb 1985  KS / AAO.  Modified to handle 2D data as well
C                    as single spectra.
C     30th Apr 1985  KS / AAO.  Check for observation time given
C                    but invalid added.
C     26th Aug 1987  DJA/ AAO.  Revised DSA_ routines - some specs
C                    changed. Now uses DYN_ routines for dynamic-memory
C                    handling.
C     20th Dec 1990  JMS / AAO. Now handles 2D Calibration Spectrum
C                    Data.
C     23rd Sep 1992  HME / UoE, Starlink.  TABs removed. INCLUDE
C                    changed.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      LOGICAL FIG_SCRCHK
C
C     Local variables
C
      INTEGER      CDIM          ! Number of dimensions of Calibration
                                 ! Spectrum
      INTEGER      CPTR          ! Dynamic-memory pointer to calibration
                                 ! data
      INTEGER      CSLOT         ! Map slot number for calibration data
      INTEGER      CXPTR         ! Pointer to calibration x-axis data
      INTEGER      CXSLOT        ! Map slot number of calibrationx-axis
                                 ! data
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      DOUBLE PRECISION DUMMY     ! Dummy arguement for magnitude flag
      INTEGER      IGNORE        ! Used to pass ignorable status
      LOGICAL      ISNEWC        ! Is CPTR address new to CNF?
      LOGICAL      ISNEWO        ! Is OPTR address new to CNF?
      LOGICAL      ISNEWS        ! Is SXPTR address new to CNF?
      INTEGER      IY            ! Current spectrum number in the image
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number for output data array
      LOGICAL      PISNC         ! Previous CNF CPTR pointer new?
      LOGICAL      PISNO         ! Previous CNF OPTR pointer new?
      LOGICAL      PISNS         ! Previous CNF SXPTR pointer new?
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    STRINGS(2)*64 ! Units & label of new output data
      INTEGER      SXDIM         ! Number of dimensions of Spectrum
                                 ! x-axis
      INTEGER      SXI           ! Do loop control variable
      INTEGER      SXPTR         ! Pointer to spectrum x-axis data
      INTEGER      SXSLOT        ! Map slot number of spectrum x-axis
                                 ! data
      INTEGER      SX2           ! Size of 2nd. dimension of Spectrum's
                                 ! x-axis
      INTEGER      TPTR          ! Temporary dynamic mem pointer
      INTEGER      TSXPTR        ! Temp. variable to hold initial SXPTR
                                 ! value
      REAL         TIME          ! The exposure time of the spectrum
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value for SPECTRUM and open the file
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE('SPECT',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=NELM/NX
C
C     Get number of dimensions of Spectrum x-axis
C
      CALL DSA_AXIS_SIZE('SPECT',1,2,SXDIM,DIMS,NELM,STATUS)
      SX2=DIMS(2)
C
C     Get value of CALSPECT and open file
C
      CALL DSA_INPUT('CALIB','CALSPECT',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check dimensions are the same as those of SPECT
C
      CALL DSA_DATA_SIZE('CALIB',2,CDIM,DIMS,NELM,STATUS)
      CALL DSA_MATCH_DIMENSION('SPECT',1,'CALIB',1,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check dimensions and data values are the same for the x-axis
C
      CALL DSA_MATCH_AXIS('SPECT',1,'CALIB',1,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the X arrays
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',SXPTR,
     :                       SXSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_MAP_AXIS_DATA('CALIB',1,'READ','FLOAT',CXPTR,
     :                       CXSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Warn if data is not scrunched
C
      PISNS = .FALSE.
      TSXPTR=SXPTR
      DO SXI=1,SX2
         IF (.NOT.FIG_SCRCHK(NX,%VAL(CNF_PVAL(SXPTR)))) THEN
            CALL PAR_WRUSER('Warning: Spectral data is not on '//
     :                              'a linear wavelength scale',IGNORE)
            CALL PAR_WRUSER('This is allowed, but can generate '//
     :                               'a confusing result.',IGNORE)
            GO TO 300   ! Break loop
         END IF
         IF (SXDIM.EQ.2) THEN
            CALL DYN_INCAD(SXPTR,'FLOAT',NX,TPTR,ISNEWS,STATUS)
            IF (PISNS) CALL CNF_UNREGP(SXPTR)
            SXPTR = TPTR
            PISNS = ISNEWS
         END IF
      END DO
  300 CONTINUE
      IF (PISNS) CALL CNF_UNREGP(SXPTR)
      SXPTR=TSXPTR
C
C     Get value of OUTPUT and map its data
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
C
C     Map the calibration spectrum data.
C
      CALL DSA_MAP_DATA('CALIB','READ','FLOAT',CPTR,CSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get the exposure time for the spectrum
C
      CALL DSA_GET_EXPOSURE('SPECT',1.0,0.0,TIME,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Perform the calibration
C
      PISNC = .FALSE.
      PISNO = .FALSE.
      PISNS = .FALSE.

      DO IY=1,NY
         CALL FIG_SPFLUX(TIME,NX,%VAL(CNF_PVAL(SXPTR)),
     :                   %VAL(CNF_PVAL(CPTR)),%VAL(CNF_PVAL(OPTR)))

         CALL DYN_INCAD(OPTR,'FLOAT',NX,TPTR,ISNEWO,STATUS)
         IF (PISNO) CALL CNF_UNREGP(OPTR)
         OPTR = TPTR
         PISNO = ISNEWO

         IF (CDIM.EQ.2) THEN
            CALL DYN_INCAD(CPTR,'FLOAT',NX,TPTR,ISNEWC,STATUS)
            IF (PISNC) CALL CNF_UNREGP(CPTR)
            CPTR = TPTR
            PISNC = ISNEWC
         END IF

         IF (SXDIM.EQ.2) THEN
            CALL DYN_INCAD(SXPTR,'FLOAT',NX,TPTR,ISNEWS,STATUS)
            IF (PISNS) CALL CNF_UNREGP(SXPTR)
            SXPTR = TPTR
            PISNS = ISNEWS
         END IF
      END DO
      IF (PISNC) CALL CNF_UNREGP(CPTR)
      IF (PISNO) CALL CNF_UNREGP(OPTR)
      IF (PISNS) CALL CNF_UNREGP(SXPTR)

C
C     The units of the calibrated spectrum are now the units of the
C     calibrating spectrum (since CSPIKE doesn't worry about the
C     technicality of modifying that to include the 'per counts per
C     second per angstrom' that it should tag on the end.  Copy that
C     over.
C
      CALL DSA_GET_DATA_INFO('CALIB',2,STRINGS,0,DUMMY,STATUS)
      CALL DSA_SET_DATA_INFO('OUTPUT',2,STRINGS,0,DUMMY,STATUS)
C
C     Tidy up
C
  500 CONTINUE
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_SPFLUX (TIME,NX,XDATA,CDATA,DATA)
C
C     F I G _ S P F L U X
C
C     Applies a calibration spectrum to an observed spectrum to
C     generate a flux calibrated spectrum.  The 'units' of the
C     resulting spectrum are unimportant; this routine assumes that
C     the units of the calibration spectrum are 'whatnots per (count
C     per angstrom per second)', that the units of the X data array
C     is angstroms, and that the spectrum to be calibrated is in
C     counts.  The resulting spectrum will then be in 'whatnots'.
C
C     Parameters -   (">" input, "!" modified)
C
C     (>) TIME    (Real) Exposure time in seconds of spectrum to
C                 be calibrated.
C     (>) NX      (Integer) Number of spectral elements
C     (>) XDATA   (Real array XDATA(NX)) Array giving wavelength
C                 of center of each element.
C     (>) CDATA   (Real array CDATA(NX)) The calibration spectrum.
C     (!) DATA    (Real array DATA(NX)) Passed as the spectrum to
C                 be calibrated, returned calibrated.
C
C     Common variables used -  None
C
C     Functions / subroutines used -
C
C     FIG_WAVEST  (FIG_ package) Wavelength of start of element
C
C                                          KS / CIT 16th May 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX
      REAL    TIME, XDATA(NX), CDATA(NX), DATA(NX)
C
C     Functions
C
      REAL FIG_WAVEST
C
C     Local variables
C
      INTEGER IX
      REAL    WEND, WSTART
C
      WSTART=FIG_WAVEST(1,NX,XDATA)
      DO IX=1,NX
         WEND=FIG_WAVEST(IX+1,NX,XDATA)
         DATA(IX)=DATA(IX)*CDATA(IX)/(TIME*ABS(WEND-WSTART))
         WSTART=WEND
      END DO
C
      END
