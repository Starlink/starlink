*+  CROSSPEC - Computes cross-spectrum of two equal length series
      SUBROUTINE CROSSPEC (STATUS)
*
*    Description :
*
*     Calls subroutine XSPEC_XS to calculate the cross-spectrum of two series
*     Y and Z which may both come from the same file or from different files.
*     If the series are not of equal length then the longer is truncated.
*
*    Method :
*
*     See e.g. Bloomfield - "Fourier Analysis of Time Series: An Introduction"
*     Power and cross-spectra are calculated using a NAG FFT routine, these
*     are then smoothed by convolution with a truncated Gaussian. A cosine
*     bell taper can be applied to the input data to reduce leakage in the
*     spectrum, though it will also reduce the total power somewhat.
*     Note that the phase spectrum has a negative slope when Y leads Z.
*
*    Deficiencies :
*
*     The calculated errors in coherency and phase are underestimated at
*     both ends of the spectrum (by a factor root 2 at the very ends) since
*     the assumed symmetry of the spectrum about its ends results in fewer
*     statistically independent points being smoothed over in these regions.
*
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*     Phillip Andrews (BHVAD::PLA)
*    History :
*
*     25 Jul 84 : Original
*      5 Sep 86 : V0.5-1 Some renaming (BHVAD::JCMP)
*     21 Oct 86 : V0.5-2 Graphics removed, more renaming (TJP)
*     12 Jun 87 : code restructured, HIST_ADD added, COMMENT removed (PLA)
*      9 Jul 87 : V0.6-2 Output file -> grafix plot file (TJP)
*     20 Feb 89 : V1.0-1 Major rewrite for ASTERIX88: data files mapped, work
*                 arrays dynamical, more consistency checks between datasets,
*                 smoothing now done simultaneously, code structured. (PLA)
*      8 Oct 92 : V1.7-0 Converted to D.P. for UNIX port (DJA)
*      6 Aug 93 : V1.7-1 MAP_MOV to ARR_COP translation (DJA)
*     20 Apr 95 : V1.8-0 Updated data interface - use GMI_ to create o/p (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    External references :
*
      INTEGER CHR_LEN
*
*    Local variables :
*
      CHARACTER*80              UNITS          		! O/p (frequency) units
      CHARACTER*80              UNITS1         		! Input 1 axis units
      CHARACTER*80              UNITS2         		! Input 2 axis units
      CHARACTER*80              TEXT(20)       		! Text for history
      CHARACTER*4               PAR            		! Parameter name

      REAL                      BANDWIDTH      ! Bandwidth of smoothing window
      REAL                      BASE           ! Base value of output axis
      REAL                      FRAC           ! Fraction of array to taper
                                              ! at each end
      REAL                      SCALE          ! Scale of output axis
      REAL                      SCALE1         ! Scale of ist input axis
      REAL                      SCALE2         ! Scale of 2nd input axis

      INTEGER                   CODAT          ! Pointer to output coherency data
      INTEGER			COFID			! Coherency object
      INTEGER                   COVAR          ! Ptr to coherency variance
      INTEGER			DIMS(ADI__MXDIM,2)	! Input dimensions
      INTEGER                   DPTR1, DPTR2   ! pointers to input data arrays
      INTEGER                   I              ! loop counter
      INTEGER			IFID(2)			! Input dataset ids
      INTEGER                   LEN            ! Length of an axis
      INTEGER                   LEN1, LEN2     ! length of input 1 & 2 units
      INTEGER                   LSHIFT         ! Alignment shift
      INTEGER                   NDIM(2)        ! No of dimensions of input (n)
      INTEGER                   NLINES         ! Number of history text lines
      INTEGER                   NPTS           ! No. of data points
      INTEGER                   NTOT           ! No. of data after padding
      INTEGER                   NV             ! No. of elements in spectra
      INTEGER			OFID(2)			! Output dataset id
      INTEGER                   PHDAT          ! Pointer to output phase data
      INTEGER			PHFID			! Phase object
      INTEGER                   PHVAR          ! Ptr to phase variance
      INTEGER                   QDIMS(ADI__MXDIM)
      INTEGER                   QNDIM
      INTEGER                   QPTR           ! Pointer to quality array
      INTEGER                   SIGMA          ! Sigma width of Gaussian window
      INTEGER                   TPTR           ! temporary pointer
      INTEGER                   WRK1PTR        ! Pointer to work array
      INTEGER                   WRK2PTR        ! Pointer to work array

      LOGICAL                   BAD            ! True if any bad quality values
      LOGICAL                   TAPER          ! Apply taper to data?
      LOGICAL                   INPRIM(2)      ! Is input (n) primitive?
      LOGICAL                   INPUT          ! Continue to look for input?
      LOGICAL                   OK
      LOGICAL                   REG            ! Is axis regularly spaced?
*
*    Version id :
*
      CHARACTER*20		VERSION
        PARAMETER               ( VERSION = 'CROSSPEC Version 1.8-0' )
*-

*    Version
      CALL MSG_PRNT (VERSION)
      CALL AST_INIT

*    Obtain input data objects, and check it.
      DO I = 1, 2
        INPUT = .TRUE.

        DO WHILE ( INPUT )
          INPUT = .FALSE.
          WRITE (PAR, '(A3,I1)') 'INP', I
          CALL USI_TASSOCI( PAR, '*', 'READ', IFID(I), STATUS)
          IF (STATUS .NE. SAI__OK) GOTO 99

*        Check not primitive
          CALL BDI_PRIM( IFID(I), INPRIM(I), STATUS )
          CALL BDI_CHKDATA (IFID(I), OK, NDIM(I), DIMS(1,I), STATUS)

          IF (.NOT. OK) THEN
            CALL MSG_PRNT ('ERROR: Invalid input')
            INPUT = .TRUE.

          ELSE IF (NDIM(I) .NE. 1) THEN
            CALL MSG_PRNT ('ERROR: Dataset must be 1 dimensional')
            INPUT = .TRUE.

          ELSE
*          Check for regular spacing
            CALL BDI_CHKAXVAL (IFID(I), 1, OK, REG, LEN, STATUS)

            IF (OK) THEN
              IF (.NOT. REG) THEN
                STATUS = SAI__ERROR
                CALL ERR_REP(' ', 'CROSSPEC only works for regularly '/
     :                                         /'spaced data', STATUS )

              END IF
            END IF
          END IF
        END DO
      END DO

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Determine data spacing
      SCALE1 = 1.0
      LEN1   = 0

      IF (INPRIM(1) .AND. INPRIM(2)) THEN
        CALL MSG_PRNT ('Assuming unit spacing')

      ELSE IF (.NOT. INPRIM(1) .AND. .NOT. INPRIM(2)) THEN
        CALL BDI_GETAXVAL (IFID(1), 1, BASE, SCALE1, LEN, STATUS)
        CALL BDI_GETAXVAL (IFID(2), 1, BASE, SCALE2, LEN, STATUS)

        IF (SCALE1 .NE. SCALE2) THEN
          CALL MSG_PRNT ('FATAL ERROR: The datasets have different '//
     :                                                       'spacings')
          STATUS = SAI__ERROR
          GOTO 99

        END IF
        CALL BDI_GETAXUNITS (IFID(1), 1, UNITS1, STATUS)
        CALL BDI_GETAXUNITS (IFID(2), 1, UNITS2, STATUS)
        CALL CHR_UCASE( UNITS1 )
        CALL CHR_UCASE( UNITS2 )
        LEN1 = CHR_LEN( UNITS1 )
        LEN2 = CHR_LEN( UNITS2 )

        IF (LEN1 .GT. 0) THEN
          UNITS = '('//UNITS1(1:LEN1)//')^-1)'

          IF (LEN2 .GT. 0) THEN
            IF (UNITS1(1:LEN1) .NE. UNITS2(1:LEN2)) THEN
              CALL MSG_PRNT ('FATAL ERROR: Datasets have different axis'
     :                                                       //' units')
              STATUS = SAI__ERROR
              GOTO 99

            END IF
          ELSE
            CALL MSG_PRNT( 'WARNING: Datasets may have different axis'
     :                                                     //' units')

          END IF
        ELSE IF (LEN2 .GT. 0) THEN
          UNITS = '('//UNITS2(1:LEN2)//')^-1)'
          LEN1  = LEN2

        END IF
      ELSE IF ( .NOT. INPRIM(1) ) THEN
        CALL MSG_PRNT( 'Using axis spacing from first dataset' )
        CALL BDI_GETAXVAL( IFID(1), 1, BASE, SCALE1, LEN, STATUS )

      ELSE IF ( .NOT. INPRIM(2) ) THEN
        CALL MSG_PRNT( 'Using axis spacing from second dataset' )
        CALL BDI_GETAXVAL( IFID(2), 1, BASE, SCALE1, LEN, STATUS )

      END IF

*    If one array is larger than the other then truncate it
      IF (DIMS(1,1) .GT. DIMS(1,2)) THEN
        CALL MSG_PRNT ('First dataset truncated to length of second')
        NPTS = DIMS(1,2)

      ELSE IF (DIMS(1,2) .GT. DIMS(1,1)) THEN
        CALL MSG_PRNT ('Second dataset truncated to length of first')
        NPTS = DIMS(1,1)

      ELSE
        NPTS = DIMS(1,1)

      END IF
      CALL MSG_SETI ('NDAT', NPTS)
      CALL MSG_PRNT ('Using ^NDAT data points')

*    Check data quality
      CALL BDI_CHKQUAL (IFID(1), OK, QNDIM, QDIMS, STATUS)

      IF (OK) THEN
        CALL BDI_MAPLQUAL (IFID(1), 'READ', BAD, QPTR, STATUS)

        IF (BAD) THEN
          CALL MSG_PRNT ('WARNING: First dataset contains bad quality '/
     :                                                   /'data points')

        END IF
        CALL BDI_UNMAPLQUAL (IFID(1), STATUS)

      END IF

      CALL BDI_CHKQUAL (IFID(2), OK, QNDIM, QDIMS, STATUS)

      IF (OK) THEN
        CALL BDI_MAPLQUAL (IFID(2), 'READ', BAD, QPTR, STATUS)

        IF (BAD) THEN
          CALL MSG_PRNT ('WARNING: Second dataset contains bad quality '
     :                                                  //'data points')

        END IF
        CALL BDI_UNMAPLQUAL (IFID(2), STATUS)

      END IF

*    Map data
      CALL BDI_MAPTDATA( IFID(1), '_DOUBLE', 'READ', DPTR1, STATUS )
      CALL BDI_MAPTDATA( IFID(2), '_DOUBLE', 'READ', DPTR2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    User input
      CALL USI_TASSOCO( 'OUT', 'GRAFIX', OFID, STATUS )
      CALL USI_GET0L( 'TAPER', TAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( TAPER ) THEN
        CALL USI_GET0R( 'FRAC', FRAC, STATUS )
      END IF

*    Taper data with cosine bell if required
      IF ( TAPER ) THEN

*      Taper first input
        CALL DYN_MAPD( 1, NPTS, TPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

        CALL ARR_COP1D( NPTS, %VAL(DPTR1), %VAL(TPTR), STATUS )
        DPTR1 = TPTR
        CALL BDI_UNMAPDATA( IFID(1), STATUS )


*      Taper second input
        CALL DYN_MAPD( 1, NPTS, TPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        CALL ARR_COP1D( NPTS, %VAL(DPTR2), %VAL(TPTR), STATUS )
        DPTR2 = TPTR
        CALL BDI_UNMAPDATA( IFID(2), STATUS )

*      Taper copied data
        CALL ARR_TAPERD( %VAL(DPTR1), NPTS, FRAC )
        CALL ARR_TAPERD( %VAL(DPTR2), NPTS, FRAC )

      END IF

*    Pad data with zeroes if necessary (for fft)
      CALL UTIL_EXTEND( NPTS, NTOT )
      NV = (NTOT / 2) + 1

*    Get output scale
      SCALE = 1.0 / (REAL(NTOT) * SCALE1)

      IF (NTOT .GT. NPTS) THEN
        CALL DYN_MAPD( 1, NTOT, TPTR, STATUS )

*      Check status
        IF (STATUS .NE. SAI__OK) GOTO 99

        CALL ARR_INIT1D( 0.0D0, NTOT, %VAL(TPTR), STATUS )
        CALL ARR_COP1D( NPTS, %VAL(DPTR1), %VAL(TPTR), STATUS )
        IF ( TAPER ) THEN
          CALL DYN_UNMAP( DPTR1, STATUS )
        ELSE
          CALL BDI_UNMAPDATA( IFID(1), STATUS )
        END IF
        DPTR1 = TPTR
        CALL DYN_MAPD( 1, NTOT, TPTR, STATUS )

*      Check status
        IF (STATUS .NE. SAI__OK) GOTO 99

        CALL ARR_INIT1D( 0.0D0, NTOT, %VAL(TPTR), STATUS )
        CALL ARR_COP1D( NPTS, %VAL(DPTR2), %VAL(TPTR), STATUS )

        IF ( TAPER ) THEN
          CALL DYN_UNMAP( DPTR2, STATUS )
        ELSE
          CALL BDI_UNMAPDATA( IFID(2), STATUS )
        END IF
        DPTR2 = TPTR

*      Check status
        IF (STATUS .NE. SAI__OK) GOTO 99

        CALL MSG_SETI( 'NTOT', NTOT )
        CALL MSG_PRNT( 'Inputs extended to ^NTOT elements for '//
     :                                           'transforming' )

      END IF

      CALL MSG_SETI( 'NV', NV )
      CALL MSG_PRNT( 'There are ^NV output frequency bins.' )

      CALL USI_GET0I( 'SHIFT', LSHIFT, STATUS )

      IF (STATUS .NE. SAI__OK) GOTO 99

      CALL USI_GET0I( 'SIGMA', SIGMA, STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Create output Multi graph dataset. Need 2 graphs - one for Coherency
*    the other for Phase information.
      CALL GMI_CREMULT( OFID, 2, STATUS )
      CALL GMI_LOCNDF( OFID, 1, COFID, STATUS )
      CALL GMI_LOCNDF( OFID, 2, PHFID, STATUS )

      CALL BDI_PUTTITLE   (COFID, 'Cross Spectrum',     STATUS )
      CALL BDI_PUTLABEL   (COFID, 'Squared Coherency',  STATUS )
      CALL BDI_CREBDS     (COFID, 1, NV, .TRUE., .TRUE.,
     :                                         .FALSE., STATUS )
      CALL BDI_MAPDATA    (COFID, 'WRITE', CODAT,       STATUS )
      CALL BDI_PUTAXVAL   (COFID, 1, 0.0, SCALE, NV,    STATUS )
      CALL BDI_PUTAXLABEL (COFID, 1, 'Frequency',       STATUS )

      CALL BDI_PUTTITLE   (PHFID, 'Cross Spectrum',     STATUS )
      CALL BDI_PUTLABEL   (PHFID, 'Phase',              STATUS )
      CALL BDI_CREBDS     (PHFID, 1, NV, .TRUE., .TRUE.,
     :                                         .FALSE., STATUS )
      CALL BDI_MAPDATA    (PHFID, 'WRITE', PHDAT,       STATUS )
      CALL BDI_PUTAXVAL   (PHFID, 1, 0.0, SCALE, NV,    STATUS )
      CALL BDI_PUTAXLABEL (PHFID, 1, 'Frequency',       STATUS )

      IF ( LEN1 .GT. 0 ) THEN
        CALL BDI_PUTAXUNITS( COFID, 1, UNITS, STATUS )
        CALL BDI_PUTAXUNITS( PHFID, 1, UNITS, STATUS )
      END IF

*    Map work arrays for cross soectrum
      CALL DYN_MAPR( 1, NV, WRK1PTR, STATUS )
      CALL DYN_MAPR( 1, NV, WRK2PTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Compute cross-spectrum
      CALL CROSSPEC_XS (NV, NTOT, LSHIFT, %VAL(DPTR1), %VAL(DPTR2),
     :            SIGMA, %VAL(WRK1PTR), %VAL(WRK2PTR), %VAL(CODAT),
     :                             %VAL(PHDAT), BANDWIDTH, STATUS )

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Calculate coherency & phase variances
      CALL BDI_MAPVAR( COFID, 'WRITE', COVAR, STATUS )
      CALL BDI_MAPVAR( PHFID, 'WRITE', PHVAR, STATUS )

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

      CALL CROSSPEC_VAR( NV, NPTS, NTOT, BANDWIDTH, FRAC, %VAL(CODAT),
     :                     %VAL(PHDAT), TAPER, %VAL(COVAR), %VAL(PHVAR))

*    History
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL USI_NAMEI( NLINES, TEXT, STATUS )
      NLINES       = NLINES + 1
      TEXT(NLINES) = ' '
      NLINES       = NLINES + 1
      TEXT(NLINES) = 'Output smoothed using a truncated Gaussian,'
      NLINES       = NLINES + 1
      WRITE(TEXT(NLINES), '(A,I2,A)') 'of SIGMA width = ',SIGMA,
     :                                                   ' output bins.'
      CALL HSI_PTXT( OFID, NLINES, TEXT, STATUS )

*    Exit
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END




*+  CROSSPEC_XS - Computes the cross spectrum
      SUBROUTINE CROSSPEC_XS( NV, NTOT, LSHIFT, IN1, IN2, SIGMA, WRK1,
     :                             WRK2, COH, PHA, BANDWIDTH, STATUS )
*    Description :
*     Computes the cross spectrum of IN1 & IN2
*    Method :
*
*     Uses NAG routine CO6FCF
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Phil Andrews (BHVAD::PLA)
*
*    History :
*
*     12 Jan 89 : Original (BHVAD::PLA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                  NTOT           ! No. of data after padding
      INTEGER                  NV             ! No. of elements in spectra
      INTEGER                  SIGMA          ! Sigma width of Gaussian window

      REAL                     LSHIFT         ! Alignment shift
      DOUBLE PRECISION         IN1(*)         ! Input data 1
      DOUBLE PRECISION         IN2(*)         ! Input data 2
      REAL                     WRK1(*)        ! Work array
      REAL                     WRK2(*)        ! Work array

*    Export :
      REAL                     COH(*)         ! Coherence spectrum
      REAL                     PHA(*)         ! Phase spectrum
      REAL                     BANDWIDTH      ! Bandwidth of smoothing window

*    Status :
      INTEGER STATUS

*    Local variables :
      DOUBLE PRECISION A1, A2, B1, B2         ! Dummy variables used to calc
      REAL             C, D1, D2              ! values of the wrk arrays.

      INTEGER          I                      ! Loop counter
      INTEGER          IFAIL                  ! Status report from NAG routine
      INTEGER          WKPTR                  ! Pointer to work array for NAG
      INTEGER          WORK1                  ! Pointer to work array
      INTEGER          WORK2                  ! Pointer to work array
      INTEGER          WORK3                  ! Pointer to work array
      INTEGER          WORK4                  ! Pointer to work array
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get dynamic memory for nag routine
      CALL DYN_MAPD( 1, NTOT, WKPTR, STATUS )

*    Call NAG routine
      IFAIL = 0
      CALL C06FCF( IN1, IN2, NTOT, %VAL(WKPTR), IFAIL )

      CALL DYN_UNMAP( WKPTR, STATUS )

      IF (IFAIL .NE. 0) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: An error has occured in'/
     :                               /' the NAG routine', STATUS )
        GOTO 999
      END IF

      DO I = 1, NV
        IF (I .GT. 1) THEN
          A1 = (IN1(I) + IN1(NTOT - I + 2)) / 2.0D0
          A2 = (IN2(I) + IN2(NTOT - I + 2)) / 2.0D0

          B1 = IN1(NTOT - I + 2) - A1
          B2 = A2 - IN2(NTOT - I + 2)

        ELSE
          A1 = IN1(1)
          A2 = IN2(1)

          B1 = 0.0D0
          B2 = 0.0D0

        END IF

        COH(I) = REAL((A1 * A2) + (B1 * B2))  ! Used as work
        PHA(I) = REAL((A1 * B1) - (A2 * B2))  ! arrays here.

*      Perform alignment shift
        IF (LSHIFT .NE. 0) THEN
          C  = 6.2831853 * LSHIFT * (I - 1) / REAL (NTOT)
          D1 = COS (C)
          D2 = SIN (C)
          C  = (COH(I) * D1) - (PHA(I) * D2)
          PHA(I) = (COH(I) * D2) + (PHA(I) * D1)
          COH(I) = C
        END IF

        WRK1(I) = REAL(A1**2 + B2**2)
        WRK2(I) = REAL(A2**2 + B1**2)

      END DO

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 999

*    Create work arrays for the smoothing.
      CALL DYN_MAPR (1, NV, WORK1, STATUS)
      CALL DYN_MAPR (1, NV, WORK2, STATUS)
      CALL DYN_MAPR (1, NV, WORK3, STATUS)
      CALL DYN_MAPR (1, NV, WORK4, STATUS)

      IF (SIGMA .GT. 0) THEN
        CALL CROSSPEC_SMOOTH (NV, SIGMA, %VAL(WORK1), %VAL(WORK2),
     :      %VAL(WORK3), %VAL(WORK4), COH, PHA, WRK1, WRK2, BANDWIDTH)

      END IF

      DO I = 1, NV
        C      = (COH(I)**2 + PHA(I)**2) / (WRK1(I) * WRK2(I))
        PHA(I) = ATAN (PHA(I) / COH(I))
        COH(I) = C
      END DO

*    Exit
 999  IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_REP( ' ', 'from CROSSPEC_XS', STATUS )
      END IF

      END



*+  CROSSPEC_VAR - Calculate the variance on the Phase & coherency spectra
      SUBROUTINE CROSSPEC_VAR (NV, NPTS, NTOT, BANDWIDTH, FRAC, CODAT,
     :                                       PHDAT, TAPER, COVAR, PHVAR)
*    Description :
*     Calculate the variance on Phase & coherency spectra, taking
*     tapering into account.
*    Authors :
*     Phil Andrews
*    History :
*     13-JAN-1989 :  Original (BHVAD::PLA)
*    Type Definitions :
      IMPLICIT NONE
*    Structure definitions :
*    Import :
      INTEGER         NV             ! No. of elements in spectra
      INTEGER         NPTS           ! No. of data points
      INTEGER         NTOT           ! No. of data after padding

      REAL            BANDWIDTH      ! Bandwidth of smoothing window
      REAL            FRAC           ! Fraction of array to taper
                                     ! at each end
      REAL            CODAT(*)       ! Coherency spectrum
      REAL            PHDAT(*)       ! Phase spectrum

      LOGICAL         TAPER          ! Taper input data?

*    Export :
      REAL            COVAR(*)       ! Coherence variance
      REAL            PHVAR(*)       ! Phase variance

*    Local variables :
      INTEGER         I              ! Loop counter

      REAL            C              ! Constant
*-

      C = 1.0

      IF ( TAPER ) THEN
        C = 1.0 - (93.0 * FRAC / 128.0)

      END IF
      C = C * NTOT / (NPTS * BANDWIDTH * 2.0)

      DO I = 1, NV
        COVAR(I) = C * (1.0 - CODAT(I))**2

        IF (ABS(CODAT(I)) .GT. 1E-20) THEN
          PHVAR(I) = SQRT(C / 2.0 * ((1.0 / CODAT(I)) - 1.0))

        ELSE
          PHVAR(I) = SQRT(C /2.0) * 100.0

        END IF
      END DO
      END



*+  CROSSPEC_SMOOTH - Smooths WRKn arrays.
      SUBROUTINE CROSSPEC_SMOOTH (NV, SIGMA, WORK1, WORK2, WORK3, WORK4,
     :                                ARR1, ARR2, ARR3, ARR4, BANDWIDTH)
*    Description :
*     The 4 ARRn arrays are smoothed using a Gaussian of sigma width SIGMA
*    pixels. The Gaussian is cut of at 2.5 * sigma pixels from center.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phillip Andrews (BHVAD::PLA)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER     NV           ! No. of elements in spectra
      INTEGER     SIGMA        ! Sigma width of Gaussian window

      REAL        WORK1(NV)    ! work array to smooth ARR1
      REAL        WORK2(NV)    ! work array to smooth ARR2
      REAL        WORK3(NV)    ! work array to smooth ARR3
      REAL        WORK4(NV)    ! work array to smooth ARR4

*    Import / Export :
      REAL        ARR1(NV)     ! Array to be smoothed
      REAL        ARR2(NV)     ! Array to be smoothed
      REAL        ARR3(NV)     ! Array to be smoothed
      REAL        ARR4(NV)     ! Array to be smoothed

*    Export :
      REAL        BANDWIDTH    ! Bandwidth of smoothing window

*    Status :
      INTEGER     STATUS

*    Local variables :
      INTEGER     LWIN         ! Points to lower end of smoothing window
      INTEGER     UWIN         ! Points to upper end of smoothing window
      INTEGER     REG_LWIN     ! Min value of LWIN. (NB LWIN -ve)
      INTEGER     REG_UWIN     ! Max value of UWIN. (NB UWIN +ve)
      INTEGER     LOOP, I      ! Loop counters
      INTEGER     REGWIN       ! Pointer to array for regular window
      INTEGER     IREGWIN      ! Pointer to array for irregular window

      LOGICAL     IRREG        ! Using IREGWIN

      REAL        SUM          ! Sum of elements of REGWIN
      REAL        WEIGHT       ! Sum of elements of IREGWIN

*    External variables
      REAL        CROSSPEC_BW           ! Bandwidth function
*-
*    Check status.
      IF (STATUS .NE. SAI__OK) RETURN

*    Copy arrays to be smoothed into work arrays
      DO I = 1, NV
        WORK1(I) = ARR1(I)
        WORK2(I) = ARR2(I)
        WORK3(I) = ARR3(I)
        WORK4(I) = ARR4(I)
      END DO

*    Generate regular smoothing function
      REG_UWIN = INT (2.5 * REAL(SIGMA))
      REG_LWIN = - REG_UWIN

      CALL DYN_MAPR (1, REG_UWIN - REG_LWIN + 1, REGWIN, STATUS)
      CALL CROSSPEC_GENWIN (SIGMA, REG_UWIN + 1, REG_LWIN, REG_UWIN,
     :                                                %VAL(REGWIN), SUM)

*    Generate array for irregular smoothing function.
      CALL DYN_MAPR (1, REG_UWIN - REG_LWIN + 1, IREGWIN, STATUS)

*    Loop over input array.
      DO LOOP = 1, NV
        IRREG = .FALSE.

        LWIN = REG_LWIN
        UWIN = REG_UWIN

        IF (LWIN + LOOP .LT. 1) THEN
          IRREG = .TRUE.
          LWIN  = 1 - LOOP
        END IF

        IF (UWIN + LOOP .GT. NV) THEN
          IRREG = .TRUE.
          UWIN  = NV - LOOP
        END IF

        IF (IRREG) THEN
*        Overlapping ends of array - generate special smoothing function
          CALL CROSSPEC_GENWIN (SIGMA, REG_UWIN + 1, LWIN, UWIN,
     :                                            %VAL(IREGWIN), WEIGHT)

        END IF

        CALL CROSSPEC_SMOOTH_DOIT (NV, REG_UWIN + 1, LOOP, LWIN,
     :     UWIN, IRREG, SUM, WEIGHT, %VAL(REGWIN), %VAL(IREGWIN), WORK1,
     :          WORK2, WORK3, WORK4, ARR1(LOOP), ARR2(LOOP), ARR3(LOOP),
     :                                                       ARR4(LOOP))

      END DO

      BANDWIDTH = CROSSPEC_BW (REG_UWIN - REG_LWIN + 1, %VAL(REGWIN))

      END



*+  CROSSPEC_GENWIN - Generate regular smoothing function.
      SUBROUTINE CROSSPEC_GENWIN(SIGMA,OFFSET,START,STOP,WINDOW,SUM)
*    Description :
*     Genarates a normalized Gaussian smoothing function, in the array
*     WINDOW.
*    History :
*    Type Definitions :
      IMPLICIT NONE

*    Import :
      INTEGER     SIGMA                ! Sigma width of Gaussian window
      INTEGER     OFFSET               ! Offset for WINDOW
      INTEGER     START                ! Index for start of window
      INTEGER     STOP                 ! Ondex for end of window

*    Export :
      REAL        WINDOW (*)           ! array for smoothing window
      REAL        SUM                  ! Sum of elements of WINDOW

*    Local variables :
      INTEGER     I                    ! Loop counter

      REAL        AREA                 ! Area under smoothing function.

*    Functions:
      REAL        CROSSPEC_GAUSS          ! Calculates value of Gaussian.

*-

      DO I = START + OFFSET, STOP + OFFSET
        WINDOW(I)  = CROSSPEC_GAUSS (REAL(I - OFFSET), REAL(SIGMA))
      END DO

*    Normalize the area to 1
      AREA = 0.0

      DO I = START + OFFSET, STOP + OFFSET - 1
         AREA = AREA + (0.5 * (WINDOW(I) + WINDOW(I + 1)))
      END DO

      SUM = 0.0

      DO I = START + OFFSET, STOP + OFFSET
        WINDOW(I) = WINDOW(I) / AREA
        SUM = SUM + WINDOW(I)
      END DO

      END




*+  CROSSPEC_SMOOTH_DOIT - Does the actual smoothing.
      SUBROUTINE CROSSPEC_SMOOTH_DOIT (NV, OFFSET, POSN, START, STOP,
     :  IRREG, SUM, WEIGHT, REGWIN, IREGWIN, WORK1, WORK2, WORK3, WORK4,
     :                                           VAL1, VAL2, VAL3, VAL4)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE

*    Import :
      INTEGER     NV                   ! No. of elements in spectra
      INTEGER     OFFSET               ! Offset for windows
      INTEGER     POSN                 ! element being smoothed
      INTEGER     START                ! Index for start of window
      INTEGER     STOP                 ! Ondex for end of window

      LOGICAL     IRREG                ! Using IREGWIN

      REAL        SUM                  ! Sum of elements of REGWIN
      REAL        WEIGHT               ! Sum of elements of IREGWIN
      REAL        REGWIN(*)            ! Regular smoothing window
      REAL        IREGWIN(*)           ! Ends smoothing window
      REAL        WORK1(NV)            ! Copy of ARR1
      REAL        WORK2(NV)            ! Copy of ARR2
      REAL        WORK3(NV)            ! Copy of ARR3
      REAL        WORK4(NV)            ! Copy of ARR4

*    Export :
      REAL        VAL1                 ! Element of smoothed array 1
      REAL        VAL2                 ! Element of smoothed array 2
      REAL        VAL3                 ! Element of smoothed array 3
      REAL        VAL4                 ! Element of smoothed array 4

*    Local variables :
      INTEGER     I                    ! Loop counter

*-
      VAL1 = 0.0
      VAL2 = 0.0
      VAL3 = 0.0
      VAL4 = 0.0

      IF (.NOT. IRREG) THEN
        DO I = START, STOP
          VAL1 = VAL1 + (REGWIN(I + OFFSET) * WORK1(I + POSN))
          VAL2 = VAL2 + (REGWIN(I + OFFSET) * WORK2(I + POSN))
          VAL3 = VAL3 + (REGWIN(I + OFFSET) * WORK3(I + POSN))
          VAL4 = VAL4 + (REGWIN(I + OFFSET) * WORK4(I + POSN))

        END DO
        VAL1 = VAL1 / SUM
        VAL2 = VAL2 / SUM
        VAL3 = VAL3 / SUM
        VAL4 = VAL4 / SUM

      ELSE
        DO I = START, STOP
          VAL1 = VAL1 + (IREGWIN(I + OFFSET) * WORK1(I + POSN))
          VAL2 = VAL2 + (IREGWIN(I + OFFSET) * WORK2(I + POSN))
          VAL3 = VAL3 + (IREGWIN(I + OFFSET) * WORK3(I + POSN))
          VAL4 = VAL4 + (IREGWIN(I + OFFSET) * WORK4(I + POSN))

        END DO
        VAL1 = VAL1 / WEIGHT
        VAL2 = VAL2 / WEIGHT
        VAL3 = VAL3 / WEIGHT
        VAL4 = VAL4 / WEIGHT

      END IF
      END



*+  CROSSPEC_GAUSS - calculates value of gaussian
      REAL FUNCTION CROSSPEC_GAUSS (POSN, SIGMA)
*    Description :
*     Calculates value of Gaussian
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants:
      INCLUDE 'MATH_PAR'
*    Import :
      REAL            SIGMA               ! Sigma of smoothing Gaussian.
      REAL            POSN                ! Position wrt mean.
*-

      IF (ABS(POSN / SIGMA) .LE. 1E-9) THEN
        CROSSPEC_GAUSS = 1.0 / SQRT (2.0 * MATH__PI * SIGMA)

      ELSE
        CROSSPEC_GAUSS = (1.0 / SQRT (2.0 * MATH__PI * SIGMA))
     :         * EXP (-0.5 * (POSN / SIGMA)**2)

      END IF
      END




*+  CROSSPEC_BW - Calculate bandwidth
      REAL FUNCTION CROSSPEC_BW (NPTS, ARRAY)
*    Description :
*     Calculates bandwidth of smoothing window
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*     17/ 2/89:  Original
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER  NPTS

      REAL     ARRAY(NPTS)

*    Local variables :
      INTEGER  I
      REAL BW
*-
      BW = 0.0

      DO I = 1, NPTS
        BW = BW + ARRAY(I)**2
      END DO

      CROSSPEC_BW = 1.0 / BW

      END
