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
*     Richard Beard (University of Birmingham)
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
*     13 Dec 1995 : V2.0-0 ADI port (DJA)
*      9 Jun 1997 : Convert to PDA (RB)
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
       LOGICAL		CHR_SIMLR
*
*    Local variables :
*
      CHARACTER*80              OUNITS          	! O/p (frequency) units
      CHARACTER*80		UNITS(2)              	! Input axis units
      CHARACTER*80              HTXT			! History text

      REAL                      BANDWIDTH      		! Bandwidth of smoothing window
      REAL                      BASE           		! Base value of output axis
      REAL                      FRAC           		! Fraction of array to taper
							! at each end
      REAL                      OSCALE          	! Scale of output axis
      REAL                      SCALE(2)         	! Scale of input axes
      REAL			SPARR(2)		! Spaced array data

      INTEGER			APTR			! Input axis data
      INTEGER                   CODAT          		! O/p coherency data
      INTEGER			COFID			! Coherency object
      INTEGER                   COVAR          		! O/p coherency variance
      INTEGER			DIMS(ADI__MXDIM,2)	! Input dimensions
      INTEGER                   DPTR(2)   		! Input data
      INTEGER                   I              		! loop counter
      INTEGER			IFID(2)			! Input dataset ids
      INTEGER			IFILES			! Input file info
      INTEGER                   LSHIFT         		! Alignment shift
      INTEGER			MOBJ			! MultiGraph object
      INTEGER			NBAD			! # bad quality points
      INTEGER                   NDIM			! I/p dimensionality
      INTEGER                   NPTS           		! # data points
      INTEGER                   NTOT           		! # data after padding
      INTEGER                   NV             		! # elements in spectra
      INTEGER			OFID(2)			! Output dataset id
      INTEGER                   PHDAT          		! Pointer to output phase data
      INTEGER			PHFID			! Phase object
      INTEGER                   PHVAR          		! Ptr to phase variance
      INTEGER                   QPTR           		! Pointer to quality array
      INTEGER                   SIGMA          		! Sigma width of Gaussian window
      INTEGER			TLEN			! Length of text
      INTEGER                   TPTR           		! Temporary pointer
      INTEGER 			ULEN(2)			! Length of UNITS()
      INTEGER                   WRK1PTR        		! Pointer to work array
      INTEGER                   WRK2PTR        		! Pointer to work array

      LOGICAL                   TAPER          		! Apply taper to data?
      LOGICAL               	INPRIM(2)      		! Is input (n) primitive?
      LOGICAL                   INPUT          		! Continue to look for input?
      LOGICAL                   OK
      LOGICAL                   REG            		! Is axis regularly spaced?
*
*    Version id :
*
      CHARACTER*20		VERSION
        PARAMETER               ( VERSION = 'CROSSPEC Version 2.1-0b' )
*-

*    Version
      CALL MSG_PRNT (VERSION)
      CALL AST_INIT

*    Obtain input data objects, and check it.
      DO I = 1, 2
        INPUT = .TRUE.

        DO WHILE ( INPUT )
          INPUT = .FALSE.

          CALL USI_IASSOC( 'INP', I, 'BinDS|Array', 'READ',
     :                     IFID(I), STATUS )
          IF (STATUS .NE. SAI__OK) GOTO 99

*        Check not primitive
          CALL ADI_DERVD( IFID(I), 'Array', INPRIM(I), STATUS )
          CALL BDI_CHK( IFID(I), 'Data', OK, STATUS )
          CALL BDI_GETSHP( IFID(I), 1, DIMS(1,I), NDIM, STATUS )
          IF (STATUS .NE. SAI__OK) GOTO 99

          IF (.NOT. OK) THEN
            CALL MSG_PRNT ('ERROR: Invalid input')
            INPUT = .TRUE.

          ELSE

*        Map the axis values
            CALL BDI_AXMAPR( IFID(I), 1, 'Data', 'READ', APTR, STATUS )
            CALL ARR_CHKREG( %VAL(APTR), DIMS(1,I), REG, BASE, SCALE(I),
     :                       STATUS )

*        Warn user about primitive input
            IF ( INPRIM(I) ) THEN
              CALL MSG_PRNT( 'Will assume primitive input data is '/
     :                       /'regularly spaced' )
            ELSE IF ( .NOT. REG ) THEN
              CALL MSG_PRNT( 'WARNING : input axis appears to be '/
     :         /'irregularly spaced - will use pixel numbers instead' )
              BASE = 1.0
              SCALE(I) = 1.0
            END IF

*        Get axis units
            CALL BDI_AXGET0C( IFID(I), 1, 'Units', UNITS(I), STATUS )
            ULEN(I) = CHR_LEN( UNITS(I) )

          END IF
        END DO
      END DO
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Warn if axis scalings are different by more than 1 in 10^5
      IF ( ABS(SCALE(1)-SCALE(2)) .GT. ABS(SCALE(1)*1.0E-5) ) THEN
        CALL MSG_PRNT( 'WARNING : Axis pixel sizes appear to be'/
     :                 /' different' )
      END IF

*  Warn if axis units are different
      IF ( .NOT. CHR_SIMLR( UNITS(1), UNITS(2) ) ) THEN
        CALL MSG_PRNT( 'WARNING: Datasets have different axis units' )
      END IF
      IF ( ULEN(1) .GT. 0 ) THEN
        OUNITS = '('//UNITS(1)(1:ULEN(1))//')^-1'
      ELSE IF ( ULEN(2) .GT. 0 ) THEN
        OUNITS = '('//UNITS(2)(1:ULEN(2))//')^-1'
      END IF

*  If one array is larger than the other then truncate it
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

*  Map data
      DO I = 1, 2

*    Map the data
        CALL BDI_MAPD( IFID(I), 'Data', 'READ', DPTR(I), STATUS )

*    Warn if quality present as we can't use it
        CALL BDI_CHK( IFID(I), 'Quality', OK, STATUS )
        IF ( OK ) THEN
          CALL BDI_MAPL( IFID(I), 'LogicalQuality', 'READ',
     :                   QPTR, STATUS )
          CALL ARR_CNT1L( NPTS, %VAL(QPTR), .FALSE., NBAD, STATUS )
          IF ( NBAD .GT. 0 ) THEN
            CALL MSG_SETI( 'N', I )
            CALL MSG_PRNT( 'WARNING: Dataset ^N contains bad quality '/
     :                                                 /'data points' )

          END IF
          CALL BDI_UNMAP( IFID(I), 'LogicalQuality', QPTR, STATUS )

        END IF

      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  User input
      CALL ADI_NEW0( 'MultiGraph', MOBJ, STATUS )
      CALL USI_CREAT( 'OUT', MOBJ, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Taper inputs?
      CALL USI_GET0L( 'TAPER', TAPER, STATUS )
      IF ( TAPER ) THEN
        CALL USI_GET0R( 'FRAC', FRAC, STATUS )
      END IF

*  Taper data with cosine bell if required
      IF ( TAPER ) THEN
        DO I = 1, 2
          CALL DYN_MAPD( 1, NPTS, TPTR, STATUS )
          CALL ARR_COP1D( NPTS, %VAL(DPTR(I)), %VAL(TPTR), STATUS )
          DPTR(I) = TPTR
          CALL ARR_TAPERD( NPTS, FRAC, %VAL(DPTR(I)), STATUS )
        END DO
      END IF

*  Pad data with zeroes if necessary (for fft)
      CALL UTIL_EXTEND( NPTS, NTOT )
      NV = (NTOT / 2) + 1

*  Get output scale
      OSCALE = 1.0 / (REAL(NTOT) * SCALE(1))

*  Extend the data?
      IF ( NTOT .GT. NPTS ) THEN

*    Extend each input
        DO I = 1, 2
          CALL DYN_MAPD( 1, NTOT, TPTR, STATUS )
          CALL ARR_INIT1D( 0.0D0, NTOT, %VAL(TPTR), STATUS )
          CALL ARR_COP1D( NPTS, %VAL(DPTR(I)), %VAL(TPTR), STATUS )
          IF ( TAPER ) THEN
            CALL DYN_UNMAP( DPTR(I), STATUS )
          ELSE
            CALL BDI_UNMAP( IFID(I), 'Data', DPTR(I), STATUS )
          END IF
          DPTR(I) = TPTR
        END DO
        IF (STATUS .NE. SAI__OK) GOTO 99

*    Tell user about it
        CALL MSG_SETI( 'NTOT', NTOT )
        CALL MSG_PRNT( 'Inputs extended to ^NTOT elements for '//
     :                                           'transforming' )

      END IF

      CALL MSG_SETI( 'NV', NV )
      CALL MSG_PRNT( 'There are ^NV output frequency bins.' )

*  Control parameters
      CALL USI_GET0I( 'SHIFT', LSHIFT, STATUS )
      CALL USI_GET0I( 'SIGMA', SIGMA, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Create output Multi graph dataset. Need 2 graphs - one for Coherency
*    the other for Phase information.
      CALL GMI_CREMULT( OFID, 2, STATUS )
      CALL GMI_LOCNDF( OFID, 1, '*', COFID, STATUS )
      CALL GMI_LOCNDF( OFID, 2, '*', PHFID, STATUS )
      CALL BDI_LINK( 'BinDS', 1, NV, 'REAL', COFID, STATUS )
      CALL BDI_LINK( 'BinDS', 1, NV, 'REAL', PHFID, STATUS )

      CALL BDI_PUT0C( COFID, 'Title', 'Cross Spectrum', STATUS )
      CALL BDI_PUT0C( COFID, 'Label', 'Squared Coherency', STATUS )
      CALL BDI_AXPUT0C( COFID, 1, 'Label', 'Frequency', STATUS )

      CALL BDI_MAPR( COFID, 'Data', 'WRITE', CODAT, STATUS )
      SPARR(1) = 0.0
      SPARR(2) = OSCALE
      CALL BDI_AXPUT1R( COFID, 1, 'SpacedData', 2, SPARR, STATUS )

      CALL BDI_PUT0C( PHFID, 'Title', 'Cross Spectrum', STATUS )
      CALL BDI_PUT0C( PHFID, 'Label', 'Phase', STATUS )

      CALL BDI_MAPR( PHFID, 'Data', 'WRITE', PHDAT, STATUS )
      CALL BDI_AXPUT1R( PHFID, 1, 'SpacedData', 2, SPARR, STATUS )
      CALL BDI_AXPUT0C( PHFID, 1, 'Label', 'Frequency', STATUS )

      IF ( ULEN(1) .GT. 0 ) THEN
        CALL BDI_AXPUT0C( COFID, 1, 'Units', OUNITS, STATUS )
        CALL BDI_AXPUT0C( PHFID, 1, 'Units', OUNITS, STATUS )
      END IF

*  Map work arrays for cross spectrum
      CALL DYN_MAPR( 1, NV, WRK1PTR, STATUS )
      CALL DYN_MAPR( 1, NV, WRK2PTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Compute cross-spectrum
      CALL CROSSPEC_XS (NV, NTOT, LSHIFT, %VAL(DPTR(1)), %VAL(DPTR(2)),
     :            SIGMA, %VAL(WRK1PTR), %VAL(WRK2PTR), %VAL(CODAT),
     :                             %VAL(PHDAT), BANDWIDTH, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Calculate coherency & phase variances
      CALL BDI_MAPR( COFID, 'Variance', 'WRITE', COVAR, STATUS )
      CALL BDI_MAPR( PHFID, 'Variance', 'WRITE', PHVAR, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

      CALL CROSSPEC_VAR( NV, NPTS, NTOT, BANDWIDTH, FRAC, %VAL(CODAT),
     :                     %VAL(PHDAT), TAPER, %VAL(COVAR), %VAL(PHVAR))

*  History
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL USI_NAMES( 'I', IFILES, STATUS )
      CALL HSI_PTXTI( OFID, IFILES, .TRUE., STATUS )
      CALL MSG_SETI( 'S', SIGMA )
      CALL MSG_MAKE( 'Output smoothed using a truncated Gaussian of '/
     :                         /'SIGMA = ^S output bins', HTXT, TLEN )
      CALL HSI_PTXT( OFID, 1, HTXT, STATUS )

*  Exit
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
*     Richard Beard (ROSAT, University of Birmingham)
*
*    History :
*
*     12 Jan 89 : Original (BHVAD::PLA)
*      9 Jun 97 : Convert to PDA (RB)
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
      INTEGER          WKPTR                  ! Pointer to work array for PDA
      INTEGER          WORK1                  ! Pointer to work array
      INTEGER          WORK2                  ! Pointer to work array
      INTEGER          WORK3                  ! Pointer to work array
      INTEGER          WORK4                  ! Pointer to work array
      INTEGER	       CMPLX		      ! For PDA
      INTEGER	       DIMS(2)		      ! For PDA
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get dynamic memory for PDA routine
      DIMS(1) = 2
      DIMS(2) = NTOT
      CALL DYN_MAPD( 2, DIMS, CMPLX, STATUS )
      CALL DYN_MAPD( 1, 4*NTOT+15, WKPTR, STATUS )
      CALL CROSSPEC_TO_CMPLX( NTOT, IN1, IN2, %VAL(CMPLX) )

*    Call PDA routines
      CALL PDA_DCFFTI( NTOT, %VAL(WKPTR) )
      CALL PDA_DCFFTF( NTOT, %VAL(CMPLX), %VAL(WKPTR) )
      CALL PDA_DC2NAG( NTOT, %VAL(CMPLX), IN1, IN2 )

      CALL DYN_UNMAP( WKPTR, STATUS )
      CALL DYN_UNMAP( CMPLX, STATUS )

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

*+  CROSSPEC_TO_CMPLX - To values into new array for PDA
      SUBROUTINE CROSSPEC_TO_CMPLX( N, X, Y, C )
* Author:
*   RB: Richard Beard (ROSAT, University of Birmingham)
* History:
*    9 Jun 1997: RB
*      Original version
*-

      DOUBLE PRECISION X(N), Y(N), C(2,N)
      INTEGER N

      INTEGER I

      DO I = 1, N
        C(1,I) = X(I)
        C(2,I) = Y(I)
      END DO

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
