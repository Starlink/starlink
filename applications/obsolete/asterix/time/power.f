      SUBROUTINE POWER( STATUS )
*+
*  Name:
*     POWER

*  Purpose:
*     FFT power spectrum program.

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL POWER( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Calculates the power spectrum of a 1D data array using an FFT for
*     speed. Normalization used is:
*                 power=F*conj(F)=(wave amplitude/2)**2
*     where F(j)=(1/N)*sum{f(k)*exp(-i*2*pi*j*k/N)}.
*     Since the data are implicitly assumed to repeat periodically, the
*     jump between the end values introduces spurious high frequency power.
*     This can be reduced by tapering the ends of the data, after removal
*     of the data mean. The user may also wish to remove the data mean in
*     order to avoid a large zero order power component (which may dominate
*     plots - the other values are unaffected by its presence, since the
*     different frequency terms are orthogonal).
*     Although the transform can be calculated for any number of elements,
*     numbers containing large prime factors can take a very long time.
*     The user can therefore elect to truncate the data (by <10%) to avoid
*     this problem.

*  Usage:
*     power {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input dataset
*     TRUNCATE = LOGICAL (read)
*        Truncate input data
*     TAPER = LOGICAL (read)
*        Apply cosine taper to input data?
*     REMOVE_MEAN = LOGICAL (read)
*        Remove mean from data before doing FFT?
*     OUT = CHAR (read)
*        Output power spectrum dataset

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     The basic technique is standard.
*     The data must not be modified by the programme, so a temporary data
*     object is used for scratch storage if mean-removal or tapering is
*     requested.
*     Arrays are mapped so there is no size limitation.

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     Errors & Quality ignored.
*     HISTORY should contain more info.
*     Only handles 1d input data.

*  References:
*     {task_references}...

*  Keywords:
*     power, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      8 Jan 1986 V0.6-0 (TJP):
*        Original version.
*     22 Jan 1986 V0.6-1 (TJP):
*        Bug in TIM_FPOWER fixed
*     20 Mar 1986 V0.6-2 (TJP):
*        Array truncation option, COMMENTS removed
*      1 Sep 1986 (TJP):
*        SGS workstation name updated
*     16 Mar 1987 V1.0-0 (PLA):
*        Rewritten to ROSAT specification. Previous name: POWER
*      9 Jun 1988 V1.1-0 (PLA):
*        Converted to new STARLINK HDS file standards.
*        No longer uses a temporary array for the data.
*     14 Jun 1990 V1.2-0 (DJA):
*        Checks for irregular axes and bad quality
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     20 Apr 1995 V1.8-1 (DJA):
*        New data interface
*      5 Dec 1995 V2.0-0 (DJA):
*        ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER                	CHR_LEN

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'POWER Version 2.1-0b' )

*  Local Variables:
      CHARACTER*80           	UNITS        		! Data/axis units

      REAL                   	BASE          		! Base value of input axis values
      REAL                   	DX            		! Data spacing.
      REAL			SPARR(2)		! Spaced array values

      INTEGER			AXPTR			! Input axis data
      INTEGER                	DIMS(ADI__MXDIM)	! I/p dimensions
      INTEGER                	DPTR          		! Pointer to TIM_FPOWER array.
      INTEGER			IFID			! Input dataset id
      INTEGER                	NBAD          		! # of bad points
      INTEGER                	NDIM         		! Dimensionality of data.
      INTEGER                	NTRUNC        		! # points after truncation.
      INTEGER                	NV            		! # freqs in power spec.
      INTEGER			OFID			! Output dataset id
      INTEGER                	QPTR          		! I/p quality
      INTEGER                	YPTR          		! Pointer to input data array.
      INTEGER			ULEN			! Length of new units

      LOGICAL                	DEMEAN        		! Remove data mean?
      LOGICAL			ISDS			! I/p non-primitive
      LOGICAL                	OK            		! General test
      LOGICAL                	REG           		! Is i/p regularly spaced?
      LOGICAL                	QOK           		! Quality there?
      LOGICAL                	TAPER         		! Data taper required?
      LOGICAL                	TRUNC         		! Data to be truncated?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Obtain data object, access and check it.
      CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
      CALL ADI_DERVD( IFID, 'BinDS', ISDS, STATUS )

*  Get data object
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF ( OK ) THEN
        CALL BDI_MAPR( IFID, 'Data', 'READ', YPTR, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: No data object found', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Create output file
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check number of dimensions
      IF ( NDIM .EQ. 1 ) THEN

*    Look for independent variable
        CALL BDI_AXMAPR( IFID, 1, 'Data', 'READ', AXPTR, STATUS )
        CALL ARR_CHKREG( %VAL(AXPTR), DIMS(1), REG, BASE, DX, STATUS )
        IF ( (STATUS.EQ.SAI__OK) .AND. REG ) THEN

        ELSE IF ( STATUS .EQ. SAI__OK ) THEN
          CALL MSG_PRNT( 'FATAL ERROR: Cannot operate on irreg'/
     :                                              /'ular data' )
          STATUS = SAI__ERROR
        ELSE
          DX = 1.0
          CALL MSG_PRNT ('WARNING: Invalid axis data - unit '/
     :                                      /'spacing assumed' )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check for bad quality
        CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
        IF ( QOK ) THEN
          CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', QPTR, STATUS )
          CALL ARR_CNT1L( DIMS(1), %VAL(QPTR), .FALSE., NBAD, STATUS )
          IF ( NBAD .GT. 0 ) THEN
            CALL MSG_SETI( 'NBAD', NBAD )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'WARNING: There are ^NBAD bad quality '/
     :                     /'points present, aborting...', STATUS )
            GOTO 99
          END IF
        END IF

*    Inform user about length of data set
        CALL MSG_SETI( 'NDAT', DIMS(1) )
        CALL MSG_PRNT( '^NDAT data points entered' )

*    User input
        CALL USI_GET0L( 'TRUNCATE', TRUNC, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

        IF ( TRUNC ) THEN
          CALL TIM_TRUNC( DIMS(1), NTRUNC )
          CALL MSG_SETI('NTRUNC', NTRUNC )
          CALL MSG_PRNT( 'Truncated to ^NTRUNC values' )
          DIMS(1) = NTRUNC
        END IF

*    User input
        CALL USI_GET0L( 'TAPER', TAPER, STATUS )
        IF ( .NOT. TAPER ) THEN
          CALL USI_GET0L( 'REMOVE_MEAN', DEMEAN, STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map space for output
        CALL DYN_MAPR( 1, DIMS(1), DPTR, STATUS )

        IF ( TAPER .OR. DEMEAN ) THEN

*      Set up modified data array with mean subtracted
          CALL POWER_MEANSUB( DIMS(1), %VAL(YPTR), %VAL(DPTR) )

        ELSE

*      Original array to be passed across
          CALL ARR_COP1R( DIMS(1), %VAL(YPTR), %VAL(DPTR), STATUS )

        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Taper 10% of data at each end if required
*    Should the fraction tapered be user selectable??
        IF ( TAPER ) THEN
          CALL ARR_TAPERR( DIMS(1), 0.1, %VAL(DPTR), STATUS )
        END IF

*    Compute power spectrum
        CALL TIM_FPOWER( DIMS(1), %VAL(DPTR), NV, STATUS )

*    Create output data array
        CALL BDI_LINK( 'PowerSpectrum', 1, NV, 'REAL', OFID, STATUS )
        CALL BDI_PUT1R( OFID, 'Data', NV, %VAL(DPTR), STATUS )

*    Create components in output file.
*    Start with the axis values
        SPARR(1) = 0.0
        SPARR(2) = 1.0 / ( REAL(DIMS(1)) * DX )
        CALL BDI_AXPUT1R( OFID, 1, 'SpacedData', 2, SPARR, STATUS )
        CALL BDI_AXPUT0C( OFID, 1, 'Label', 'Frequency', STATUS )
        CALL BDI_PUT0C( OFID, 'Label', 'Power', STATUS )

*    Copy stuff from input
        IF ( ISDS ) THEN

*      The data units
          CALL BDI_GET0C( IFID, 'Units', UNITS, STATUS )
          IF ( UNITS .GT. ' ' ) THEN
            CALL MSG_SETC( 'UN', UNITS )
            CALL MSG_MAKE( '(^UN)**2', UNITS, ULEN )
            CALL BDI_PUT0C( OFID, 'Units', UNITS(:ULEN), STATUS )
          END IF

*      Frequency units
          CALL BDI_AXGET0C( IFID, 1, 'Units', UNITS, STATUS)
          IF ( UNITS .GT. ' ' ) THEN
            CALL MSG_SETC( 'UN', UNITS )
            CALL MSG_MAKE( '(^UN)**-1', UNITS, ULEN )
            CALL BDI_AXPUT0C( OFID, 1, 'Units', UNITS(:ULEN), STATUS)
          END IF

          CALL BDI_COPY( IFID, 'Title', OFID, ' ', STATUS )

        END IF

*    Copy ancillaries
        CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )

*    Copy History.
        CALL HSI_COPY( IFID, OFID, STATUS )

*    Add new history record
        CALL HSI_ADD( OFID, VERSION, STATUS )

      ELSE
        CALL ERR_REP( ' ', 'FATAL ERROR: Data is not one-dimensional',
     :                                                        STATUS )

      END IF

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  POWER_MEANSUB - Calculates data mean, and subtracts it from the data.
      SUBROUTINE POWER_MEANSUB( N, YIN, YOUT )
*    Description :
*      Subtracts mean from array YIN to give YOUT.
*      Mean is returned in YMEAN.
*    History :
*     8 Jan 86: original version (BHVAD::TJP)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                N             ! Number of data points.
      REAL                   YIN( * )      ! Input data vector.

*    Export :
      REAL                   YOUT( * )     ! Output mean subtracted data vector.

*    Local variables :
      INTEGER                LOOP          ! Dummy variable for DO loops.

      DOUBLE PRECISION       SUM           ! Dummy variable used in calc. of mean.
      DOUBLE PRECISION       YMEAN         ! Mean value of YIN.

*-

      SUM = 0.0D0

      DO LOOP = 1, N
        SUM = SUM + DBLE( YIN( LOOP ) )
      END DO

      YMEAN = SUM / DBLE( N )

      DO LOOP = 1, N
        YOUT( LOOP ) = REAL( DBLE( YIN(LOOP) ) - YMEAN )
      END DO

      CALL MSG_SETD ('YMEAN', YMEAN)
      CALL MSG_PRNT ('Data mean ^YMEAN subtracted')

      END




*+  TIM_TRUNC - converts array length for FFT routines
	SUBROUTINE TIM_TRUNC( NOLD, NNEW )
*    Description :
*	Given an array length NOLD, returns a length NNEW <= NOLD which
*	contains no large prime factors and will therefore allow rapid
*	Fourier transformation by FOURT.
*	No more than 10% of the data will be truncated.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*	  Version 1	(20-Mar-86)
*     5/9/88: ASTERIX88 version (PLA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER                NOLD                          ! Number of data points
*    Export :
      INTEGER                NNEW                          ! Number of truncated data points
*    Local variables :
      INTEGER                M
      INTEGER                MULT                          ! Multiplier

      REAL                   BASE                          ! Base value

      LOGICAL                CONTINUE
*-

      IF ( NOLD .LE. 64) THEN
        NNEW = NOLD

      ELSE
*      Calculate D=log2(N)-INT[log2(N)]
	M = INT( ALOG10( REAL(NOLD) ) / ALOG10( 2.0 ) )

*      If already a power of two then return
	IF( NOLD. EQ. 2**M .OR. NOLD .EQ. 2**(M+1) ) THEN
	  NNEW = NOLD

        ELSE
*        Truncate
          CONTINUE = .TRUE.
          BASE     = 2.0 ** (M - 5)
          MULT     = 64

          DO WHILE ( CONTINUE )
            MULT = MULT - 2
            NNEW = REAL(MULT) * BASE

            IF ( NNEW .LE. NOLD ) THEN
              CONTINUE = .FALSE.

            END IF
          END DO
        END IF
      END IF
      END
