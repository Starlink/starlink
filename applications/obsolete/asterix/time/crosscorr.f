      SUBROUTINE CROSSCORR( STATUS )
*+
*  Name:
*     CROSSCORR

*  Purpose:
*     Computes cross-correlation of two equal length series

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL CROSSCORR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Calls subroutine TIM_XCOR to calculate the cross-correlation of two series
*     Y and Z which may both come from the same file or from different files.
*     If the series are not of equal length then the longer is truncated.

*  Usage:
*     crosscorr {parameter_usage}

*  Environment Parameters:
*     INP1 = CHAR (read)
*         First input object
*     INP2 = CHAR (read)
*         Second input object
*     LAG = INTEGER (read)
*         Maximum lag to be computed
*     WEIGHTED = LOGICAL (read)
*         Cross-correlation to be weighted?
*     NOISE = LOGICAL (read)
*         Remove expected noise bias?
*     OUT = CHAR (read)
*         Name of output object

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
*     The autocorrelation calculated is the biased version, which has lower
*     variance than the unbiased estimator. It reduces towards zero as the
*     lag increases.
*     Data weights may be taken into account, and the noise contribution
*     can be removed from the denominator of the autocorrelation - see e.g.
*     Weisskopf et al, Ap.J.199, L147.

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
*     The FFT is not used; this involves a time penalty for large data sets.

*  References:
*     {task_references}...

*  Keywords:
*     crosscorr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      4 Apr 1984 V0.6-0 (TJP):
*        Original version.
*     29 Aug 1986 V0.6-1 (JCMP):
*        Graphics removed
*     11 Jun 1987 V1.0-0 (PLA):
*        Restructured, COMMENT removed
*     24 Sep 1988 V1.0-1 (TJP):
*        ASTERIX88 conversion
*     13 Dec 1988 V1.0-2 (TJP):
*        Rationalised to use standard subroutines etc.
*     13 Jun 1990 V1.2-0 (DJA):
*        UTIL_NBAD removed
*     13 Dec 1995 V2.0-0 (DJA):
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
      EXTERNAL                  CHR_LEN
        INTEGER                 CHR_LEN

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'CROSSCORR Version 2.1-0b' )

*  Local Variables:
      CHARACTER*80           	HTXT(2)			! History text

      REAL                   	BASE            	! Axis base value
      REAL                   	OSCALE           	! Axis scale value
      REAL                   	SCALE(2)
      REAL			SPARR(2)		! Spaced array data
      REAL                   	VMIN,VMAX       	! Min and max variance values

      LOGICAL			AOK(2)			! Axis data ok?
      INTEGER			APTR			! Input axis data
      INTEGER                   BF                      ! The base file number
      INTEGER                   DPTR(2)                 ! Mapped input data
      INTEGER                	I			!
      INTEGER                   IFID(2)                 ! Input identifiers
      INTEGER			IFILES			! Input file info
      INTEGER                	LMAX            	! Maximum lag to be computed
      INTEGER                	NBAD            	! No.of bad quality data
      INTEGER                	ND              	! No. of data points used
      INTEGER                	NDIM            	! Dimensionality of data
      INTEGER                   NELM(2)                 ! # points per input
      INTEGER                	NL              	! Total number of lag values
      INTEGER                   QPTR                    ! Input quality pointer
      INTEGER                	TLEN         		! Length of text string
      INTEGER                   VPTR(2)                 ! Mapped input errors
      INTEGER			XCID			! Output dataset id
      INTEGER                	XCPTR           	! Pointer to cross-correln values

      LOGICAL                   DENOISE                 ! Noise variance removed?
      LOGICAL                   OK                      ! Present & correct?
      LOGICAL			REG			! Axis data is regular?
      LOGICAL                   VOK(2)                  ! Variances ok?
      LOGICAL                   WEIGHT                  ! Weighted cross-correlation?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Obtain input data
      DO I = 1, 2

*    Associate file
        CALL USI_IASSOC( 'INP', I, 'BinDS|Array', 'READ', IFID(I),
     :                   STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Data ok?
        CALL BDI_CHK( IFID(I), 'Data', OK, STATUS )
        IF ( .NOT. OK ) THEN
          CALL MSG_SETI( 'N', I )
          STATUS = SAI__ERROR
          CALL ERR_REP( 'BADDAT', 'Invalid data in dataset ^N',
     :                   STATUS )
        END IF

*    Get number of elements
        CALL BDI_GETSHP( IFID(I), 1, NELM(I), NDIM, STATUS )

*    Map input data
        CALL BDI_MAPR( IFID(I), 'Data', 'READ', DPTR(I), STATUS )

*    Variance present?
        CALL BDI_CHK( IFID(I), 'Variance', VOK(I), STATUS )

*    Warn if quality present
        CALL BDI_CHK( IFID(I), 'Quality', OK, STATUS )
        IF ( OK ) THEN
          CALL BDI_MAPL( IFID(I), 'LogicalQuality', 'READ', QPTR,
     :      STATUS )
          CALL ARR_CNT1L( NELM(I), %VAL(QPTR), .FALSE., NBAD, STATUS )
          IF ( NBAD .GT. 0 ) THEN
            CALL MSG_SETI( 'NB', NBAD )
            CALL MSG_SETI( 'NF', I )
            CALL MSG_PRNT( 'WARNING : There are ^NB bad quality '/
     :      /'points in file ^NF which this program cannot handle' )
          END IF
          CALL BDI_UNMAP( IFID(I), 'LogicalQuality', QPTR, STATUS )

        END IF

*    Check axis values
        CALL BDI_AXCHK( IFID(I), 1, 'Data', AOK(I), STATUS )
        IF ( AOK(I) ) THEN
          CALL BDI_AXMAPR( IFID(I), 1, 'Data', 'READ', APTR, STATUS )
          CALL ARR_CHKREG( %VAL(APTR), NELM(I), REG, BASE, SCALE(I),
     :      STATUS )
          IF ( .NOT. REG ) THEN
            CALL MSG_SETI( 'N', I )
            CALL MSG_PRNT( 'WARNING : Axis of input ^N is not '/
     :      /'regular; will continue using pixel numbers instead' )
            SCALE(I) = 1.0
          END IF
        ELSE
          SCALE(I) = 1.0
        END IF

      END DO

*  Warn if axis scalings are different
      IF ( (AOK(1) .AND. AOK(2)) .AND.
     :  (ABS(SCALE(1)-SCALE(2)) .GT. ABS(SCALE(1)*1.0E-5)) ) THEN
        CALL MSG_PRNT( 'WARNING : The two datasets have different'//
     :    ' axis spacing; continuing assuming equal spacing' )
        OSCALE = 1.0
      ELSE
        OSCALE = SCALE(1)
      END IF

*  Truncate to shorter of inputs
      ND = MIN( NELM(1), NELM(2) )
      CALL MSG_SETI( 'NDAT', ND )
      CALL MSG_PRNT( 'Using ^NDAT data points')

*  User input
      CALL USI_DEF0I( 'LAG', ND-1, STATUS )
      CALL USI_GET0I( 'LAG', LMAX, STATUS )
      IF(STATUS.NE.SAI__OK) GOTO 99
      IF ( LMAX .GT. ND-1 ) THEN
        LMAX = ND - 1
        CALL MSG_SETI('LMAX',LMAX)
        CALL MSG_PRNT('Maximum lag possible is ^LMAX')
      END IF

*  Check data variances - only allow weighting if all variances are available
      IF ( VOK(1) .AND. VOK(2) ) THEN

*    Weighting &/or noise correction required?
        CALL USI_GET0L( 'WEIGHTED', WEIGHT, STATUS )
        CALL USI_GET0L( 'NOISE', DENOISE, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Map and check variances
        IF ( WEIGHT .OR. DENOISE ) THEN
          DO I = 1, 2

*        Map the variances
            CALL BDI_MAPR( IFID(I), 'Variance', 'READ', VPTR(I),
     :                     STATUS )

*        Get their range
            CALL ARR_RANG1R( ND, %VAL(VPTR(I)), VMIN, VMAX, STATUS )
            IF ( VMIN .LE. 0.0 ) THEN
              CALL MSG_SETI( 'NF', I )
              CALL MSG_PRNT( 'WARNING : Negative variances present'/
     :          /' in input ^NF, proceeding without weighting' )
              WEIGHT = .FALSE.
              DENOISE = .FALSE.
            END IF

          END DO
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

      END IF

*  Create a cross-correlation object
      CALL USI_CREAT( 'OUT', ADI__NULLID, XCID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Create principal arrays
      NL = 2*LMAX + 1
      CALL BDI_LINK( 'BinDS', 1, NL, 'REAL', XCID, STATUS )

*  Map output data
      CALL BDI_MAPR( XCID, 'Data', 'WRITE', XCPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Compute cross-correlation
      CALL TIM_XCOR( ND, %VAL(DPTR(1)), %VAL(DPTR(2)),
     :  %VAL(VPTR(1)), %VAL(VPTR(2)), WEIGHT, DENOISE, NL,
     :  %VAL(XCPTR))

*  Enter lag values
      SPARR(1) = -LMAX*OSCALE
      SPARR(2) = OSCALE
      CALL BDI_AXPUT1R( XCID, 1, 'SpacedData', 2, SPARR, STATUS )

*  Choose file to copy from
      BF = 1
      IF ( AOK(2) .AND. .NOT. AOK(1) ) BF = 2

*  Create or copy data and axis ancillaries
      CALL BDI_PUT0C( XCID, 'Label', 'Cross-correlation', STATUS )
      CALL BDI_AXPUT0C( XCID, 1, 'Label', 'Lag', STATUS )
      CALL BDI_AXPUT0R( XCID, 1, 'ScalarWidth', OSCALE, STATUS )
      CALL BDI_AXCOPY( IFID(BF), 1, 'Units', XCID, 1, STATUS )

*  Copy ancillaries
      CALL UDI_COPANC( IFID(BF), 'grf', XCID, STATUS )

*  History entry (copy history file from IFID, if available)
      CALL HSI_COPY( IFID(BF), XCID, STATUS )
      CALL HSI_ADD(XCID,VERSION,STATUS)
      CALL USI_NAMES( 'I', IFILES, STATUS )
      CALL HSI_PTXTI( XCID, IFILES, .TRUE., STATUS )

      CALL MSG_SETI('BF',BF)
      CALL MSG_MAKE('NOTE: Above HISTORY records refer to '//
     :  'dataset ^BF',HTXT(1), TLEN)
      IF ( WEIGHT ) THEN
        CALL MSG_SETC( 'W', 'Weighted cross-correlation' )
      ELSE
        CALL MSG_SETC( 'W', 'Unweighted cross-correlation' )
      END IF
      IF ( DENOISE ) THEN
        CALL MSG_SETC( 'N', 'Noise contribution removed from'/
     :                                       /' denominator' )
      ELSE
        CALL MSG_SETC( 'N', 'No noise correction' )
      END IF
      CALL MSG_MAKE( '^W. ^N', HTXT(2), TLEN )
      CALL HSI_PTXT( XCID, 2, HTXT, STATUS )

*  Exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
