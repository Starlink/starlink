      SUBROUTINE COVERAGE( STATUS )
*+
*  Name:
*     COVERAGE

*  Purpose:
*     Produce percentage coverage as function of data value

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL COVERAGE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*
*     Constructs the coverage function for 2d images, or in the case where
*     an input with dimensionality greater than 2 is supplied,the coverage
*     function for each slice. The coverage function is defined as that
*     percentage of the good pixels in a 2D slice with value above a specified
*     percentage of the range of the data in the slice. The user selects
*     the range of data to be covered - this defaults in the full range in
*     the whole dataset.
*
*     The output dataset is then a 1D dataset, with x-axis in units of the
*     input dataset's data, and y-axis in percentage. In the higher dimen-
*     sional case a stack of such functions is returned.

*  Usage:
*     coverage {parameter_usage}

*  Environment Parameters:
*     {parameter_name}[pdims] = {parameter_type} ({parameter_access_mode})
*        {parameter_description}

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
*     {algorithm_description}...

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
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     coverage, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Jun 1992 V1.7-0 (DJA):
*        Original
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*      4 Dec 1995 V2.0-0 (DJA):
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

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'COVERAGE Version 2.2-0' )

*  Local Variables:
      REAL                      DMAX, DMIN              ! Range in data
      REAL			SPARR(2)		! Spaced array data

      INTEGER                   I                       ! General loop variable
      INTEGER                   IDIMS(ADI__MXDIM)       ! Input dimensions
      INTEGER                   IDPTR                   ! Input data ptr
      INTEGER			IFID			! Input dataset id
      INTEGER                   INDIM                   ! Input dimensionality
      INTEGER                   INELM                   ! Input # of points
      INTEGER                   IQPTR                   ! Input quality ptr
      INTEGER                   NCBIN                   ! # coverage bins
      INTEGER                   ODIMS(ADI__MXDIM-1)     ! Output dimensions
      INTEGER                   ODPTR                   ! Output data ptr
      INTEGER			OFID			! Output dataset id

      LOGICAL                   OK                      ! Validity test
      LOGICAL                   QOK                     ! Quality present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get files
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check data
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, IDIMS, INDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( INDIM .LT. 2 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input data should be 2 or 3 dimensional',
     :                                                       STATUS )
      END IF

*  Map data
      CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Quality present?
      CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
      IF ( QOK ) THEN
        CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', IQPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Number of data points
      CALL ARR_SUMDIM( INDIM, IDIMS, INELM )

*  Get range in data
      IF ( QOK ) THEN
        CALL ARR_RANG1RLQ( INELM, %VAL(IDPTR), %VAL(IQPTR), DMIN, DMAX,
     :                                                         STATUS )
      ELSE
        CALL ARR_RANG1R( INELM, %VAL(IDPTR), DMIN, DMAX, STATUS )
      END IF

*  Set defaults for coverage graph
      CALL USI_DEF0R( 'MIN', DMIN, STATUS )
      CALL USI_DEF0R( 'MAX', DMAX, STATUS )

*  Get range for coverage
      CALL USI_GET0R( 'MIN', DMIN, STATUS )
      CALL USI_GET0R( 'MAX', DMAX, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get number of bins for coverage array
      CALL USI_GET0I( 'NBIN', NCBIN, STATUS )

*  Create output dimensions
      ODIMS(1) = NCBIN
      IF ( INDIM .GT. 2 ) THEN
        DO I = 3, INDIM
          ODIMS(I-1) = IDIMS(I)
        END DO
      END IF

*  Create interface object
      CALL BDI_LINK( 'BinDS', INDIM-1, ODIMS, 'REAL', OFID, STATUS )

*  Create output axes. First axis comes from input data, second and
*  subsequent axes are copied from 3rd onwards from input, if present.
      SPARR(1) = DMIN + (DMAX-DMIN)/REAL(NCBIN)/2.0
      SPARR(2) = (DMAX-DMIN)/REAL(NCBIN)
      CALL BDI_AXPUT1R( OFID, 1, 'SpacedData', 2, SPARR, STATUS )
      CALL BDI_COPY( IFID, 'Label', OFID, 'Axis_1_Label', STATUS )
      CALL BDI_COPY( IFID, 'Units', OFID, 'Axis_1_Units', STATUS )
      IF ( INDIM .GT. 2 ) THEN
        DO I = 3, INDIM
          CALL BDI_AXCOPY( IFID, I, ' ', OFID, I-1, STATUS )
        END DO
      END IF
      CALL BDI_PUT0C( OFID, 'Label', 'Coverage', STATUS )
      CALL BDI_PUT0C( OFID, 'Units', 'percent', STATUS )

*  Create output data
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', ODPTR, STATUS )

*  Perform coverage analysis
      CALL COVERAGE_INT( IDIMS(1)*IDIMS(2), INELM/(IDIMS(1)*IDIMS(2)),
     :                   %VAL(IDPTR), QOK, %VAL(IQPTR), NCBIN,
     :                   DMIN, DMAX, %VAL(ODPTR), STATUS )

*  Copy ancillary stuff
      CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )

*  History update
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  COVERAGE_INT - Do coverage analysis
      SUBROUTINE COVERAGE_INT( NIMAGE, NFRAME, IN, QOK, INQ,
     :                         NCOVER, LO, HI, OUT, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     15 Jun 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER                  NIMAGE                  ! # points per frame
      INTEGER                  NFRAME                  ! # frames
      REAL                     IN(NIMAGE,NFRAME)       ! Input data
      LOGICAL                  QOK                     ! Quality present
      LOGICAL                  INQ(NIMAGE,NFRAME)      ! Input quality
      INTEGER                  NCOVER                  ! # coverage bins
      REAL                     LO, HI                  ! Data range for scaling
*
*    Export :
*
      REAL                     OUT(NCOVER,NFRAME)      ! Output coverages
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                     CBIN                    ! Width of bin in %
      REAL                     FRAC                    ! Normalised data value
      REAL                     RANGE                   ! Range of data

      INTEGER                  I                       ! Loop over input bins
      INTEGER                  IC                      ! Loop over coverage bins
      INTEGER                  ICB                     ! Coverage bin number
      INTEGER                  IFRAME                  ! Loop over frames
      INTEGER                  NGOOD                   ! # good points
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Range to cover
      RANGE = HI - LO
      CBIN = 1.0 / NCOVER

*    Loop over frames
      DO IFRAME = 1, NFRAME

*      Zero coverage counter
        DO IC = 1, NCOVER
          OUT(IC,IFRAME) = 0.0
        END DO

*      Quality present?
        IF ( QOK ) THEN
          NGOOD = 0
          DO I = 1, NIMAGE
            IF ( INQ(I,IFRAME) ) THEN
              NGOOD = NGOOD + 1
              IF ( (IN(I,IFRAME).GE.LO) .AND.
     :             (IN(I,IFRAME).LE.HI) ) THEN
                FRAC = (IN(I,IFRAME)-LO)/RANGE
                ICB = INT(FRAC/CBIN) + 1
                OUT(ICB,IFRAME) = OUT(ICB,IFRAME) + 1.0
              END IF
            END IF
          END DO
        ELSE
          NGOOD = NIMAGE
          DO I = 1, NIMAGE
            IF ( (IN(I,IFRAME).GE.LO) .AND.
     :           (IN(I,IFRAME).LE.HI) ) THEN
              FRAC = (IN(I,IFRAME)-LO)/RANGE
              ICB = INT(FRAC/CBIN) + 1
              OUT(ICB,IFRAME) = OUT(ICB,IFRAME) + 1.0
            END IF
          END DO
        END IF

*      Correct coverage bins to cumulative percentage of all good pixels
        DO IC = 1, NCOVER
          OUT(IC,IFRAME) = OUT(IC,IFRAME) * 100.0 / REAL(NGOOD)
          IF ( IC .GT. 1 ) THEN
            OUT(IC,IFRAME) = OUT(IC,IFRAME) + OUT(IC-1,IFRAME)
          END IF
        END DO

      END DO

      END
