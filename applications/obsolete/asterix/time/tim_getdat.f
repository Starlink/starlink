      SUBROUTINE TIM_GETDAT( IFID, NTOT, NGOOD, TPTR, DPTR,
     :                                 LVAR, VPTR, STATUS )
*+
*  Name:
*     TIM_GETDAT

*  Purpose:
*     Read an input times series

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TIM_GETDAT( IFID, NTOT, NGOOD, TPTR, DPTR, LVAR, VPTR, STATUS )

*  Description:
*     Gets an array of times,data values and variances from an
*     input datafile. Outputs the good values.

*  Arguments:
*     IFID = INTEGER (returned)
*        Times series ADI identifier
*     NTOT = INTEGER (returned)
*        Number of points in input arrays
*     NGOOD = INTEGER (returned)
*        Number of points in output arrays
*     TPTR = INTEGER (returned)
*        Pointer to array of times for good points
*     DPTR = INTEGER (returned)
*        Pointer to array of data for good points
*     LVAR = LOGICAL (returned)
*        Were variances found in input file?
*     VPTR = INTEGER (returned)
*        Pointer to array of variances for good points. If variances are
*        not set then they are set to ABS(data)
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Environment Parameters:
*     INP = CHAR (read)
*        Name of input time series dataset

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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     TIM Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/tim.html

*  Keywords:
*     package:tim, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RDS: Richard Saxton (Starlink, University of Leicester)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      8 Jan 1991 (RDS):
*        Original version
*     10 Apr 1993 (DJA):
*        Removed UTIL_MOVEBYTE (for a change)
*     11 Apr 1995 (DJA):
*        Use BDI rather than BDA
*      7 Dec 1995 (DJA):
*        ADI port. Use logical quality rather than UTIL_QUALSPLIT
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'

*  Arguments Returned:
      INTEGER                   IFID,NTOT,NGOOD,TPTR,DPTR,VPTR
      LOGICAL                   LVAR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			IDPTR			! I/p data array
      INTEGER			ITPTR			! I/p time axis
      INTEGER			IVPTR			! I/p variance
      INTEGER 			IQPTR                  ! I/p quality array
      INTEGER			NBAD			! # duff data points
      INTEGER 			NDIM,DIMS(ADI__MXDIM)   ! I/p dimensions

      LOGICAL 			OK                      ! Data ok ?
      LOGICAL 			LQUAL                   ! Were quality values available ?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get identifier to input file
      CALL USI_ASSOC( 'INP', 'TimeSeries', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check data array is ok and find size
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error looking for input data array')
        IF (STATUS .EQ. SAI__OK) STATUS=SAI__ERROR
        GOTO 99
      END IF

*  Can only handle 1-d data
      IF ( NDIM .NE. 1 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP(' ','Error: Data must be 1 dimensional',
     :               STATUS )
        GOTO 99
      END IF

*  Set the number of data points
      NTOT = DIMS(1)

*  Map data array
      CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Error mapping input data array')
        GOTO 99
      END IF

*  Check variance array is present
      CALL BDI_CHK( IFID, 'Variance', LVAR, STATUS )
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT( '* Variance not present in input datafile *' )
      END IF

*  Map variance array. NB: this returns the data array if the variance array
*  is missing.
      CALL BDI_MAPR( IFID, 'Variance', 'READ', IVPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT('Error mapping input variance array')
        GOTO 99
      END IF

*  Check quality array is present
      CALL BDI_CHK( IFID, 'Quality', LQUAL, STATUS )
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('* Quality not present in input datafile *')
      END IF

*  Map quality array if present
      IF ( LQUAL ) THEN

*    Map the quality
        CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', IQPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_PRNT('Error mapping input quality array')
          GOTO 99
        END IF

*    Count the number of bad points
        CALL ARR_NBAD( NTOT, %VAL(IQPTR), NBAD, STATUS )

*    If all quality values are good then dont bother quality checking.
        IF ( NBAD .EQ. 0 ) THEN
          LQUAL = .FALSE.
          CALL BDI_UNMAP( IFID, 'LogicalQuality', IQPTR, STATUS )
        END IF

*    Set number of good points
        NGOOD = NTOT - NBAD

      ELSE
        NGOOD = NTOT

      END IF

*  Get the times of each data bin from the axis
      CALL BDI_AXCHK( IFID, 1, 'Data', OK, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT('** Axis corrupted or incompatible with data '/
     :                /'array **')
        GOTO 99
      END IF

*  If the axis is missing tell user integers will be used.
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT('Axis values not available using 1.0,2.0,3.0...')
      END IF

*  Map the axis values. NB: if the axis is not present then this routine
*  returns integers i.e. 1.0,2.0,3.0...
      CALL BDI_AXMAPR( IFID, 1, 'Data', 'READ', ITPTR, STATUS )
      IF (STATUS .NE. SAI__OK)THEN
         CALL MSG_PRNT('Error mapping axis array')
         GOTO 99
      END IF

*  Map output time axis, data and variance arrays
      CALL DYN_MAPR( 1, NGOOD, TPTR, STATUS )
      CALL DYN_MAPR( 1, NGOOD, DPTR, STATUS )
      CALL DYN_MAPR( 1, NGOOD, VPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT('Error obtaining virtual memory')
        GOTO 99
      END IF

*  If quality values available and none zero then split off the good
*  data values
      IF ( LQUAL ) THEN

*    Copy axis data
        CALL ARR_CCOP1R( NTOT, %VAL(ITPTR), %VAL(IQPTR), %VAL(TPTR),
     :                   STATUS )

*    Copy data
        CALL ARR_CCOP1R( NTOT, %VAL(IDPTR), %VAL(IQPTR), %VAL(DPTR),
     :                   STATUS )

*    Copy variance
        CALL ARR_CCOP1R( NTOT, %VAL(IVPTR), %VAL(IQPTR), %VAL(VPTR),
     :                   STATUS )

*  If quality data is not available then copy the time,data and variance
*  input arrays into the output arrays
      ELSE
        CALL ARR_COP1R( NTOT, %VAL(ITPTR), %VAL(TPTR), STATUS )
        CALL ARR_COP1R( NTOT, %VAL(IDPTR), %VAL(DPTR), STATUS )
        CALL ARR_COP1R( NTOT, %VAL(IVPTR), %VAL(VPTR), STATUS )

      END IF

*  Announce number of data points
      CALL MSG_SETI('NGOOD', NGOOD)
      CALL MSG_SETI('NTOT', NTOT)
      CALL MSG_PRNT('Using ^NGOOD of the ^NTOT input data values')

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'TIM_GETDAT', STATUS )
      END IF

      END
