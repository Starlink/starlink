      SUBROUTINE SIGNIF( STATUS )
*+
*  Name:
*     SIGNIF

*  Purpose:
*     Change the input binned dataset to its significance

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL SIGNIF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     For a binned dataset, weight the data according to a specified
*     algorithm.

*  Usage:
*     signif {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input data object (binned dataset)
*     OUT = CHAR (read)
*        Output data object (can be same as input)
*     WEIGHT_DATA[] = REAL (read)
*        Array of SIGNIF data
*     OVER = LOGICAL (read)
*        Overwriting to take place ( default is N )

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
*     signif, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     JCMP: Jim Peden (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Jan 1985 V0.6 (JCMP):
*        Original version.
*     20 Jul 1988 V1.0-0 (DJA):
*        Converted to new STARLINK HDS standards
*      7 Oct 1988 V1.0-1 (DJA):
*        Now handles overwriting using OVER hidden parameter
*     23 Aug 1989 V1.0-2 (DJA):
*        Quality handling now included. Output variances are set to unity
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     11 Nov 1995 V2.0-0 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      INTEGER 			INVAR  			! Algorithm number
        PARAMETER 		( INVAR = 1 )

      INTEGER                	MAXLIN
        PARAMETER           	( MAXLIN = 10 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'SIGNIF Version 2.1-0b' )

*  Local Variables:
      CHARACTER*80           	ACTION(MAXLIN) 	        ! History records

      INTEGER			IFID			! I/p file identifier
      INTEGER	             	NELM	      		! Size of input data
      INTEGER                	NREC                    ! # history records written
      INTEGER			OFID			! O/p file identifier
      INTEGER	             	OPTR	                ! O/p data array
      INTEGER	             	QPTR	                ! O/p quality array
      INTEGER	             	WNELM      	        ! Size of SIGNIF data
      INTEGER			WFID			! Weights data file
      INTEGER	             	WPTR	      		! pointer to SIGNIF data

      LOGICAL	             	OK		      	! Data present and valid?
      LOGICAL                	OVERWR             	! Overwriting?
      LOGICAL		     	Q_OK	      		! Quality present and valid?
      LOGICAL		     	V_OK	      		! Variance present and valid?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  See whether we're going to overwrite
      CALL USI_GET0L( 'OVER', OVERWR, STATUS )

*  Obtain data object, access and check it
      IF ( OVERWR ) THEN
        CALL USI_ASSOC( 'INP', 'BinDS', 'UPDATE', OFID, STATUS )
      ELSE
        CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
        CALL USI_CLONE( 'INP', 'OUT', 'BinDS', OFID, STATUS )
        CALL USI_SHOW( 'Output data {OUT}', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get size of data array
      CALL BDI_CHK( OFID, 'Data', OK, STATUS )
      IF ( OK ) THEN
        CALL BDI_GETNEL( OFID, NELM, STATUS )
      ELSE
	STATUS = SAI__ERROR
	CALL ERR_REP( ' ', 'Data object invalid', STATUS )
	GOTO 99
      END IF

*  See if we have quality data in input object
      CALL BDI_CHK( OFID, 'Quality', Q_OK, STATUS )
      IF ( Q_OK ) THEN
        CALL BDI_MAPL( OFID, 'LogicalQuality', 'READ', QPTR, STATUS )
      END IF

*  See if we have error data in input object
      CALL BDI_CHK( OFID, 'Variance', V_OK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  If variance is present, map it as error. If not then get a weights array
*  from the user
      IF ( V_OK ) THEN
	CALL BDI_MAPR( OFID, 'Error', 'UPDATE', WPTR, STATUS )
      ELSE

*    We don't so try for a weights data array
	CALL MSG_PRNT( 'No error data in object' )
        CALL MSG_SETI( 'N', NELM )
	CALL MSG_PRNT( 'Data has ^N values' )

        CALL USI_ASSOC( 'WEIGHT_DATA', 'BinDS|Array', 'READ',
     :                  WFID, STATUS )
	IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check weights object
        CALL BDI_CHK( WFID, 'Data', OK, STATUS )
        IF ( .NOT. OK ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Weights data invalid', STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

        CALL BDI_GETNEL( WFID, WNELM, STATUS )

	IF ( NELM .NE. WNELM ) THEN
	  STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Sizes of input data and weights arrays '/
     :                                        /'don''t match', STATUS )
	END IF

        CALL BDI_MAPR( WFID, 'Data', 'READ', WPTR, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Map output data. Since we've copied the data over to the output if
*  we're not overwriting, we just overwrite the output data.
      CALL BDI_MAPR( OFID, 'Data', 'UPDATE', OPTR, STATUS )
      IF ( Q_OK ) THEN
        CALL UTIL_WEIGHTQ( INVAR, NELM, %VAL(OPTR), %VAL(QPTR),
     :                         %VAL(WPTR), %VAL(OPTR), STATUS )
      ELSE
        CALL UTIL_WEIGHT( INVAR, NELM, %VAL(OPTR), %VAL(WPTR),
     :                                    %VAL(OPTR), STATUS )
      END IF

*  Set output VARIANCE array to unity if present
      IF ( V_OK ) THEN
        CALL ARR_INIT1R( 1.0, NELM, %VAL(WPTR), STATUS )
      END IF

*  History file entry
      CALL HSI_ADD( OFID, VERSION, STATUS)
      ACTION(1)='Input data {INP}'
      IF ( V_OK ) THEN
        ACTION(2)='Weight data was variance array'
      ELSE
        ACTION(2)='Weights data {WEIGHT_DATA}'
      END IF
      IF ( Q_OK ) THEN
        ACTION(3)='Data quality taken into account'
      ELSE
        ACTION(3)='No bad quality points in dataset'
      END IF
      NREC = MAXLIN
      CALL USI_TEXT( 3, ACTION, NREC, STATUS )
      CALL HSI_PTXT( OFID, NREC, ACTION, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
