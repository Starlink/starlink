      SUBROUTINE STREAMLINE( STATUS )
*+
*  Name:
*     STREAMLINE

*  Purpose:
*     Reduces a 1-d file by removing bad quality bins

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL STREAMLINE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Takes a 1-dimensional datafile and removes bad quality points.
*     The axis in the output file always becomes simple even when the
*     input axis was spaced. The principle use of this routine is
*     to save space in mainly blank time series.

*  Usage:
*     streamline {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input dataset
*     OUT = CHAR (read)
*        Output dataset

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
*     streamline, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RDS: Richard Saxton (Starlink, University of Leicester)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Jul 1991 V1.8-0 (RDS):
*        Original version.
*     11 Dec 1995 V2.0-0 (DJA):
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
        PARAMETER		( VERSION = 'STREAMLINE Version V2.0-0' )

*  Local Variables:
      CHARACTER*80 		PTEXT(4)                ! History text

      LOGICAL 			LVAR     		! I/p variance present?

      INTEGER			IFID			! Input dataset
      INTEGER			TPTR, DPTR, VPTR	! Input data
      INTEGER			OFID			! Output dataset
      INTEGER 			NTOT                    ! # input data points
      INTEGER 			NGOOD                   ! # good input points
      INTEGER 			NLINES                  ! # history list items
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input data into an array of GOOD values only.
      CALL TIM_GETDAT( IFID, NTOT, NGOOD, TPNTR, DPNTR, LVAR,
     :                 VPNTR, STATUS)

*  Test that bad quality points were found
      IF ( NTOT .EQ. NGOOD ) THEN
        CALL MSG_PRNT('** No bad quality data points were found **')
        GOTO 99
      END IF

*  Create output file
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      CALL BDI_LINK( 'TimeSeries', 1, NGOOD, 'REAL', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Propogate from input...
      CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )
      CALL BDI_COPY( IFID, 'Title,Label,Units', OFID, ' ', STATUS )

*  Write data arrays into output file
      CALL BDI_AXPUT1R( OFID, 1, 'Data', NGOOD, %VAL(TPTR), STATUS )
      CALL BDI_PUT1R( OFID, 'Data', NGOOD, %VAL(DPTR), STATUS )
      IF ( LVAR ) THEN
        CALL BDI_PUT1R( OFID, 'Variance', NGOOD, %VAL(VPTR), STATUS )
      END IF

*  Copy and update history
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL USI_NAMEI( NLINES, PTEXT, STATUS )
      CALL HSI_PTXT( OFID, NLINES, PTEXT, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
