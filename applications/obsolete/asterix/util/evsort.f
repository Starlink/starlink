      SUBROUTINE EVSORT( STATUS )
*+
*  Name:
*     EVSORT

*  Purpose:
*     Reorder an event list on the values in a particular list

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL EVSORT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The events in the input dataset are sorted into order by the value
*     of one of the lists.

*  Usage:
*     evsort {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input dataset
*     OUT = CHAR (read)
*        Input dataset

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
*     evsort, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 May 1993 V1.7-0 (DJA):
*        Original version.
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     14 Aug 1995 V1.8-1 (DJA):
*        Started ADI conversion
*     18 Aug 1995 V2.0-0 (DJA):
*        Full ADI port
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
      INTEGER                	MXLIN              	! Max amount of history
        PARAMETER            	( MXLIN = 8 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'EVSORT Version V2.0-0' )

*  Local Variables:
      CHARACTER*20		MTYPE			! List mapping type
      CHARACTER*20		NAME			! Any old list name
      CHARACTER*20 		SLIST              	! Sort list name
      CHARACTER*80           	TXT(MXLIN)         	! History text
      CHARACTER*20		TYPE			! List data type

      INTEGER                	I                  	! Loop counters
      INTEGER			IFID			! Input dataset
      INTEGER			ILID			! Input list id
      INTEGER			LLEN            	! List length
      INTEGER                	IPTR, OPTR         	! I/p and o/p list data
      INTEGER                	IDPTR              	! Sort index
      INTEGER                	NLIN               	! # text lines used
      INTEGER                	NLIST              	! # lists in input
      INTEGER			OFID			! Output dataset

      LOGICAL                	ASCEND             	! Sort in ascending order?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Obtain data objects
      CALL USI_ASSOC( 'INP', 'EventDS', 'READ', IFID, STATUS )
      CALL USI_CLONE( 'INP', 'OUT', 'EventDS', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get number of lists and events
      CALL EDI_GETNS( IFID, LLEN, NLIST, STATUS )

*  Tell user if there aren't any
      IF ( NLIST .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'There are no lists to print', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Define the default to the SLIST parameter
      CALL EDI_DEFLD( IFID, 'SLIST', 'T', 'name', STATUS )

*  Locate list to be sorted
      CALL EDI_SELCTN( IFID, 'SLIST', SLIST, STATUS )

*  Ascending order?
      CALL USI_GET0L( 'ASCEND', ASCEND, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Map input list
      CALL EDI_MAPD( IFID, SLIST, 'READ', 0, 0, IPTR, STATUS )

*  Create index array
      CALL DYN_MAPI( 1, LLEN, IDPTR, STATUS )
      CALL ARR_REG1I( 1, 1, LLEN, %VAL(IDPTR), STATUS )

*  Sort the index
      CALL MSG_SETC( 'SL', SLIST )
      CALL MSG_PRNT( 'Sorting by ^SL...' )
      CALL SORT_IDXD( LLEN, %VAL(IPTR), ASCEND, %VAL(IDPTR), STATUS )

*  Unmap sort list
      CALL EDI_UNMAP( IFID, SLIST, STATUS )

*  Move data using index for each output list
      DO I = 1, NLIST

*    Locate input list
        CALL EDI_IDX( IFID, I, ILID, STATUS )
        CALL ADI_CGET0C( ILID, 'TYPE', TYPE, STATUS )

*    Get the name of this list
        CALL ADI_CGET0C( ILID, 'Name', NAME, STATUS )

*    Choose mapping type
        CALL EDI_MTYPE( ILID, MTYPE, STATUS )

*    Map input and output lists
        CALL EDI_MAP( IFID, NAME, MTYPE, 'READ', 0, 0, IPTR, STATUS )
        CALL EDI_MAP( OFID, NAME, MTYPE, 'WRITE', 0, 0, OPTR, STATUS )

*    Move the data using the index
        CALL SORT_MVIDXT( LLEN, MTYPE, %VAL(IPTR), %VAL(IDPTR),
     :                                     %VAL(OPTR), STATUS )

*    Unmap the lists
        CALL EDI_UNMAP( IFID, NAME, STATUS )
        CALL EDI_UNMAP( OFID, NAME, STATUS )

*    Free the list
        CALL ADI_ERASE( ILID, STATUS )

      END DO

*  Free the index array
      CALL DYN_UNMAP( IDPTR, STATUS )

*  Write history
      CALL HSI_ADD( OFID, VERSION, STATUS )
      TXT(1) = 'Input evds {INP}'
      CALL MSG_SETC( 'L', SLIST )
      IF ( ASCEND ) THEN
        CALL MSG_SETC( 'O', 'ascending' )
      ELSE
        CALL MSG_SETC( 'O', 'descending' )
      END IF
      CALL MSG_SETC( 'L', SLIST )
      TXT(2) = 'Sorted by list ^L in ^O order'
      NLIN = MXLIN
      CALL USI_TEXT( 3, TXT, NLIN, STATUS )
      CALL HSI_PTXT( OFID, NLIN, TXT, STATUS )

*  Exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
