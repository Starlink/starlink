      SUBROUTINE AFLINK( STATUS )
*+
*  Name:
*     AFLINK

*  Purpose:
*     Writes a file reference to a source dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL AFLINK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Writes the logical file link specified by the user to the specified
*     source dataset. This link can be used subsequently by ASTERIX to load
*     the linked file automatically. Supplying a null value for the
*     linked file results in the link being deleted.

*  Usage:
*     aflink {parameter_usage}

*  Environment Parameters:
*     INP = UNIV (update)
*        Source dataset
*     LINK = CHAR (read)
*        The logical name of the link
*     LFILE = UNIV (read)
*        The file to be linked, or null

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
*     aflink, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      1 Aug 1995 V2.0-0 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_ERR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'AFLINK Version 2.1-0b' )

*  Local Variables:
      CHARACTER*20		LINK			! Link name

      INTEGER			BFID			! Linked dataset
      INTEGER			IFID			! Source dataset
      INTEGER			LLEN			! Link name length
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get names of source dataset
      CALL USI_ASSOC( 'INP', '*', 'UPDATE', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get name of link
      CALL USI_GET0C( 'LINK', LINK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Issue warning if not a standard link
      LLEN = CHR_LEN( LINK )
      IF ( .NOT. CHR_INSET( 'BGND,RESP,RMF,ARF,VIGN,PRIMARY',
     :                      LINK(:LLEN) ) ) THEN
        CALL MSG_SETC( 'LINK', LINK(:LLEN) )
        CALL MSG_PRNT( 'WARNING : ^LINK is not a ASTERIX supported'/
     :                 /' link name' )

      END IF

*  Object to link
      CALL USI_ASSOC( 'LFILE', '*', 'READ', BFID, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
        BFID = ADI__NULLID

*  Abort on other bad status values
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 99

      END IF

*  Make the link
      CALL FRI_PUTI( IFID, LINK(:LLEN), BFID, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
