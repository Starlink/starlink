      SUBROUTINE HSI1_ADD( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI1_ADD

*  Purpose:
*     Create a new history record in an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI1_ADD( NARG, ARGS, OARG, STATUS )

*  Description:
*     Creates a new history record in an HDS file.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     DAT:
*        CMP_PUT0x	- Write value of HDS component
*        DAT_ANNUL	- Release an HDS locator
*        DAT_ERASE	- Erase an an HDS component
*        DAT_FIND	- Find an HDS component
*        DAT_NEW[0x]    - Create new HDS component
*        DAT_THERE	- Does an HDS component exist?

*  Implementation Deficiencies:
*     The constants in this routine should be loadable resources. This
*     would enable the user to turn off all history creation (somehow).

*  References:
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants
      INCLUDE 'DAT_PAR'					! HDS constants
      INCLUDE 'HIST_PAR'

*  Arguments Given:
      INTEGER			NARG			! # arguments
      INTEGER			ARGS(*)			! Method arguments

*  Arguments Returned:
      INTEGER			OARG			! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Constants:
      CHARACTER*8		UMODE
        PARAMETER		( UMODE = 'NORMAL' )
      INTEGER			ESIZE
        PARAMETER		( ESIZE = 10 )

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CRLOC			! Current record
      CHARACTER*(DAT__SZLOC)	HLOC			! Input HISTORY object
      CHARACTER*(DAT__SZLOC)	LOC			! Output HDS object
      CHARACTER*(DAT__SZLOC)	RLOC			! RECORDS structure

      CHARACTER*80		NAME			! Creator name
      CHARACTER*12           	SYSNAME, NODENAME,
     :                       	RELEASE, VERSION, MACHINE
      CHARACTER*18		TSTR			! Time string

      INTEGER			CURREC			! Current rec number
      INTEGER			ESIZE			! Extend size
      INTEGER			IVERB			! Verbosity
      INTEGER			SIZE			! Number of records

      LOGICAL			THERE			! Exists already?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No output from this method
      OARG = ADI__NULLID

*  Extract locator
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )

*  Ensure history exists
      CALL DAT_THERE( LOC, 'HISTORY', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        CALL HSI_NEW( ARGS(1), STATUS )
      END IF

*  Locate HISTORY component
      CALL DAT_FIND( LOC, 'HISTORY', HLOC, STATUS )

*  Get verbosity
      CALL HSI1_GETVRB( HLOC, IVERB, STATUS )

*  Only write text if verbosity is QUIET or greater
      IF ( IVERB .GE. HIST__QUIET ) THEN

*    Locate RECORDS structure, and find its size
        CALL DAT_FIND( HLOC, 'RECORDS', RLOC, STATUS )
        CALL DAT_SIZE( RLOC, SIZE, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get current record number
        CALL CMP_GET0I( HLOC, 'CURRENT_RECORD', CURREC, STATUS )

*    Extend file if necessary
        CURREC = CURREC + 1
        IF ( CURREC .GT. SIZE ) THEN
          CALL CMP_GET0I( HLOC, 'EXTEND_SIZE', ESIZE, STATUS )
          CALL DAT_ALTER( RLOC, 1, SIZE + ESIZE, STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Increment current record component
        CALL CMP_PUT0I( HLOC, 'CURRENT_RECORD', CURREC, STATUS )

*    Get locator to current record
        CALL DAT_CELL( RLOC, 1, CURREC, CRLOC, STATUS )

*    Get system stuff including node name
        CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE,
     :                                                     STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          NODENAME = 'unknown'
        END IF
        CALL DAT_NEW0C( CRLOC, 'HOST', 30, STATUS )
        CALL CMP_PUT0C( CRLOC, 'HOST', NODENAME, STATUS )

*    Get date and time and write it
        CALL DAT_NEW0C( CRLOC, 'DATE', 18, STATUS )
        CALL HSI0_TIME( TSTR, STATUS )
        CALL CMP_PUT0C( CRLOC, 'DATE', TSTR, STATUS )

*    Write the command name
        CALL ADI_GET0C( ARGS(2), NAME, STATUS )
        NLEN = MAX( 1, CHR_LEN(NAME) )
        CALL DAT_NEW0C( CRLOC, 'COMMAND', NAME(:NLEN), STATUS )
        CALL CMP_PUT0C( CRLOC, 'COMMAND', NAME(:NLEN), STATUS )

*    Release current record
        CALL DAT_ANNUL( CRLOC, STATUS )

*    Release RECORDS
        CALL DAT_ANNUL( RLOC, STATUS )

*  End of verbosity test
      END IF

*  Release HISTORY component
      CALL DAT_ANNUL( HLOC, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HSI1_ADD', STATUS )
      END IF

      END
