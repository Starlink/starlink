      SUBROUTINE HSI1_PTXT( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI1_PTXT

*  Purpose:
*     Write history text to current history record in an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI1_PTXT( NARG, ARGS, OARG, STATUS )

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
      INCLUDE 'HSI_PAR'

*  Arguments Given:
      INTEGER			NARG			! # arguments
      INTEGER			ARGS(*)			! Method arguments

*  Arguments Returned:
      INTEGER			OARG			! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      INTEGER			TLEN			! History text length
        PARAMETER		( TLEN = 80 )

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CRLOC			! Current record
      CHARACTER*(DAT__SZLOC)	HLOC			! Input HISTORY object
      CHARACTER*(DAT__SZLOC)	LOC			! Output HDS object
      CHARACTER*(DAT__SZLOC)	RLOC			! RECORDS structure
      CHARACTER*(DAT__SZLOC)	TXLOC			! TEXT structure
      CHARACTER*(DAT__SZLOC)	TXCLOC			! TEXT cell

      CHARACTER*132		LINE			! Line of text

      INTEGER			CID			! String array slice
      INTEGER			CURREC			! Current rec number
      INTEGER			ILINE 			! Loop over lines
      INTEGER			IVERB			! Verbosity
      INTEGER			NL 			! # existing lines
      INTEGER			NLINE 			! # lines to write

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
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'History structure has not been created'/
     :                /' - programmer error', STATUS )
        GOTO 99
      END IF

*  Locate HISTORY component
      CALL DAT_FIND( LOC, 'HISTORY', HLOC, STATUS )

*  Get verbosity
      CALL HSI1_GETVRB( HLOC, IVERB, STATUS )

*  Only write text if verbosity is NORMAL or greater
      IF ( IVERB .GE. HSI__NORMAL ) THEN

*    Get number of lines
        CALL ADI_SIZE( ARGS(2), NLINE, STATUS )

*    Locate RECORDS structure, and find its size
        CALL DAT_FIND( HLOC, 'RECORDS', RLOC, STATUS )

*    Get current record number
        CALL CMP_GET0I( HLOC, 'CURRENT_RECORD', CURREC, STATUS )

*    Get locator to current record
        CALL DAT_CELL( RLOC, 1, CURREC, CRLOC, STATUS )

*    Is there already a text component ?
        CALL DAT_THERE( CRLOC, 'TEXT', THERE, STATUS )

*    If TEXT doesn't exist, create it big enough to hold our text
        IF ( .NOT. THERE ) THEN
          CALL DAT_NEWC( CRLOC, 'TEXT', TLEN, 1, NLINE, STATUS )
          NL = 0
          CALL DAT_FIND( CRLOC, 'TEXT', TXLOC, STATUS )

*    Otherwise extend it
        ELSE
          CALL DAT_FIND( CRLOC, 'TEXT', TXLOC, STATUS )
          CALL DAT_SIZE( TXLOC, NL, STATUS )
          CALL DAT_ALTER( TXLOC, 1, NL + NLINE, STATUS )

        END IF

*    Write lines one at a time
        DO ILINE = 1, NLINE

*      Locate ILINE'th ADI string
          CALL ADI_CELL( ARGS(2), 1, ILINE, CID, STATUS )

*      Locate the (NL+ILINE)'th HDS string
          CALL DAT_SLICE( TXLOC, 1, NL + ILINE, -1, TXCLOC, STATUS )

*      Extract the string and write it
          CALL ADI_GET0C( CID, LINE, STATUS )
          CALL DAT_PUT0C( TXCLOC, LINE(:TLEN), STATUS )

*      Release the sliced objects
          CALL ADI_ERASE( CID, STATUS )
          CALL DAT_ANNUL( TXCLOC, STATUS )

        END DO

*    Release TEXT structure
        CALL DAT_ANNUL( TXLOC, STATUS )

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
        CALL AST_REXIT( 'HSI1_PTXT', STATUS )
      END IF

      END
