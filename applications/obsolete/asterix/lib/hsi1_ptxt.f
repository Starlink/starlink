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
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
*        Original version.
*     13 Feb 1996 (DJA):
*        Protect against primitive HDS file
*      5 June 1997 (RB):
*        Extend length of text line to 132 characters
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
      CHARACTER*1		NUL			! String separator
        PARAMETER		( NUL = CHAR(0) )
      INTEGER			TLEN			! History text length
        PARAMETER		( TLEN = 132 )

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CRLOC			! Current record
      CHARACTER*(DAT__SZLOC)	HLOC			! Input HISTORY object
      CHARACTER*(DAT__SZLOC)	LOC			! Output HDS object
      CHARACTER*(DAT__SZLOC)	RLOC			! RECORDS structure
      CHARACTER*(DAT__SZLOC)	TXLOC			! TEXT structure
      CHARACTER*(DAT__SZLOC)	TXCLOC			! TEXT cell

      CHARACTER*512		LINE			! Line of text

      INTEGER			CID			! String array slice
      INTEGER			CURREC			! Current rec number
      INTEGER			I			! Loop over LINE
      INTEGER			ILINE, JLINE 		! Loop over lines
      INTEGER			IPOS, NPOS		! String pointers
      INTEGER			IVERB			! Verbosity
      INTEGER			LLEN			! Length of LINE
      INTEGER			NL 			! # existing lines
      INTEGER			NLINE 			! # lines to write
      INTEGER			NNUL			! # nulls in LINE

      LOGICAL			PRIM			! Object is primitive?
      LOGICAL			THERE			! Exists already?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No output from this method
      OARG = ADI__NULLID

*  Extract locator
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )

*  Ensure not primitive
      CALL DAT_PRIM( LOC, PRIM, STATUS )
      IF ( PRIM ) GOTO 99

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
        JLINE = NL + 1
        DO ILINE = 1, NLINE

*      Locate ILINE'th ADI string and get its data
          CALL ADI_CELL( ARGS(2), 1, ILINE, CID, STATUS )
          CALL ADI_CLEN( CID, LLEN, STATUS )
          CALL ADI_GET0C( CID, LINE(:LLEN), STATUS )
          LLEN = MIN( LLEN, LEN(LINE) )
          CALL ADI_ERASE( CID, STATUS )

*      If the string contains nulls we need to split it into several
*      lines. Count the nulls and extend the text array
          NNUL = 0
          DO I = 1, LLEN
            IF ( LINE(I:I) .EQ. NUL ) NNUL = NNUL + 1
          END DO
          IF ( NNUL .GT. 0 ) THEN
            CALL DAT_SIZE( TXLOC, NL, STATUS )
            CALL DAT_ALTER( TXLOC, 1, NL + NNUL, STATUS )
          END IF

*      Write each bit of LINE to a new HDS string
          IPOS = 1
          DO WHILE ( IPOS .LE. LLEN )

*        Find position of next null
            NPOS = INDEX( LINE(IPOS:LLEN), NUL )
            IF ( NPOS .EQ. 0 ) THEN
              NPOS = LLEN + 1
            ELSE
              NPOS = NPOS + IPOS - 1
            END IF

*        Write the HDS string
            CALL DAT_CELL( TXLOC, 1, JLINE, TXCLOC, STATUS )
            CALL DAT_PUT0C( TXCLOC, LINE(IPOS:NPOS-1), STATUS )
            CALL DAT_ANNUL( TXCLOC, STATUS )

*        Next output line
            JLINE = JLINE + 1

*        Next bit of line
            IPOS = NPOS + 1

          END DO

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
