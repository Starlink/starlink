      SUBROUTINE HSI2_PTXT( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI2_PTXT

*  Purpose:
*     Write history text to current history record in an FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI2_PTXT( NARG, ARGS, OARG, STATUS )

*  Description:
*     Creates a new history record in an FITS file.

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
*      6 Jun 1996 (DJA):
*        Original version, adapted from HDS version
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
      INCLUDE 'HSI_PAR'

*  Arguments Given:
      INTEGER			NARG, ARGS(*)

*  Arguments Returned:
      INTEGER			OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*1		NUL			! String separator
        PARAMETER		( NUL = CHAR(0) )
      INTEGER			TLEN			! History text length
        PARAMETER		( TLEN = 132 )

*  Local Variables:
      CHARACTER*512		LINE			! Line of text

      INTEGER			CID			! String array slice
      INTEGER			I			! Loop over LINE
      INTEGER			ILINE			! Loop over lines
      INTEGER			IPOS, NPOS		! String pointers
      INTEGER			LLEN			! Length of LINE
      INTEGER			NLINE 			! # lines to write
      INTEGER			NNUL			! # nulls in LINE
      INTEGER			PHDU			! Primary HDU
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No output from this method
      OARG = ADI__NULLID

*  Locate primary HDU
      CALL ADI2_FNDHDU( ARGS(1), 'PRIMARY', .TRUE., PHDU, STATUS )

*  Get number of lines
      CALL ADI_SIZE( ARGS(2), NLINE, STATUS )

*  Write lines one at a time
      DO ILINE = 1, NLINE

*    Locate ILINE'th ADI string and get its data
        CALL ADI_CELL( ARGS(2), 1, ILINE, CID, STATUS )
        CALL ADI_CLEN( CID, LLEN, STATUS )
        CALL ADI_GET0C( CID, LINE(:LLEN), STATUS )
        LLEN = MIN( LLEN, LEN(LINE) )
        CALL ADI_ERASE( CID, STATUS )

*    If the string contains nulls we need to split it into several
*    lines. Count the nulls and extend the text array
        NNUL = 0
        DO I = 1, LLEN
          IF ( LINE(I:I) .EQ. NUL ) NNUL = NNUL + 1
        END DO

*    Write each bit of LINE to a new HDS string
        IPOS = 1
        DO WHILE ( IPOS .LE. LLEN )

*      Find position of next null
          NPOS = INDEX( LINE(IPOS:LLEN), NUL )
          IF ( NPOS .EQ. 0 ) THEN
            NPOS = LLEN + 1
          ELSE
            NPOS = NPOS + IPOS - 1
          END IF

*      Write the HDS string
          CALL ADI2_HPHIS( PHDU, '  '//LINE(IPOS:NPOS-1), STATUS )

*      Next bit of line
          IPOS = NPOS + 1

        END DO

      END DO

*  Release the HDU
      CALL ADI_ERASE( PHDU, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HSI2_PTXT', STATUS )
      END IF

      END
