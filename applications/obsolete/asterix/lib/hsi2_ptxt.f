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

*  External Routines Used:

*  References:
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/hsi.html

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
      INCLUDE 'HSI_PAR'

*  Arguments Given:
      INTEGER			NARG, ARGS(*)

*  Arguments Returned:
      INTEGER			OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*132		LINE			! Line of text

      INTEGER			CID			! String array slice
      INTEGER			ILINE 			! Loop over lines
      INTEGER			IVERB			! Verbosity
      INTEGER			NLINE 			! # lines to write
      INTEGER			PHDU			! Main HDU
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No output from this method
      OARG = ADI__NULLID

*  Locate main HDU
      CALL ADI2_FNDHDU( ARGS(1), ' ', PHDU, STATUS )

*  Get verbosity
      IVERB = HSI__NORMAL

*  Only write text if verbosity is NORMAL or greater
      IF ( IVERB .GE. HSI__NORMAL ) THEN

*    Get number of lines
        CALL ADI_SIZE( ARGS(2), NLINE, STATUS )

*    Write lines one at a time
        DO ILINE = 1, NLINE

*      Locate ILINE'th ADI string
          CALL ADI_CELL( ARGS(2), 1, ILINE, CID, STATUS )

*      Extract the string and write it
          CALL ADI_GET0C( CID, LINE, STATUS )
          CALL ADI2_ADDHIS( PHDU, '  '//LINE(:MAX(1,CHR_LEN(LINE))),
     :                      .TRUE., STATUS )

*      Release the sliced object
          CALL ADI_ERASE( CID, STATUS )

        END DO

*  End of verbosity test
      END IF

*  Rerlease the HDU
      CALL ADI_ERASE( PHDU, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HSI2_PTXT', STATUS )
      END IF

      END
