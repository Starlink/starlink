      SUBROUTINE HSI_PTXT( IFID, NLINE, TEXT, STATUS )
*+
*  Name:
*     HSI_PTXT

*  Purpose:
*     Write text lines to current history record

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI_PTXT( IFID, NLINE, TEXT, STATUS )

*  Description:
*     Write text lines to current history record

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier of the dataset
*     NLINE = INTEGER (given)
*        Number of lines to of text to write
*     TEXT[] = CHARACTER*(*) (given)
*        The array of text strings to write
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
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:public, history, creation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'HSI_CMN'                                 ! HSI common block
*       HSI_INIT = LOGICAL (given)
*         HSI class definitions loaded?

*  Arguments Given:
      INTEGER			IFID			! Dataset identifier
      INTEGER			NLINE			! # text lines
      CHARACTER*(*)		TEXT(*)			! Lines of text

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			IARG(2)			! Method inputs
      INTEGER			OARG			! Method output

      LOGICAL			TEMPOK			! Temp string created?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check lines to output!
      IF ( NLINE .LT. 1 ) RETURN

*  Check initialised
      IF ( .NOT. HSI_INIT ) CALL HSI0_INIT( STATUS )

*  Store first argument
      IARG(1) = IFID

*  Temporary string for command name
      CALL ADI_NEWV1C( NLINE, TEXT, IARG(2), STATUS )
      TEMPOK = (STATUS.EQ.SAI__OK)

*  Invoke the method
      CALL ADI_EXEC( 'AddHistoryText', 2, IARG, OARG, STATUS )

*  Scrub temporary string
      IF ( TEMPOK ) THEN

*    New error context in case addition of history failed
        CALL ERR_BEGIN( STATUS )

*    Scrub the string
        CALL ADI_ERASE( IARG(2), STATUS )

*    Restore context
        CALL ERR_END( STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI_PTXT', STATUS )

      END
