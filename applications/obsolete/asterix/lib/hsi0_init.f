      SUBROUTINE HSI0_INIT( STATUS )
*+
*  Name:
*     HSI0_INIT

*  Purpose:
*     Load ADI definitions required for HSI operation

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI0_INIT( STATUS )

*  Description:
*     Loads those class definitions required by the HSI subroutine group.
*     Results in the following classes being defined,
*
*     Methods are defined to read and write HSI information from HDS and
*     FITS files.

*  Arguments:
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
*     ADI:
*        ADI_REQPKG - Load a package from the load path

*  Implementation Deficiencies:
*     {routine_deficiencies}...

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
*     9 Jan 1995 (DJA):
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
      INCLUDE 'HSI_CMN'					! HSI globals
*        HSI_INIT = LOGICAL (given and returned)
*           HSI definitions load attempted?

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			HSI1_ADD
      EXTERNAL			HSI1_COPY
      EXTERNAL			HSI1_NEW

      EXTERNAL			ADI_DEFMTH
      EXTERNAL			ADI_REQPKG

*  Local Variables:
      INTEGER			DID			! Unused method id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check not already initialised?
      IF ( .NOT. HSI_INIT ) THEN

*    Load the ADI classes
        CALL ADI_REQPKG( 'history', STATUS )

*    Define history copiers
        CALL ADI_DEFMTH( 'CopyHistory(HDSfile,HDSfile)', HSI1_COPY,
     :                   DID, STATUS )

*    Creators of new history
        CALL ADI_DEFMTH( 'AddHistory(HDSfile,CHAR)', HSI1_ADD,
     :                   DID, STATUS )
        CALL ADI_DEFMTH( 'NewHistory(HDSfile)', HSI1_NEW,
     :                   DID, STATUS )

*    Now initialised
	HSI_INIT = .TRUE.

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI0_INIT', STATUS )

      END
