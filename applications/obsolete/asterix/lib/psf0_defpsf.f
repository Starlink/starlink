      SUBROUTINE PSF0_DEFPSF( NAME, IRTN, STATUS )
*+
*  Name:
*     PSF0_DEFPSF

*  Purpose:
*     Define a new psf by supplying the initialisation routine

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF0_DEFPSF( NAME, IRTN, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NAME = CHARACTER*(*) (given)
*        Tag name of the psf
*     IRTN = EXTERNAL (given)
*        Routine to handle initialisations
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
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      1 May 1996 (DJA):
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
      INCLUDE 'PSF_CMN'

*  Arguments Given:
      CHARACTER*(*)		NAME
      EXTERNAL			IRTN

*  Status:
      INTEGER 			STATUS             	! Global status

*  External references:
      EXTERNAL			UTIL_PLOC
        INTEGER			UTIL_PLOC
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the address of the routine
      CALL ADI_CPUT0I( P_PLIST, NAME, UTIL_PLOC( IRTN ), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF0_DEFPSF', STATUS )
      END IF

      END
