      SUBROUTINE PSF0_GETAXID( PSID, XAX, YAX, EAX, TAX, STATUS )
*+
*  Name:
*     PSF0_GETAXID

*  Purpose:
*     Read axis identifiers from psf storage

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF0_GETAXID( PSID, XAX, YAX, EAX, TAX, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = (given)
*        ADI identifier of psf storage block
*     XAX = INTEGER (returned)
*        X axis number, or zero if none
*     YAX = INTEGER (returned)
*        Y axis number, or zero if none
*     EAX = INTEGER (returned)
*        Energy axis number, or zero if none
*     TAX = INTEGER (returned)
*        Time axis number, or zero if none
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
*      8 May 1996 (DJA):
*        Original version, adapted from C version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			PSID

*  Arguments Returned:
      INTEGER			XAX, YAX, EAX, TAX

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Retrieve identifiers
      CALL ADI_CGET0I( PSID, 'Xax', XAX, STATUS )
      CALL ADI_CGET0I( PSID, 'Yax', YAX, STATUS )
      CALL ADI_CGET0I( PSID, 'Eax', EAX, STATUS )
      CALL ADI_CGET0I( PSID, 'Tax', TAX, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF0_GETAXID', STATUS )
      END IF

      END
