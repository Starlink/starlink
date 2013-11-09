      SUBROUTINE ERI_FOLD( NENER, ESPEC, NCHAN, RMFID, ARFID,
     :                     CSPEC, STATUS )
*+
*  Name:
*     ERI_FOLD

*  Purpose:
*     Fold an energy spectrum through the instrument responses

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERI_FOLD( NENER, ESPEC, NCHAN, RMFID, ARFID, CSPEC, STATUS )

*  Description:
*     Folds the supplied energy spectrum through the energy response
*     supplied. The response is in the form of a redistribution matrix
*     and an area response. If the former is normalised then the second
*     object is required to convert to counts/cm**2.
*
*     For details of how different forms of response are handled see the
*     ERI_FOLDN routine.

*  Arguments:
*     NENER = INTEGER (given)
*        Number of energy space bins for output spectrum
*     ESPEC[] = REAL (given)
*        Input energy space spectrum
*     NCHAN = INTEGER (given)
*        Number of channels in output spectrum. Used to check against
*        response
*     RMFID = INTEGER (given)
*        ADI identifier of RedistributionMatrix object
*     ARFID = INTEGER (given)
*        ADI identifier of AreaResponse object
*     CSPEC[] = REAL (returned)
*        The channel space spectrum
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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eci, usage:public, energy response, folding

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     6 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			NENER			! Number of energy bins
      REAL			ESPEC(*)		! Energy spectrum
      INTEGER			RMFID			! Redistribution
      INTEGER			ARFID			! Area response
      INTEGER			NCHAN			! Number of channels

*  Arguments Returned:
      REAL			CSPEC(*)		! Channel spectrum

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke vector routine
      CALL ERI_FOLDN( NENER, 1, ESPEC, NCHAN, RMFID, ARFID,
     :                CSPEC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ERI_FOLD', STATUS )

      END
