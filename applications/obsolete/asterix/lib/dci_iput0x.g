      SUBROUTINE DCI_IPUT0<T>( DETID, NAME, VALUE, STATUS )
*+
*  Name:
*     DCI_IPUT0<T>

*  Purpose:
*     Write scalar <COMM> instrument dependent variable to MissionStrings object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DCI_IPUT0<T>( DETID, NAME, VALUE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     DETID = INTEGER (given)
*        ADI identifier of mission strings object
*     NAME = CHARACTER*(*) (given)
*        Name of instrument specific parameter to be written
*     VALUE = <TYPE (given)
*        The value of the scalar instrument specific parameter
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
*     DCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/dci.html

*  Keywords:
*     package:dci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Dec 1995 (DJA):
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
      INTEGER			DETID
      CHARACTER*(*)		NAME
      <TYPE>			VALUE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			IID			! Instrument container
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate instrument container
      CALL ADI_FIND( DETID, 'InstrPars', IID, STATUS )
      CALL ADI_CPUT0<T>( IID, NAME, VALUE, STATUS )
      CALL ADI_ERASE( IID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI_IPUT0<T>', STATUS )

      END
