      SUBROUTINE ADI2_FPKY<T>( FITID, HDU, KEY, VALUE, CMNT, STATUS )
*+
*  Name:
*     ADI2_FPKY<T>

*  Purpose:
*     Write value of keyword to specified HDU of specified file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FPKY<T>( FITID, HDU, KEY, VALUE, CMNT, STATUS )

*  Description:
*     Write value of keyword to specified HDU. Any existing keyword value
*     is overwritten.

*  Arguments:
*     FITID = INTEGER (given)
*        The ADI identifier of the FITS file
*     HDU = CHARACTER*(*) (given)
*        Name of HDU to write
*     KEY = CHARACTER*(*) (given)
*        The name of the keyword to be set
*     VALUE = <TYPE> (given)
*        The keyword value
*     CMNT = CHARACTER*(*) (given)
*        The keyword comment
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Sep 1995 (DJA):
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
      INTEGER			FITID
      <TYPE>			VALUE
      CHARACTER*(*)		HDU, KEY, CMNT

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			HDUID			! The HDU to write to
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the HDU
      CALL ADI2_FNDHDU( FITID, HDU, .TRUE., HDUID, STATUS )

*  Write the keyword
      CALL ADI2_HPKY<T>( HDUID, KEY, VALUE, CMNT, STATUS )

*  Release the HDU
      CALL ADI_ERASE( HDUID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FPKY<T>', STATUS )

      END
