      SUBROUTINE ADI2_HGKYI<T>( HDUID, KEY, INDEX, VALUE, STATUS )
*+
*  Name:
*     ADI2_HGKYI<T>

*  Purpose:
*     Get value of indexed keyword from specified HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_HGKYI<T>( HDUID, KEY, INDEX, VALUE, STATUS )

*  Description:
*     Get value of keyword from specified HDU, where the keyword consists
*     of a root name ROOT and character coded integer INDEX. It is an error
*     for the keyword not to exist.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     KEY = CHARACTER*(*) (given)
*        The name of the keyword to be extracted
*     INDEX = INTEGER (given)
*        The index of the keyword
*     VALUE = <TYPE> (returned)
*        The keyword value
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
*     22 Feb 1996 (DJA):
*        Changed string concatenation to work on Linux
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			HDUID, INDEX
      CHARACTER*(*)		KEY

*  Arguments Returned:
      <TYPE>			VALUE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*72		CMNT			! Keyword comment
      CHARACTER*8		LKEY			! Local keyword name
      CHARACTER*7		STR			! Index encoded

      INTEGER			NDIG			! Chars used in STR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Encode the integer
      CALL CHR_ITOC( INDEX, STR, NDIG )
      LKEY = KEY(:LEN(KEY))//STR(:NDIG)

*  Get keyword value
      CALL ADI2_HGKY<T>( HDUID, LKEY(:NDIG+LEN(KEY)), VALUE, CMNT,
     :                   STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_HGKYI<T>', STATUS )

      END
