      SUBROUTINE ADI2_GKEY0<T>( FID, HDU, KEY, MARK, GETCOM, VALUE,
     :                          COMNT, STATUS )
*+
*  Name:
*     ADI2_GKEY0<T>

*  Purpose:
*     Get a keyword value from the buffer structures

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_GKEY0<T>( FID, HDU, KEY, MARK, GETCOM, VALUE, COMNT, STATUS )

*  Description:
*     Get value of a keyword to be written to a FITS file object from its
*     keyword buffer, and optionally mark it as committed to disk. The
*     routine returns the keyword value and optionally the comment.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARACTER*(*) (given)
*        Logical HDU whose keyword this is. Blank for primary
*     KEY = CHARACTER*(*) (given)
*        Name of the keyword. Same as the FITS keyword name, so should be
*        less than 9 characters in length
*     MARK = LOGICAL (given)
*        Mark the keyword as committed to disk
*     GETCOM = LOGICAL (given)
*        Get the keyword comment value?
*     VALUE = <TYPE> (returned)
*        The value for the keyword
*     COMNT = CHARACTER*(*) (returned)
*        The keyword comment, returned if GETCOM is true
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
*     2 Feb 1995 (DJA):
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
      INTEGER			FID			! File identifier
      CHARACTER*(*)		HDU			! HDU name
      CHARACTER*(*)		KEY			! Keyword name
      LOGICAL			MARK			! Mark as committed
      LOGICAL			GETCOM			! Retrieve comment?

*  Arguments Returned:
      <TYPE>			VALUE			! Value of the keyword
      CHARACTER*(*)		COMNT			! Keyword comment

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*72		CMNT2

      INTEGER			HID			! HDU identifier

      LOGICAL			DIDCRE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the HDU structure that contains the keyword
      CALL ADI2_CFIND( FID, HDU, ' ', ' ', .FALSE., .FALSE.,
     :                 '<HTYPE>', 0, 0, DIDCRE, HID, STATUS )

* Read the value and comment from the keyword
      CALL ADI2_HGKY<T>( HID, KEY, VALUE, CMNT2, STATUS)

      IF ( GETCOM ) COMNT = CMNT2

*  Report any errors
      IF ( STATUS .NE. SAI__OK )
     :        CALL AST_REXIT( 'ADI2_GKEY0<T>', STATUS )

      END
