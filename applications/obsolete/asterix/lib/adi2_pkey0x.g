      SUBROUTINE ADI2_PKEY0<T>( FID, HDU, KEY, VALUE, COMNT, STATUS )
*+
*  Name:
*     ADI2_PKEY0<T>

*  Purpose:
*     Write a keyword value into the buffer structures

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_PKEY0<T>( FID, HDU, KEY, VALUE, COMNT, STATUS )

*  Description:
*     Write a keyword to the ADI object representing a FITS file. This
*     routine will write both the value and the comment, but if the comment
*     has value '*' then the keyword is a standard keyword and a standard
*     comment will be invented (and not stored in the buffer).

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARACTER*(*) (given)
*        Logical HDU whose keyword this is. Blank for primary
*     KEY = CHARACTER*(*) (given)
*        Name of the keyword. Same as the FITS keyword name, so should be
*        less than 9 characters in length
*     VALUE = <TYPE> (given)
*        The value for the keyword
*     COMNT = CHARACTER*(*) (given)
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

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
      <TYPE>			VALUE			! Value of the keyword
      CHARACTER*(*)		COMNT			! Keyword comment

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			HID			! HDU identifier
      INTEGER			KID			! Keyword identifier

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the HDU structure to contain the keyword
      CALL ADI2_LOCHDU( FID, HDU, HID, STATUS )

*  Does the keyword already exist?
      CALL ADI_THERE( HID, KEY, THERE, STATUS )
      IF ( THERE ) THEN

*    Do some checks here? Committed to disk? Same value etc?

      END IF

*  Write the value
      CALL ADI_CPUT0<T>( HID, KEY, VALUE, STATUS )

*  Comment is non-standard? If so write the value as a property
      IF ( COMNT(1:1) .NE. '*' ) THEN
        CALL ADI_FIND( HID, KEY, KID, STATUS )
        CALL ADI_CPUT0C( KID, '.COMMENT', COMNT, STATUS )
        CALL ADI_ERASE( KID, STATUS )
      END IF

*  Free the identifier
      CALL ADI_ERASE( HID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK )
     :        CALL AST_REXIT( 'ADI2_PKEY0<T>', STATUS )

      END
