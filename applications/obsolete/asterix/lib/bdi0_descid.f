      SUBROUTINE BDI0_DESCID( ID, TOKEN, STATUS )
*+
*  Name:
*     BDI0_DESCID

*  Purpose:
*     Set the named token to a text description of the object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_DESCID( ID, TOKEN, STATUS )

*  Description:
*     The primary use of this routine is in internal BDI error reporting.
*     The routine guarantees to produce a text description of the input
*     object and will not return bad status.

*  Arguments:
*     ID = INTEGER (given)
*        The ADI identifier of the object to be described
*     TOKEN = CHARACTER*(*) (given)
*        The name of the message system token to set
*     STATUS = INTEGER (given)
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     13 Dec 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		TOKEN

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*14		CSTR			! Char version of ID
      CHARACTER*200		FILE			! File path
      CHARACTER*40		TYPE			! ADI class name

      INTEGER			CLEN, FLEN		! Lengths of strings
      INTEGER			FID			! File identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the file object
      CALL ADI_GETFILE( ID, FID, STATUS )

*  Duff identifier?
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL MSG_SETC( TOKEN, '<corrupt dataset identifier>' )

*  Memory based object?
      ELSE IF ( FID .EQ. ADI__NULLID ) THEN
        CALL CHR_ITOC( ID, CSTR, CLEN )
        CALL ADI_TYPE( ID, TYPE, STATUS )
        CALL MSG_SETC( TOKEN, '<memory object, class='/
     :            /TYPE(:CHR_LEN(TYPE))//' id='//CSTR(:CLEN)//'>' )

*  Otherwise file object
      ELSE
        CALL ADI_FOBNAM( FID, FILE, CLEN, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL MSG_SETC( TOKEN, '<illegal file identifier>' )
        ELSE
          CALL MSG_SETC( TOKEN, FILE(:CLEN) )
        END IF

      END IF

      END
