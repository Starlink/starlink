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
      CHARACTER*200		FILE			! File path
      CHARACTER*15		PAR			! USI parameter
      CHARACTER*40		TYPE			! ADI class name

      INTEGER			CLEN, PLEN		! Lengths of strings
      INTEGER			FID			! File identifier

      LOGICAL			THERE			! Object exists?
*.

*  New error context
      CALL ERR_BEGIN( STATUS )

*  Get the file object
      CALL ADI_GETFILE( ID, FID, STATUS )

*  Duff identifier?
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        FILE = '<corrupt dataset identifier>'
        CLEN = 28

*  Memory based object?
      ELSE IF ( FID .EQ. ADI__NULLID ) THEN
        CALL MSG_SETI( 'CODE', ID )
        CALL ADI_TYPE( ID, TYPE, STATUS )
        CALL MSG_SETC( 'TYP', TYPE )
        CALL MSG_MAKE( '<memory object, class=^TYP, id=^CODE>', FILE,
     :                 CLEN )

*  Otherwise file object
      ELSE
        CALL ADI_FOBNAM( FID, FILE, CLEN, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          FILE = '<illegal file identifier>'
          CLEN = 25
        END IF

*    Is the file linked to a USI parameter
        CALL ADI_THERE( FID, '.USI_PAR', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CGET0C( FID, '.USI_PAR', PAR, STATUS )
          PLEN = CHR_LEN( PAR )
          FILE = FILE(:CLEN)//' (associated with parameter '/
     :                /PAR(:PLEN)//')'
          CLEN = CHR_LEN( FILE )
        END IF

      END IF

*  Release error stack
      CALL ERR_END( STATUS )

*  Set the token
      CALL MSG_SETC( TOKEN, FILE(:CLEN) )

      END
