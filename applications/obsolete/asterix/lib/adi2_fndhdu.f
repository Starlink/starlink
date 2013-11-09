      SUBROUTINE ADI2_FNDHDU( ID, HDU, CREATE, HDUID, STATUS )
*+
*  Name:
*     ADI2_FNDHDU

*  Purpose:
*     Locate an HDU in a FITSfile, creating if required

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FNDHDU( ID, HDU, CREATE, HDUID, STATUS )

*  Description:
*     Locate an HDU in a FITSfile. It is an error if the HDU does not
*     exist. HDUs are located by name, defined by the EXTNAME keyword.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARACTER*(*) (given)
*        Name of the HDU we're loooking for
*     CREATE = LOGICAL (given)
*        Create if not present?
*     HDUID = INTEGER (returned)
*        ADI identifier of HDU cache object
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
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		HDU
      LOGICAL			CREATE

*  Arguments Returned:
      INTEGER			HDUID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      LOGICAL			DIDCRE			! Did we create HDU?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the named HDU
      CALL ADI2_CFIND( ID, HDU, ' ', ' ', CREATE, .FALSE.,
     :                 ' ', 0, 0, DIDCRE, HDUID, STATUS )

*  Issue error if not there
      IF ( STATUS .EQ. SAI__OK ) THEN

*    If there, check not marked for delete
        IF ( HDUID .NE. ADI__NULLID ) THEN
          CALL ADI2_CHKDEL( HDUID, STATUS )

*    otherwise error
        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'H', HDU )
          CALL ERR_REP( ' ', 'Unable to locate HDU ^H in file',
     :                  STATUS )
        END IF
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FNDHDU', STATUS )

      END
