      LOGICAL FUNCTION MSG1_GREF( PARAM, REFSTR, REFLEN )
*+
*  Name:
*     MSG1_GREF

*  Purpose:
*     Get the reference for the specified parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = MSG1_GREF( PARAM, REFSTR, REFLEN )

*  Description:
*     This routine makes an enquiry of the parameter system to
*     get the absolute object, device or file name (i.e. reference)
*     associated with the specified parameter.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     REFSTR = CHARACTER * ( * ) (Returned)
*        The reference.
*     REFLEN = INTEGER (Returned)
*        The length of the reference.

*  Implementation Notes:
*     -  This function is only for use in the ADAM version of MSG_.
*     -  This function makes calls to SUBPAR_FINDPAR and SUBPAR_GREF

*  Algorithm:
*     SUBPAR_GREF attempts to get a name via a valid locator from the parameter
*     system.
*     -  If this fails and the parameter type indicates a name, it gets
*     the name from the parameter table.

*  Authors:
*     AJC: Alan Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-DEC-2002 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      CHARACTER * ( * ) REFSTR
      INTEGER REFLEN
      
*  External References:
      LOGICAL SUBPAR_GREF

*  Local Variables:
      INTEGER STATUS                    ! Local status
      INTEGER NAMECODE                  ! Parameter namecode
*.

*  Initialise the returned value of MSG1_GREF.
      MSG1_GREF = .FALSE.

*  Initialise the returned string.
      REFSTR =  ' '
      REFLEN = 1

*  Initialise the local status.
      STATUS = SAI__OK

*  Set new error reporting context
      CALL EMS_MARK

*  Get the parameteer namecode and then the reference
      CALL SUBPAR_FINDPAR( PARAM, NAMECODE, STATUS )
      MSG1_GREF = SUBPAR_GREF( NAMECODE, REFSTR, REFLEN )
      
*  Annul any error reports and release the error context
      IF( STATUS .NE. SAI__OK ) CALL EMS_ANNUL( STATUS )
      CALL EMS_RLSE

      END
