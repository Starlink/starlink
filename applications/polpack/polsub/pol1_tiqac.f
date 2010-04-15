      SUBROUTINE POL1_TIQAC( GI, ATTRIB, VALUE, STATUS )
*+
*  Name:
*     POL1_TIQAC

*  Purpose:
*     Get the value of an attribute of a CAT component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_TIQAC( GI, ATTRIB, VALUE, STATUS )

*  Description:
*     This routine returns the value of a single attribute for a
*     CAT component. This is a wrapper around CAT_TIQAC to handle cases
*     where the attribute may not exist.

*  Arguments:
*     GI = INTEGER (Given)
*        Generic component identifier.
*     ATTRIB = CHARACTER * ( * ) (Given)
*        Name of the attribute of the component.
*     VALUE = CHARACTER * ( * ) (Given and Returned)
*        Value of the named attribute. If the named attribute does not
*        exist, the supplied value is returned unchanged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*    - No error is reported if the named attribute does not exist.
*    - If an error occurs, the original VALUE is returned unchanged.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-FEB-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_ERR'          ! CAT error constants

*  Arguments Given:
      INTEGER GI
      CHARACTER ATTRIB*(*)

*  Arguments Returned:
      CHARACTER VALUE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER OVAL*255         ! Orignal value string
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the supplied VALUE.
      OVAL = VALUE

*  Attempt to get a value for the attribute.
      CALL CAT_TIQAC( GI, ATTRIB, VALUE, STATUS )

*  If an error has occurred, re-instate the original VALUE>
      IF( STATUS .NE. SAI__OK ) VALUE = OVAL

*  If the attribute does not exist, annul the error.
      IF( STATUS .EQ. CAT__NOATT .OR.
     :    STATUS .EQ. CAT__NOCMP .OR.
     :    STATUS .EQ. CAT__INVPR ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

      END
