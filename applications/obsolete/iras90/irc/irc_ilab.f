      SUBROUTINE IRC_ILAB( LIST, STATUS )
*+
*  Name:
*     IRC_ILAB

*  Purpose:
*     Return a list of legal values for NDF component LABEL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_ILAB( LIST, STATUS )

*  Description:
*     A string is returned containing a list of names identifying the
*     legal values of the NDF component LABEL. The values are separated
*     by commas. The currently recognised values are:
*
*     1) "Survey CRDD"

*  Arguments:
*     LIST = CHARACTER * ( * ) (Returned)
*        The list of recognized values for NDF component LABEL. The
*        character variable supplied for this argument should have a
*        declared size equal to the value of parameter IRC__SZLLS. If
*        the supplied string is not long enough to hold all the names, a
*        warning message is given, but no error status is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors

*  Arguments Returned:
      CHARACTER LIST*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Return the list of currently recognised values of LABEL. The length
*  of this string is stored in parameter IRC__SZLLS which should be
*  updated when new values are added to the list.
      LIST = 'Survey CRDD'

*  If the list was truncated, give a warning message.
      IF( LEN( LIST ) .LT. IRC__SZLLS ) THEN
         CALL MSG_OUT( 'IRC_ILAB_MSG1', 'WARNING - List of valid'//
     :          'values for NDF component LABEL was truncated', STATUS )
      END IF

      END
