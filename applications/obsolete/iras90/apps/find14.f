      SUBROUTINE FIND14( STATUS )
*+
*  Name:
*     FIND14

*  Purpose:
*     Tidying up of initialised systems etc.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND14( STATUS )

*  Description:
*     Tidying up of initialised systems etc.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     IRA:
*        IRA_CLOSE
*     FIO:
*        FIO_STOP

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     18-JUN-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Close the IRAS Astrometry routines
      CALL IRA_CLOSE( STATUS )

*  Close FIO
      CALL FIO_STOP( STATUS )

      END
