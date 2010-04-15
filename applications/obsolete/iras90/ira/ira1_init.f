      BLOCK DATA IRA1_INIT
*+
*  Name:
*     IRA1_INIT

*  Purpose:
*     Initialise the IRA common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     All IRA identifiers are set un-used. The state of IRA is set to
*     STOPPED. Initial values for graphics options are set up.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-AUG-1992 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'IRA_PAR'          ! IRA public constants.
      INCLUDE 'PRM_PAR'          ! Starlink data constant.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.

*  Local Variables:
      INTEGER I                  ! Implicit loop count.

*  Global Data:
*  Initalise all IRA identifiers to invalid.
      DATA (ACM_VALID( I ), I = 1, IRA__MAX) / IRA__MAX*.FALSE. /

*  Initalise the state to STOPPED
      DATA ACM_STATE/'STOPPED'/

*  Initialise graphics options.
      DATA ACM_DROPT/ 1.25D-2, 1.25D-2, 6.0D0, 1.0D0, -1.0D0, -1.0D0,
     :                1.0D0, 1.0D0, -1.0D0, -1.0D0, 1.0D0, 1.0D0 /

*  Indicate that no curve has yet been drawn for which IRA_DRBRK can
*  return information.
      DATA ACM_LENG/ VAL__BADR /

*  Indicate that no value has yet been drawn for which IRA_DRVPO can
*  return information.
      DATA ACM_DRVPO/ 5*VAL__BADR /
*.

      END
