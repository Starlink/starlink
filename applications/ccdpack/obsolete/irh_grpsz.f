      SUBROUTINE IRH_GRPSZ( IDH, SIZE, STATUS )
*+
*  Name:
*     IRH_GRPSZ

*  Purpose:
*     Returns the number of names in a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_GRPSZ( IDH, SIZE, STATUS )

*  Description:
*     This routine returns the number of names in a group.

*  Arguments:
*     IDH = INTEGER (Given)
*        An IRH identifier for the group.
*     SIZE = INTEGER (Returned)
*        The number of names in the group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAY-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR reference. Added DAT_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRH_ERR'          ! IRH error values.

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read)
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Arguments Given:
      INTEGER IDH

*  Arguments Returned:
      INTEGER SIZE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the group identifier is not valid, report an error.
      IF( IDH .LT. 1 .OR. IDH .GT. IRH__MAXG ) THEN
         STATUS = IRH__INVID

      ELSE IF( .NOT. HCM_VALID( IDH ) ) THEN
         STATUS = IRH__INVID

      END IF

      IF( STATUS .EQ. IRH__INVID ) THEN
         CALL ERR_REP( 'IRH_GRPSZ_ERR1',
     :                 'IRH_GRPSZ: Invalid IRH identifier supplied',
     :                 STATUS )
      END IF

*  Extract the required value from common.
      SIZE = HCM_GSIZE( IDH )

      END
* $Id$
