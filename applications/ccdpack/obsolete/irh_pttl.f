      SUBROUTINE IRH_PTTL( IDH, TITLE, STATUS )
*+
*  Name:
*     IRH_PTTL

*  Purpose:
*     Associated a new title with a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_PTTL( IDH, TITLE, STATUS )

*  Description:
*     The given title is associated with the group, replacing the one
*     which was specified when the group was created.

*  Arguments:
*     IDH = INTEGER (Given)
*        An IRH group identifier.
*     TITLE = CHARACTER (Given)
*        The group title.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JUN-1991 (DSB):
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
*        HCM_LOCG = CHARACTER (Read)
*           An HDS locator to the array of GROUP structures.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read)
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Arguments Given:
      INTEGER IDH
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving the used length of a
                                 ! string.

*  Local Variables:
      INTEGER TLEN               ! Used length of the supplied title.
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
         CALL ERR_REP( 'IRH_PTTL_ERR1',
     :                 'IRH_PTTL: Invalid group identifier supplied',
     :                 STATUS )
      END IF

*  Delete the old TITLE component.
      CALL DAT_ERASE( HCM_LOCG( IDH ), 'TITLE', STATUS )

*  Create a new TITLE component, and store the supplied title.
      TLEN = MAX( 1, CHR_LEN( TITLE ) )
      CALL DAT_NEW0C( HCM_LOCG( IDH ), 'TITLE', TLEN, STATUS )
      CALL CMP_PUT0C( HCM_LOCG( IDH ), 'TITLE', TITLE( : TLEN ),
     :                STATUS )

*  If an error occured, give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRH_PTTL_ERR2',
     :                 'IRH_PTTL: Unable to store a group title',
     :                 STATUS )
      END IF

      END
* $Id$
