      SUBROUTINE IRH_GTTL( IDH, TITLE, STATUS )
*+
*  Name:
*     IRH_GTTL

*  Purpose:
*     Retrieve the title associated with a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_GTTL( IDH, TITLE, STATUS )

*  Description:
*     The title specified when the group was created is retrieved.

*  Arguments:
*     IDH = INTEGER (Given)
*        An IRH group identifier.
*     TITLE = CHARACTER (Returned)
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
      INCLUDE 'DAT_ERR'          ! HDS error values.
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRH_ERR'          ! IRH error values.

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_LOCG( IRH__MAXG ) = CHARACTER (Read)
*           HDS locators to each individual GROUP structure. 
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read)
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Arguments Given:
      INTEGER IDH

*  Arguments Returned:
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the group identifier is not valid, report an error.
      IF( IDH .LT. 1 .OR. IDH .GT. IRH__MAXG ) THEN
         STATUS = IRH__INVID

      ELSE IF ( IDH .GT. 0 ) THEN 
         IF( .NOT. HCM_VALID( IDH ) ) THEN
            STATUS = IRH__INVID
         END IF
      END IF

      IF( STATUS .EQ. IRH__INVID ) THEN
         CALL ERR_REP( 'IRH_GTTL_ERR1',
     :                 'IRH_GTTL: Invalid group identifier supplied',
     :                 STATUS )
      END IF

*  Get the TITLE component from the groups GROUP structure.
      IF ( IDH .GT. 0 ) THEN 
         CALL CMP_GET0C( HCM_LOCG( IDH ), 'TITLE', TITLE, STATUS )

*  If the title had to be truncated to fit it into the supplied 
*  character variable, then annul the error.
         IF( STATUS .EQ. DAT__TRUNC ) CALL ERR_ANNUL( STATUS )
      ELSE
*  Supply a null title.
         TITLE = ' '
      END IF

*  If an error occured, give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRH_GTTL_ERR2',
     :                 'IRH_GTTL: Unable to get a group title',
     :                 STATUS )
      END IF

      END
* $Id$
