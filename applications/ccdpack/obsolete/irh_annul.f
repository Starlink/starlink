      SUBROUTINE IRH_ANNUL( IDH, STATUS )
*+
*  Name:
*     IRH_ANNUL

*  Purpose:
*     Release a group from the IRH system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_ANNUL( IDH, STATUS )

*  Description:
*     All internal resources used by the group are released. Any
*     parameter association used to establish the group is NOT
*     cancelled.  
*
*     This routine attempts to execute even if STATUS is bad on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.

*  Arguments:
*     IDH = INTEGER (Given)
*        An IRH identifier for the group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAY-1991 (DSB):
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
*           True if the corresponding IRH identifier is valid (i.e. in
*           use).

*  Arguments Given:
      INTEGER IDH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Saved input STATUS value.

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK
 
*  Clear the local status value.
      STATUS = SAI__OK
      
*  If the IRH identifier is not valid, report an error.
      IF( IDH .LT. 1 .OR. IDH .GT. IRH__MAXG ) THEN
         STATUS = IRH__INVID

      ELSE IF( .NOT. HCM_VALID( IDH ) ) THEN
         STATUS = IRH__INVID

      END IF

      IF( STATUS .EQ. IRH__INVID ) THEN
         CALL ERR_REP( 'IRH_ANNUL_ERR1',
     :                 'IRH_ANNUL: Invalid IRH identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Call IRH1_IANNU to annul the group.
      CALL IRH1_IANNU( IDH, STATUS )
 
*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand, but add a context message. Release the error
*  stack.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

         ELSE
            CALL ERR_REP( 'IRH_ANNUL_ERR2',
     :            'IRH_ANNUL: Unable to annul a group', STATUS )

         END IF

      ELSE
         STATUS = TSTAT

      END IF

      CALL ERR_RLSE      

      END
* $Id$
