
      SUBROUTINE IRH_CLOSE( STATUS )
*+
*  Name:
*     IRH_CLOSE

*  Purpose:
*     Close down the IRH package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_CLOSE( STATUS )

*  Description:
*     All internal resources used by any groups currently in use are
*     released. 
*
*     This routine attempts to execute even if STATUS is bad on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.

*  Arguments:
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

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_LOC = CHARACTER (Read and Write)
*           An HDS locator to the array of GROUP structures.
*        HCM_STATE = CHARACTER (Write)
*           If HCM_STATE = IRH__GOING then IRH has been initialised.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read)
*           True if the corresponding IRH identifier is valid (i.e. in
*           use).


*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
      INTEGER IDH                ! Group identifier.
      INTEGER TSTAT              ! Saved input STATUS value.
*.

*  Return immediately if IRH has not been initialised.
      IF( HCM_STATE .NE. IRH__GOING ) RETURN
 
*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK
 
*  Annul each IRH identifier currently in use. Note, IRH1_IANNU 
*  overwrite the supplied IRH identifier with the value IRH__NOID.
      STATUS = SAI__OK

      DO I = 1, IRH__MAXG
         IDH = I
         IF( HCM_VALID( IDH ) ) CALL IRH1_IANNU( IDH, STATUS )
      END DO

*  Delete the array of GROUP structures.
      CALL AIF_ANTMP( HCM_LOC, STATUS )
 
*  Indicate that IRH is no longer ready for use.
      HCM_STATE = ' '

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand. Release the error stack.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT
         ELSE
            CALL ERR_REP( 'IRH_CLOSE_ERR1',
     :                 'IRH_CLOSE: Error closing down the IRH package',
     :                  STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF
      CALL ERR_RLSE      

      END
* $Id$
