      SUBROUTINE IRG1_CHECK( GID, VALID, STATUS )
*+
*  Name:
*     IRG1_CHECK

*  Purpose:
*     Check that a group was produced by IRG.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG1_CHECK( GID, VALID, STATUS)

*  Description:
*     An attempt is made to get the group title. If this fails because
*     the supplied group identifier is invalid, then an error is report
*     only if the VALID argument is false. If a title is succesfully
*     obtained, a check is made to see if it begins with the string
*     given by symbolic constant IRG__PREFX. If it doesn't an error is
*     reported. The access mode stored for the group is also checked. If
*     it illegal an error is reported.

*  Arguments:
*     GID = INTEGER (Given)
*        The group identifier to be checked.
*     VALID = LOGICAL (Given)
*        If false, then an error is report if GID does not identify a
*        valid IRH group. Otherwise, no error is reported if the group
*        is invalid. Note, an error is always reported if a valid group
*        has an title which does not begin with "IRG: ".
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JUN-1991 (DSB):
*        Original version.
*     31-JAN-1992 (DSB):
*        Check on access mode added.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRG_PAR'          ! IRG constants.
      INCLUDE 'IRG_ERR'          ! IRG status values.
      INCLUDE 'IRH_ERR'          ! IRH status values.
      INCLUDE 'IRH_PAR'          ! IRH constants.

*  Global Variables:
      INCLUDE 'IRG_COM'          ! IRG common blocks.
*        GCM_AMODE( IRH__MAXG ) = CHARACTER (Read and Write)
*           Access mode (READ, WRITE or UPDATE) for each group. 
*           the corresponding GROUP strcuture.

*  Arguments Given:
      INTEGER GID
      LOGICAL VALID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TITLE*10         ! First 10 characters of group title.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark the error stack.
      CALL ERR_MARK

*  Attempt to get the title from the group.
      CALL IRH_GTTL( GID, TITLE, STATUS )      

*  If the identifier was invalid, but the VALID argument indicates that
*  invalid groups are OK, annul the error.
      IF( STATUS .EQ. IRH__INVID ) THEN
         IF( .NOT. VALID ) CALL ERR_ANNUL( STATUS )

*  If the identifier was valid, check that the title begins with the
*  string given by symbolic constant IRG__PREFX. If it doesn't, report
*  an error.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         IF( INDEX( TITLE, IRG__PREFX) .NE. 1 ) THEN
            STATUS = IRG__NOIRG
            CALL ERR_REP( 'IRG1_CHECK_ERR1',
     :                    'Supplied group was not created by IRG',
     :                    STATUS )

*  Also check that the stored access mode is legal. If not, an error is
*  reported.
         ELSE
            CALL IRG1_VMODE( GCM_AMODE( GID ), GCM_AMODE( GID ),
     :                       STATUS )

         END IF
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
* $Id$
