      SUBROUTINE IRH_SETSZ( IDH, SIZE, STATUS )
*+
*  Name:
*     IRH_SETSZ

*  Purpose:
*     Reduce the size of a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_SETSZ( IDH, SIZE, STATUS )

*  Description:
*     This routine sets the size of the given group to the specified 
*     value. The new size must be less than or equal to the old size.
*     The names with indices greater than the new size are lost. Other
*     names remain unaltered.

*  Arguments:
*     IDH = INTEGER (Given)
*        An IRH identifier for the group.
*     SIZE = INTEGER (Given)
*        The new group size.
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
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.
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
         CALL ERR_REP( 'IRH_SETSZ_ERR1',
     :                 'IRH_SETSZ: Invalid IRH identifier supplied',
     :                 STATUS )
      END IF

*  If the new size is greater than the old size, report an error.
      IF( SIZE .GT. HCM_GSIZE( IDH ) ) THEN
         STATUS = IRH__SZINC
         CALL ERR_REP( 'IRH_SETSZ_ERR2',
     : 'IRH_SETSZ: Attempt made to increase the size of a group', 
     :                  STATUS )

*  Otherwise store the new size.
      ELSE
         HCM_GSIZE( IDH ) = SIZE

      END IF

      END
* $Id$
