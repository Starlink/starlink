      SUBROUTINE IRH1_ERELM( IDH, INDEX, STATUS )
*+
*  Name:
*     IRH1_ERELM

*  Purpose:
*     Erase an element from a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_ERELM( IDH, INDEX, STATUS )

*  Description:
*     This routine erases the specified element from the specified 
*     group by replacing the NAMES array entry with a string which
*     indicates that the entry is unused (IRH__BLANK), and setting the
*     supplementary information to their null values. The group size
*     stored in common (HCM_GSIZE) is not altered. 
*
*     Routine IRH1_REMBL should be called when all necessary group
*     elements have been erased. This will shuffle the group contents
*     down to fill the gaps left by calling IRH1_ERELM, and will update
*     the group size stored in common.

*  Arguments:
*     IDH = INTEGER (Given)
*        The IRH identifier for the group.
*     INDEX = INTEGER (Given)
*        The index within the group of the element to be erased.  If
*        this is negative or zero, or greater than the current size of
*        the group, the routine returns without action.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Added DAT_PAR
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
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER IDH
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If INDEX is zero or negative, or greater than the
*  current size of the group, return without action.
      IF( INDEX .LE. 0 .OR. INDEX .GT. HCM_GSIZE( IDH ) ) RETURN

*  Store null information at the given index.
      CALL IRH1_PTELM( IDH, INDEX, IRH__BLANK, 0, ' ', 0, 0, STATUS )

      END
* $Id$
