      SUBROUTINE IRH_SECT( IDH1, INDXLO, INDXHI, REJECT, IDH2, STATUS )
*+
*  Name:
*     IRH_SECT

*  Purpose:
*     Produce a new group holding a subsection of an existing group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_SECT( IDH, INDXLO, INDXHI, REJECT, IDH2, STATUS )

*  Description:
*     A new group is created by copying a subsection of an existing
*     group. A range of indices are supplied in INDXLO and INDXHI. The
*     output group can be formed in one of two ways:
*
*     1) All names from the input group are copied to the output group
*     except for those with indices in the given range.
*
*     2) Only those names from the input group which have indices within
*     the given range are copied to the output group.
*
*     The method to use is determined by the argument REJECT. Note, a
*     name with a given index in the input group will in general have a
*     different index in the output group. The new group inherits the
*     title of the old group. If the input group is no longer required,
*     it should be annulled using IRH_ANNUL.

*  Arguments:
*     IDH1 = INTEGER (Given)
*        The IRH identifier for the input group.
*     INDXLO = INTEGER (Given)
*        The lowest index to reject or to copy.
*     INDXHI = INTEGER (Given)
*        The highest index to reject or to copy.
*     REJECT = LOGICAL ( Given)
*        If reject is true, then method 1) (see above) is used to
*        create the group (i.e. names in the given range are rejected).
*        Otherwise, method 2) is used (i.e. names in the given range are
*        copied).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-MAY-1990 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Added DAT_PAR.
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
      INTEGER IDH1
      INTEGER INDXLO
      INTEGER INDXHI
      LOGICAL REJECT

*  Arguments Returned:
      INTEGER IDH2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TITLE*(IRH__SZNAM)! Current index.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the title from the input group.
      CALL IRH_GTTL( IDH1, TITLE, STATUS )

*  Create a new (initially empty) group.
      CALL IRH_NEW( TITLE, IDH2, STATUS )

*  If the supplied range of names is to be rejected...
      IF( REJECT ) THEN

*  Copy all the names with indices below the low index limit.
         CALL IRH1_CPELM( IDH1, IDH2, 1, INDXLO - 1,  STATUS )

*  Copy all the names with indices above the high index limit. If the
*  limits overlap, copy all names.
         CALL IRH1_CPELM( IDH1, IDH2, MAX( INDXLO, INDXHI + 1 ), 
     :                    HCM_GSIZE( IDH1 ), STATUS )

*  If the supplied range of names is to be copied...
      ELSE

*  Copy them.
         CALL IRH1_CPELM( IDH1, IDH2, INDXLO, INDXHI, STATUS )

      END IF

*  Remove any blank entries from the output group.
      CALL IRH1_REMBL( IDH2, STATUS )      

*  If an error occured, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRH_SECT_ERR1',
     :   'IRH_SECT: Unable to create a group section.' , STATUS )
      END IF

      END
* $Id$
