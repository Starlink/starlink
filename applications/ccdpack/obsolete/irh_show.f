      SUBROUTINE IRH_SHOW( IDH, INDXLO, INDXHI, STATUS )
*+
*  Name:
*     IRH_SHOW

*  Purpose:
*     List names in a group subsection on the terminal.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_SHOW( IDH, INDXLO, INDXHI, STATUS )

*  Description:
*     All the valid names in the specified group subsection are
*     displayed on the terminal screen or in the batch job log file.
*     One name is displayed per line with 5 spaces at the start of each
*     line.

*  Arguments:
*     IDH = INTEGER (Given)
*        The IRH identifier for the group to be listed.
*     INDXLO = INTEGER (Given)
*        The low index limit of the group subsection. Values less than
*        one cause the value one to be used instead.
*     INDXHI = INTEGER (Given)
*        The high index limit of the group subsection. Values greater
*        than the size of the group cause a value equal to the size of
*        the group to be used instead.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAY-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR, added trailing string lengths and DAT_PAR.
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
*        HCM_NMPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped NAMES array of each group.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read)
*           True if the corresponding group identifier is valid (i.e. in
*           use).
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER IDH
      INTEGER INDXLO
      INTEGER INDXHI

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the IRH identifier is not valid, report an error.
      IF( IDH .LT. 1 .OR. IDH .GT. IRH__MAXG ) THEN
         STATUS = IRH__INVID

      ELSE IF( .NOT. HCM_VALID( IDH ) ) THEN
         STATUS = IRH__INVID

      END IF

      IF( STATUS .EQ. IRH__INVID ) THEN
         CALL ERR_REP( 'IRH_SHOW_ERR1',
     :                 'IRH_SHOW: Invalid group identifier supplied',
     :                 STATUS )
      END IF

*  Call IRH1_ISHOW to list the names. NB, the final argument specifies
*  the length of each character string in the mapped NAMES array, and
*  is required by UNIX. There is no corresponding dummy argument in the
*  code for IRH1_ISHOW.
      IF( HCM_GSIZE( IDH ) .GT. 0 ) THEN
         CALL IRH1_ISHOW( HCM_GSIZE( IDH ), %VAL( HCM_NMPNT( IDH ) ),
     :                 INDXLO, INDXHI, STATUS, %VAL( IRH__SZNAM ) )

      ELSE
         CALL MSG_OUT( 'IRH_SHOW_MSG1', '     Group is empty', STATUS )

      END IF

*  If an error occured, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRH_SHOW_ERR1',
     :   'IRH_SHOW: Unable to list contents of a group', STATUS )
      END IF

      END
* $Id$
