      SUBROUTINE IRH_PURGE( IDH1, IDH2, STATUS )
*+
*  Name:
*     IRH_PURGE

*  Purpose:
*     Purge duplicate entries from a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_PURGE( IDH1, IDH2, STATUS )

*  Description:
*     This routine creates a new group based on a given existing group.
*     The contents of the existing group are copied to the new group,
*     but any duplicated names are only included once. The check for
*     duplication is case insensitive. The new group inherits the title
*     of the old group.
*
*     Note, indices determined from the old group will in general not
*     point to the same name in the new group. The old group should be
*     annulled using IRH_ANNUL if it is no longer required.

*  Arguments:
*     IDH1 = INTEGER (Given)
*        The IRH identifier for an existing group.
*     IDH2 = INTEGER (Returned)
*        An IRH identifier for the created group. This group is a purged
*        form of the group identified by IDH1.
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

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_NMPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped NAMES array of each group.
*        HCM_MGPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped MOD_GROUP array of each group.
*        HCM_MIPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped MOD_INDEX array of each group.
*        HCM_LVPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped LEVEL array of each group.
*        HCM_FLPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped FILE array of each group.
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.
*        HCM_SIZE( IRH__MAXG ) = INTEGER (Read)
*           The size of the array components in each GROUP structure.

*  Arguments Given:
      INTEGER IDH1

*  Arguments Returned:
      INTEGER IDH2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TITLE*(IRH__SZNAM)! Title from input group.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the title of the input group.
      CALL IRH_GTTL( IDH1, TITLE, STATUS )

*  Create the new (initially empty) group.
      CALL IRH_NEW( TITLE, IDH2, STATUS )

*  Cause the group to be extended so that its size is the same as that
*  of the input group.
      CALL IRH1_PTELM( IDH2, HCM_GSIZE( IDH1 ), IRH__BLANK, 0, ' ', 0, 
     :                 0, STATUS )

*  Call IRH1_IPURG to do the work.  NB, the final argument specifies
*  the length of each character string in the mapped character arrays,
*  and is required by UNIX. There is no corresponding dummy argument in
*  the code for IRH1_IPURG.
      CALL IRH1_IPURG( HCM_SIZE( IDH1 ), %VAL( HCM_NMPNT( IDH1 ) ),
     :             %VAL( HCM_MGPNT( IDH1 ) ), %VAL( HCM_MIPNT( IDH1 ) ),
     :             %VAL( HCM_LVPNT( IDH1 ) ), %VAL( HCM_FLPNT( IDH1 ) ),
     :             HCM_GSIZE( IDH1 ),
     :             HCM_SIZE( IDH2 ), %VAL( HCM_NMPNT( IDH2 ) ),
     :             %VAL( HCM_MGPNT( IDH2 ) ), %VAL( HCM_MIPNT( IDH2 ) ),
     :             %VAL( HCM_LVPNT( IDH2 ) ), %VAL( HCM_FLPNT( IDH2 ) ),
     :             HCM_GSIZE( IDH2 ), STATUS ,
     :             %VAL( IRH__SZNAM ), %VAL( IRH__SZNAM ),
     :              %VAL( IRH__SZNAM ), %VAL( IRH__SZNAM ) )

*  If an error occured, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRH_PURGE_ERR2',
     :       'IRH_PURGE: Unable to purge a group of duplicate entries.',
     :          STATUS )
      END IF

      END
* $Id$
