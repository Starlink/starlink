      SUBROUTINE IRH1_IANNU( IDH, STATUS )
*+
*  Name:
*     IRH1_IANNU

*  Purpose:
*     Annul a valid IRH identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_IANNU( IDH, STATUS )

*  Description:
*     It is assumed that the supplied IRH identifier is valid and
*     currently in use. All references made to this group within the 
*     MOD_GROUP arrays of the other defined groups, are reset to zero.
*     All component of the associated GROUP structure are deleted. The  
*     identifier is flagged as no longer being in use, and an invalid 
*     group identifier is returned.
*
*     Note, this routine executes even if STATUS is bad on entry.

*  Arguments:
*     IDH = INTEGER (Given and Returned)
*        The IRH identifier to be annulled. Assumed valid on entry.
*        Set to IRH__NOID on exit.
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
*        HCM_LOCG( IRH__MAXG ) = CHARACTER (Read and Write)
*           HDS locators to each individual GROUP structure. 
*        HCM_LOCNM( IRH__MAXG ) = CHARACTER (Read and Write)
*           HDS locators to NAMES component of each group.
*        HCM_LOCMG( IRH__MAXG ) = CHARACTER (Read and Write)
*           HDS locators to MOD_GROUP component of each group.
*        HCM_LOCMI( IRH__MAXG ) = CHARACTER (Read and Write)
*           HDS locators to MOD_INDEX component of each group.
*        HCM_LOCLV( IRH__MAXG ) = CHARACTER (Read and Write)
*           HDS locators to LEVEL component of each group.
*        HCM_LOCFL( IRH__MAXG ) = CHARACTER (Read and Write)
*           HDS locators to FILE component of each group.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read and Write)
*           True if the corresponding group identifier is valid (i.e. in
*           use).
*        HCM_MGPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped MOD_GROUP array of each group.
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given and Returned:
      INTEGER IDH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
      INTEGER TSTAT              ! Saved input STATUS value.

*.

*  Save the input status value and initialise the local status value.
      TSTAT = STATUS
      STATUS = SAI__OK

*  Reset any references made to the group identified by IDH within
*  the MOD_GROUP array of each currently defined group. 
      DO I = 1, IRH__MAXG
         IF( I .NE. IDH .AND. HCM_VALID( I ) .AND. 
     :       HCM_GSIZE( I ) .GT. 0 ) THEN
            CALL IRH1_MGRES( IDH, HCM_GSIZE( I ), 
     :                       %VAL( HCM_MGPNT( I ) ), STATUS )
         END IF
      END DO

*  Erase the TITLE component from the GROUP structure.
      CALL DAT_ERASE( HCM_LOCG( IDH ), 'TITLE', STATUS )

*  Annul the locator to the NAMES component and then erase it. This
*  automatically unmaps the corresponding array.
      CALL DAT_ANNUL( HCM_LOCNM( IDH ), STATUS )
      CALL DAT_ERASE( HCM_LOCG( IDH ), 'NAMES', STATUS )

*  Annul the locator to the MOD_GROUP component and then erase it. This
*  automatically unmaps the corresponding array.
      CALL DAT_ANNUL( HCM_LOCMG( IDH ), STATUS )
      CALL DAT_ERASE( HCM_LOCG( IDH ), 'MOD_GROUP', STATUS )

*  Annul the locator to the MOD_INDEX component and then erase it. This
*  automatically unmaps the corresponding array.
      CALL DAT_ANNUL( HCM_LOCMI( IDH ), STATUS )
      CALL DAT_ERASE( HCM_LOCG( IDH ), 'MOD_INDEX', STATUS )

*  Annul the locator to the LEVEL component and then erase it. This
*  automatically unmaps the corresponding array.
      CALL DAT_ANNUL( HCM_LOCLV( IDH ), STATUS )
      CALL DAT_ERASE( HCM_LOCG( IDH ), 'LEVEL', STATUS )

*  Annul the locator to the FILE component and then erase it. This
*  automatically unmaps the corresponding array.
      CALL DAT_ANNUL( HCM_LOCFL( IDH ), STATUS )
      CALL DAT_ERASE( HCM_LOCG( IDH ), 'FILE', STATUS )

*  Annul the locator to the group structure.
      CALL DAT_ANNUL( HCM_LOCG( IDH ), STATUS )

*  Indicate that the group is no longer in use.
      HCM_VALID( IDH ) = .FALSE.

*  Set the group identifier invalid.
      IDH = IRH__NOID      

*  If status was bad on entry, restore its input value.
      IF( TSTAT .NE. SAI__OK ) STATUS = TSTAT

      END
* $Id$
