      SUBROUTINE IRH1_REMBL( IDH, STATUS )
*+
*  Name:
*     IRH1_REMBL

*  Purpose:
*     Reorder a group to remove unused elements.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_REMBL( IDH, STATUS )

*  Description:
*     Unused elements (i.e. elements holding the value given by 
*     IRH__BLANK) can be introduced into a group by calling
*     IRH1_ERELM.  This routine re-organises the contents of a group to
*     remove such unused elements. The group size stored in common
*     (HCM_GSIZE) is reduced by the number of unused elements removed.

*  Arguments:
*     IDH = INTEGER (Given)
*        The IRH identifier for the group.
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
*        Added trailing string lengths and DAT_PAR.
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

*  Arguments Given:
      INTEGER IDH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NEWSIZ             ! New group size.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call IRH1_IRMBL to do the work. NB, the final argument specifies the
*  length of the character strings in the mapped character arrays, and
*  is required by UNIX. There is no corresponding dummy argument in the
*  code for IRH1_IRMBL.
      IF( HCM_GSIZE( IDH ) .GT. 0 ) THEN
         CALL IRH1_IRMBL( HCM_GSIZE( IDH ), %VAL( HCM_NMPNT( IDH ) ),
     :               %VAL( HCM_MGPNT( IDH ) ), %VAL( HCM_MIPNT( IDH ) ),
     :               %VAL( HCM_LVPNT( IDH ) ), %VAL( HCM_FLPNT( IDH ) ),
     :               NEWSIZ, STATUS, %VAL( IRH__SZNAM ),
     :               %VAL( IRH__SZNAM ) )

*  Store the new group size.
         HCM_GSIZE( IDH ) = NEWSIZ

      END IF

      END

* $Id$
