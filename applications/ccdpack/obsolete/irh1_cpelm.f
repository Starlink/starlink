      SUBROUTINE IRH1_CPELM( IDH1, IDH2, INDXLO, INDXHI, STATUS )
*+
*  Name:
*     IRH1_CPELM

*  Purpose:
*     Copy a section of one group to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_CPELM( IDH1, IDH2, INDXLO, INDXHI, STATUS )

*  Description:
*     If the output group is smaller than the input group, it is
*     extended so that they are the same size. The information within
*     the index range INDXLO to INDXHI is copied from input to output.

*  Arguments:
*     IDH1 = INTEGER (Given)
*        The identifier for the input group.
*     IDH2 = INTEGER (Given)
*        The identifier for the output group.
*     INDXLO = INTEGER (Given)
*        The lowest index to be copied. Values less than 1 cause 1 to be
*        used.
*     INDXHI = INTEGER (Given)
*        The highest index to be copied. Values greater than the size of
*        the input group cause the size of the input group to be used.
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
*        Added trailing string length arguments and DAT_PAR.
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
*        HCM_LOCNM( IRH__MAXG ) = CHARACTER (Read)
*           HDS locators to NAMES component of each group.
*        HCM_LOCMG( IRH__MAXG ) = CHARACTER (Read)
*           HDS locators to MOD_GROUP component of each group.
*        HCM_LOCMI( IRH__MAXG ) = CHARACTER (Read)
*           HDS locators to MOD_INDEX component of each group.
*        HCM_LOCLV( IRH__MAXG ) = CHARACTER (Read)
*           HDS locators to LEVEL component of each group.
*        HCM_LOCFL( IRH__MAXG ) = CHARACTER (Read)
*           HDS locators to FILE component of each group.
*        HCM_NMPNT( IRH__MAXG ) = INTEGER (Read and Write)
*           Pointers to the mapped NAMES array of each group.
*        HCM_MGPNT( IRH__MAXG ) = INTEGER (Read and Write)
*           Pointers to the mapped MOD_GROUP array of each group.
*        HCM_MIPNT( IRH__MAXG ) = INTEGER (Read and Write)
*           Pointers to the mapped MOD_INDEX array of each group.
*        HCM_LVPNT( IRH__MAXG ) = INTEGER (Read and Write)
*           Pointers to the mapped LEVEL array of each group.
*        HCM_FLPNT( IRH__MAXG ) = INTEGER (Read and Write)
*           Pointers to the mapped FILE array of each group.
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.
*        HCM_SIZE( IRH__MAXG ) = INTEGER (Read and Write)
*           The size of the array components in each GROUP structure.

*  Arguments Given:
      INTEGER IDH1
      INTEGER IDH2
      INTEGER INDXLO
      INTEGER INDXHI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER OUTSIZ             ! Required size of output group.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the required final size for the output group.
      OUTSIZ = MAX( HCM_GSIZE( IDH2 ), MIN( HCM_GSIZE( IDH1), INDXHI ) )

*  If the output group is smaller than the its required size, then
*  extend it.
      IF( HCM_GSIZE( IDH2 ) .LT. OUTSIZ ) THEN
         CALL IRH1_PTELM( IDH2, OUTSIZ, IRH__BLANK, 0, ' ', 0, 0, 
     :                    STATUS )
      END IF

*  Copy the data. NB, the final arguments specify the length of each
*  character string in the mapped character arrays, and is required by
*  UNIX. There is no corresponding dummy argument in the code for
*  IRH1_ICPEL.
      IF( HCM_GSIZE( IDH1 ) .GT. 0 ) THEN
         CALL IRH1_ICPEL( HCM_GSIZE( IDH1 ), MAX( 1, INDXLO ),
     :                 MIN( HCM_GSIZE( IDH1 ), INDXHI ),
     :             %VAL( HCM_NMPNT( IDH1 ) ), %VAL( HCM_MGPNT( IDH1 ) ),
     :             %VAL( HCM_MIPNT( IDH1 ) ), %VAL( HCM_LVPNT( IDH1 ) ),
     :             %VAL( HCM_FLPNT( IDH1 ) ), %VAL( HCM_NMPNT( IDH2 ) ),
     :             %VAL( HCM_MGPNT( IDH2 ) ), %VAL( HCM_MIPNT( IDH2 ) ),
     :             %VAL( HCM_LVPNT( IDH2 ) ), %VAL( HCM_FLPNT( IDH2 ) ),
     :                 STATUS,
     :             %VAL( IRH__SZNAM ), %VAL( IRH__SZNAM ),
     :             %VAL( IRH__SZNAM ), %VAL( IRH__SZNAM ) )
      END IF

      END
* $Id$
