      SUBROUTINE IRH1_PTELM( IDH, INDEX, NAME, LEVEL, FILE, MODGP,
     :                       MODIN, STATUS )
*+
*  Name:
*     IRH1_PTELM

*  Purpose:
*     Put an element into a group, including supplementary information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_PTELM( IDH, INDEX, NAME, LEVEL, FILE, MODGP, MODIN,
*                      STATUS )

*  Description:
*     This routine stores the given text string in an element of the
*     NAMES array associated with the group identified by IDH. The
*     supplementary information is stored in the corresponding elements
*     of the LEVEL, FILE, MOD_GROUP and MOD_INDEX arrays.  These arrays
*     are stored in the GROUP structure created by routine IRH_GTIDH.
*     The calling routine can specify an explicit index at which the
*     element is to be stored in the group, or alternatively, if INDEX
*     is negative or zero (or greater than the current group size), the
*     element is appended to the end of the group.  If the index is
*     greater than the size of the GROUP structure arrays, the arrays
*     are extended to accomodate it.

*  Arguments:
*     IDH = INTEGER (Given)
*        The IRH identifier for the group to which the new name is to be
*        added. No check is performed on the validity of this value.
*     INDEX = INTEGER (Given)
*        The index within the group at which the element is to be
*        stored. If this is negative or zero, the element is appended
*        to the end of the group. If INDEX is is bigger than the size of
*        the group, then the group is extended to include the given
*        index. This may introduce blank elements, which will normally
*        be removed before returning the group to the calling
*        application.
*     NAME = CHARACTER (Given)
*        The text to be stored in the NAMES array.
*     LEVEL = INTEGER (Given)
*        The indirection depth at which the name was specified. Zero
*        should be given if the name was given directly, instead of by
*        an indirection element.
*     FILE = CHARACTER (Given)
*        The name of the indirection file in which the name was
*        specified. A blank should be given if the name was given
*        directly, instead of by an indirection element.
*     MODGP = INTEGER (Given)
*        The IRH identifier of the group used as a basis for the name
*        if it was created as a result of a modification element. A
*        value of zero should be given if the name was not created as a
*        result of a modification element.
*     MODIN = INTEGER (Given)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the name given by argument NAME.  If MODGP
*        is given as zero, then MODIN is ignored.
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
*        Added %VALed string lengths at end of argument lists.
*        Added DAT_PAR.
*     27-FEB-1992 (PDRAPER):
*        Changed IRH1_SETC argument list. Changed IRH1_PTELM argument
*        list. These caused problems with %VALed character arrays
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
*        HCM_NMPNT( IRH__MAXG ) = INTEGER (Write)
*           Pointers to the mapped NAMES array of each group.
*        HCM_MGPNT( IRH__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_GROUP array of each group.
*        HCM_MIPNT( IRH__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_INDEX array of each group.
*        HCM_LVPNT( IRH__MAXG ) = INTEGER (Write)
*           Pointers to the mapped LEVEL array of each group.
*        HCM_FLPNT( IRH__MAXG ) = INTEGER (Write)
*           Pointers to the mapped FILE array of each group.
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.
*        HCM_SIZE( IRH__MAXG ) = INTEGER (Read and Write)
*           The size of the array components in each GROUP structure.

*  Arguments Given:
      INTEGER IDH
      INTEGER INDEX
      CHARACTER NAME*(*)
      INTEGER LEVEL
      CHARACTER FILE*(*)
      INTEGER MODGP
      INTEGER MODIN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FSTNEW             ! Index of first new array element.
      INTEGER INDX               ! Actual array index to use.
      INTEGER NEWSIZ             ! New size of group arrays.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the text string is to be appended to the end of the NAMES array
*  (indicated by INDEX being zero or negative), the used index will be
*  one higher than the current group size.
      IF( INDEX .LE. 0 ) THEN
         INDX = HCM_GSIZE( IDH ) + 1

      ELSE
         INDX = INDEX

      END IF

*  If necessary, extend the NAMES array to make room for the new value.
*  This involves unmapping all array components of the GROUP structure,
*  altering the sizes of the arrays, and then mapping them again. To
*  cut down on the number of times this needs to be done, the arrays
*  are extended by more than necessary. The new elements are then
*  initialised.
      IF( INDX .GT. HCM_SIZE( IDH ) ) THEN
         FSTNEW = HCM_SIZE( IDH ) + 1
         NEWSIZ = INDX + IRH__INCN

*  NAMES...
         CALL DAT_UNMAP( HCM_LOCNM( IDH ), STATUS )
         CALL DAT_ALTER( HCM_LOCNM( IDH ), 1, NEWSIZ, STATUS )
         CALL DAT_MAPV( HCM_LOCNM( IDH ), '_CHAR', 'UPDATE',
     :                  HCM_NMPNT( IDH ), NEWSIZ, STATUS )
         CALL IRH1_SETC( FSTNEW, NEWSIZ, NEWSIZ,
     :                   %VAL( HCM_NMPNT( IDH ) ), IRH__BLANK, STATUS,
     :                   %VAL( IRH__SZNAM ) )

*  MOD_GROUP...
         CALL DAT_UNMAP( HCM_LOCMG( IDH ), STATUS )
         CALL DAT_ALTER( HCM_LOCMG( IDH ), 1, NEWSIZ, STATUS )
         CALL DAT_MAPV( HCM_LOCMG( IDH ), '_INTEGER', 'UPDATE',
     :                  HCM_MGPNT( IDH ), NEWSIZ, STATUS )
         CALL IRH1_SETI( FSTNEW, NEWSIZ, 0, NEWSIZ, 
     :                  %VAL( HCM_MGPNT( IDH ) ), STATUS )

*  MOD_INDEX...
         CALL DAT_UNMAP( HCM_LOCMI( IDH ), STATUS )
         CALL DAT_ALTER( HCM_LOCMI( IDH ), 1, NEWSIZ, STATUS )
         CALL DAT_MAPV( HCM_LOCMI( IDH ), '_INTEGER', 'UPDATE',
     :                  HCM_MIPNT( IDH ), NEWSIZ, STATUS )
         CALL IRH1_SETI(FSTNEW, NEWSIZ, 0,  NEWSIZ, 
     :                  %VAL( HCM_MIPNT( IDH ) ), STATUS )

*  LEVEL...
         CALL DAT_UNMAP( HCM_LOCLV( IDH ), STATUS )
         CALL DAT_ALTER( HCM_LOCLV( IDH ), 1, NEWSIZ, STATUS )
         CALL DAT_MAPV( HCM_LOCLV( IDH ), '_INTEGER', 'UPDATE',
     :                  HCM_LVPNT( IDH ), NEWSIZ, STATUS )
         CALL IRH1_SETI( FSTNEW, NEWSIZ, 0, NEWSIZ, 
     :                  %VAL( HCM_LVPNT( IDH ) ), STATUS )

*  FILE...
         CALL DAT_UNMAP( HCM_LOCFL( IDH ), STATUS )
         CALL DAT_ALTER( HCM_LOCFL( IDH ), 1, NEWSIZ, STATUS )
         CALL DAT_MAPV( HCM_LOCFL( IDH ), '_CHAR', 'UPDATE',
     :                  HCM_FLPNT( IDH ), NEWSIZ, STATUS )
         CALL IRH1_SETC( FSTNEW, NEWSIZ, NEWSIZ, 
     :                   %VAL( HCM_FLPNT( IDH ) ), ' ', STATUS,
     :                   %VAL( IRH__SZNAM ) )

*  Store the new array size in common.
         HCM_SIZE( IDH ) = NEWSIZ

      END IF

*  Update the group size if necessary.
      IF( INDX .GT. HCM_GSIZE( IDH ) ) HCM_GSIZE( IDH ) = INDX

*  Store the information in the group.
      IF( HCM_GSIZE( IDH ) .GT. 0 ) THEN
         CALL IRH1_IPUT( INDX, 
     :               HCM_GSIZE( IDH ), %VAL( HCM_NMPNT( IDH ) ),
     :               %VAL( HCM_LVPNT( IDH ) ), %VAL( HCM_FLPNT( IDH ) ),
     :               %VAL( HCM_MGPNT( IDH ) ), %VAL( HCM_MIPNT( IDH ) ),
     :               NAME, LEVEL, FILE, MODGP, MODIN,
     :               STATUS, %VAL( IRH__SZNAM ), %VAL( IRH__SZNAM ) )
      END IF

      END
* $Id$
