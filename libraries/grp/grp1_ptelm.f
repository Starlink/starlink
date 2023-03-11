      SUBROUTINE GRP1_PTELM( SLOT, INDEX, NAME, LEVEL, IFILE, MODGP,
     :                       MODIN, STATUS )
*+
*  Name:
*     GRP1_PTELM

*  Purpose:
*     Put an element into a group, including supplementary information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_PTELM( SLOT, INDEX, NAME, LEVEL, IFILE, MODGP, MODIN,
*                      STATUS )

*  Description:
*     This routine stores the given text string in an element of the
*     NAMES array associated with the group identified by SLOT. The
*     supplementary information is stored in the corresponding elements
*     of the LEVEL, FILE_INDEX, MOD_GROUP and MOD_INDEX arrays (see
*     routine GRP1_GTSLT). The calling routine can specify an explicit
*     index at which the element is to be stored in the group, or
*     alternatively, if INDEX is negative or zero (or greater than the
*     current group size), the element is appended to the end of the
*     group.  If the index is greater than the size of the group
*     arrays, the arrays are extended to accommodate it.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The GRP slot number for the group to which the new name is to
*        be added.
*     INDEX = INTEGER (Given)
*        The index within the group at which the element is to be
*        stored. If this is negative or zero, the element is appended
*        to the end of the group. If INDEX is is bigger than the size
*        of the group, then the group is extended to include the given
*        index. This may introduce blank elements.
*     NAME = CHARACTER * ( * ) (Given)
*        The text to be stored in the NAMES array.
*     LEVEL = INTEGER (Given)
*        The indirection depth at which the name was specified. Zero
*        should be given if the name was given directly, instead of by
*        an indirection element.
*     IFILE = INTEGER (Given)
*        The index within the FILES array (see routine GRP1_PTIND) at
*        which the the name of the indirection file in which the name
*        was specified is given. A value of zero should be given if the
*        name was given directly, instead of by an indirection element.
*     MODGP = INTEGER (Given)
*        The GRP identifier for the group used as a basis for the name
*        if it was created as a result of a modification element. A
*        value of GRP__NOID should be given if the name was not created
*        as a result of a modification element.
*     MODIN = INTEGER (Given)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the name given by argument NAME.  If MODGP
*        is given as GRP__NOID, then MODIN is ignored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_NMPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped NAMES array of each group.
*        CMN_MGPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_GROUP array of each group.
*        CMN_MIPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_INDEX array of each group.
*        CMN_LVPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped LEVEL array of each group.
*        CMN_INPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped FILE_INDEX array of each group.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.
*        CMN_SIZE( GRP__MAXG ) = INTEGER (Read and Write)
*           The size of the array components in each GROUP structure.

*  Arguments Given:
      INTEGER SLOT
      INTEGER INDEX
      CHARACTER NAME*(*)
      INTEGER LEVEL
      INTEGER IFILE
      INTEGER MODGP
      INTEGER MODIN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

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
         INDX = CMN_GSIZE( SLOT ) + 1

      ELSE
         INDX = INDEX

      END IF

*  If necessary, extend the NAMES array to make room for the new value.
*  To cut down on the number of times this needs to be done, the arrays
*  are extended by more than necessary. The new elements are then
*  initialised.
      IF( INDX .GT. CMN_SIZE( SLOT ) ) THEN
         FSTNEW = CMN_SIZE( SLOT ) + 1
         NEWSIZ = INDX + GRP__INCN

*  NAMES...
*  Extend the memory mapped to the character data.
         CALL PSX_REALLOC( NEWSIZ*GRP__NBC*GRP__SZNAM,
     :                     CMN_NMPNT( SLOT ), STATUS )

*  Initialize the new names to be blank.
         CALL GRP1_SETC( FSTNEW, NEWSIZ, NEWSIZ,
     :                   %VAL( CNF_PVAL( CMN_NMPNT( SLOT ) ) ),
     :                   ' ', STATUS,
     :                   %VAL( CNF_CVAL( GRP__SZNAM ) ) )

*  MOD_GROUP...
*  Extend the memory mapped to the integer data.
         CALL PSX_REALLOC( NEWSIZ*GRP__NBI, CMN_MGPNT( SLOT ), STATUS )

*  Initialise the new integer data.
         CALL GRP1_SETI( FSTNEW, NEWSIZ, GRP__NOID, NEWSIZ,
     :                   %VAL( CNF_PVAL( CMN_MGPNT( SLOT ) ) ), STATUS )

*  MOD_INDEX...
*  Extend the memory mapped to the integer data.
         CALL PSX_REALLOC( NEWSIZ*GRP__NBI, CMN_MIPNT( SLOT ), STATUS )

*  Initialise the new integer data.
         CALL GRP1_SETI( FSTNEW, NEWSIZ, 0,  NEWSIZ,
     :                   %VAL( CNF_PVAL( CMN_MIPNT( SLOT ) ) ), STATUS )

*  LEVEL...
*  Extend the memory mapped to the integer data.
         CALL PSX_REALLOC( NEWSIZ*GRP__NBI, CMN_LVPNT( SLOT ), STATUS )

*  Initialise the new integer data.
         CALL GRP1_SETI( FSTNEW, NEWSIZ, 0, NEWSIZ,
     :                   %VAL( CNF_PVAL( CMN_LVPNT( SLOT ) ) ), STATUS )

*  IFILE...
*  Extend the memory mapped to the integer data.
         CALL PSX_REALLOC( NEWSIZ*GRP__NBI, CMN_INPNT( SLOT ), STATUS )

*  Initialise the new integer data.
         CALL GRP1_SETI( FSTNEW, NEWSIZ, 0, NEWSIZ,
     :                   %VAL( CNF_PVAL( CMN_INPNT( SLOT ) ) ), STATUS )

*  Store the new array size in common.
         CMN_SIZE( SLOT ) = NEWSIZ

      END IF

*  If all has gone OK, update the group size if necessary...
      IF ( STATUS .EQ. SAI__OK ) then
         IF( INDX .GT. CMN_GSIZE( SLOT ) ) CMN_GSIZE( SLOT ) = INDX

*  ...and store the information in the group.
         IF( CMN_GSIZE( SLOT ) .GT. 0 ) THEN
            CALL GRP1_IPUT( INDX,
     :             CMN_GSIZE( SLOT ),
     :             %VAL( CNF_PVAL( CMN_NMPNT( SLOT ) ) ),
     :             %VAL( CNF_PVAL( CMN_LVPNT( SLOT ) ) ),
     :             %VAL( CNF_PVAL( CMN_INPNT( SLOT ) ) ),
     :             %VAL( CNF_PVAL( CMN_MGPNT( SLOT ) ) ),
     :             %VAL( CNF_PVAL( CMN_MIPNT( SLOT ) ) ),
     :             NAME, LEVEL, IFILE, MODGP, MODIN, STATUS,
     :             %VAL( CNF_CVAL( GRP__SZNAM ) ) )
         END IF

      END IF

      END
