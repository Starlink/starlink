      SUBROUTINE GRP1_GTSLT( TYPE, SLOT, STATUS )
*+
*  Name:
*     GRP1_GTSLT

*  Purpose:
*     Create a new empty group and return its index within the common
*     arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_GTSLT( TYPE, SLOT, STATUS )

*  Description:
*     This routine finds the lowest common array index not currently in
*     use, and returns it in argument SLOT. The slot is flagged as
*     being in use. If there is no free space in the common arrays an
*     error is reported. The five mapped arrays holding the group
*     contents are then created, and pointers to them stored in common.
*     The default syntax characters are assigned to the group. Groups
*     are initially set to be case sensitive on all systems, but this
*     can be changed by calling GRP_SETCS.  If any error occurs, an
*     slot number of zero is returned.
*
*     The GRP_ system stores information describing each group in
*     arrays in common.  The same element from different arrays holds
*     information for the same group. Different elements within each
*     array hold information for different groups. The "slot" number
*     for a group is just the index into these common arrays at which
*     the information describing the group is stored. The arrays in
*     common have a lower bound of 1 and an upper bound given by
*     symbolic constant GRP__MAXG.
*
*     The common arrays hold "global" properties of each group (such as
*     the group size for instance). The actual contents of each group
*     are stored in five temporary dynamic arrays:
*
*     NAMES(SIZE) (_CHAR*GRP__SZNAM ) - The names which form the group.
*     This is a character array. The array is extended as necessary to
*     make room for new names. The current size is stored in common
*     array CMN_SIZE. Unused elements of the NAMES array are set blank.
*
*     MOD_GROUP(SIZE) (_INTEGER) - This is GRP__NOID for all names NOT
*     created by a modification element. For names created by a
*     modification element, MOD_GROUP gives the GRP identifier of the
*     group containing the name upon which this name was based.
*
*     MOD_INDEX(SIZE) (_INTEGER) - This is zero for all names not
*     created by a modification element. For names created by a
*     modification element, MOD_INDEX gives the index of the name upon
*     which this name was based. The index refers to the group
*     identified by MOD_GROUP.
*
*     LEVEL(SIZE) (_INTEGER) - This gives the depth of indirection at
*     which the corresponding name was specified. Names given directly,
*     rather than by indirection have a value zero.
*
*     FILE_INDEX(SIZE) (_INTEGER) - This holds zero for names with
*     indirection level of zero. For other names, it gives the
*     index into another array (called the FILES array) at which the
*     specification of the file in which the name was given is stored.
*     The FILES array is created by routine GRP1_PTIND when the first
*     indirection element is found.
*
*     Pointers to all these mapped arrays are stored in common. The
*     arrays are kept permanently mapped until the group is deleted.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        A description of the contents of the group.
*     SLOT = INTEGER (Returned)
*        The slot number for the new group. Set to zero if an error
*        occurs.
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
      INCLUDE 'GRP_ERR'          ! GRP error values.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_CHARS( GRP__MAXG ) = CHARACTER*(GRP__NCHAR) (Write)
*           The control characters used to define the syntax of group
*           expressions. A set of characters stored in a single string
*           for each group. The order is defined by global constants
*           GRP__PINDC, GRP__PDELC, GRP__PCOMC, etc (see GRP_PAR).
*        CMN_INPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped FILE_INDEX array of each group.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Write)
*           The index of the last entry in each group.
*        CMN_LVPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped LEVEL array of each group.
*        CMN_MGPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_GROUP array of each group.
*        CMN_MIPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_INDEX array of each group.
*        CMN_NMPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped NAMES array of each group.
*        CMN_OWNER( GRP__MAXG ) = INTEGER (Write)
*           The identifier of the group (if any) which owns each group.
*        CMN_SIZE( GRP__MAXG ) = INTEGER (Write)
*           The size of the temporary mapped arrays for each group.
*        CMN_SLAVE( GRP__MAXG ) = INTEGER (Write)
*           The identifier of the group (if any) owned by each group.
*        CMN_TYPE( GRP__MAXG ) = CHARACTER*(GRP__SZTYP) (Write)
*           The TYPE of each group.
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Write)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.
*        CMN_USED( GRP__MAXG ) = LOGICAL (Write)
*           True if the corresponding group identifier is in use.

*  Arguments Given:
      CHARACTER TYPE*(*)

*  Arguments Returned:
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER I                  ! Loop count.

*.

*  Set the output group slot number to zero before checking the status.
      SLOT = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the lowest GRP identifier not currently in use.
      DO I = 1, GRP__MAXG
         IF( .NOT. CMN_USED( I ) .AND. SLOT .EQ. 0 ) SLOT = I
      END DO

*  If no free slot was found, give an error report.
      IF( SLOT .EQ. 0 ) THEN
         STATUS = GRP__NOMOR
         CALL MSG_SETI( 'MX', GRP__MAXG )
         CALL ERR_REP('GRP1_GTSLT_ERR1',
     :            'GRP1_GTSLT: Maximum number of groups (^MX) exceeded',
     :                STATUS )
         GO TO 999
      END IF

*  Store the supplied group type in common.
      CMN_TYPE( SLOT ) = TYPE

*  Indicate that the group is case sensitive.
      CMN_UPPER( SLOT ) = .FALSE.

*  Each group can be "owned" by another group, thus establishing a
*  chain of groups associated with each other by this "owner-slave"
*  relationship. CMN_OWNER holds the identifier of the group which owns
*  the current group. Indicate that no "owner" has yet been defined for
*  this group.
      CMN_OWNER( SLOT ) = GRP__NOID

*  CMN_SLAVE holds the identifier of the group owned by the current
*  group. Indicate that no "slave" has yet been defined for this group.
      CMN_SLAVE( SLOT ) = GRP__NOID

*  Assign the default syntax characters to the group.
      CMN_CHARS( SLOT ) = GRP__DEFCC

*  Store the initial size of the group (this is the total available
*  size for the group, not size actually in use). The initial size of
*  this array is given by the symbolic constant GRP__INITN.
      CMN_SIZE( SLOT ) = GRP__INITN

*  Get a pointer to a character array (known as NAMES) This is
*  extended as necessary by other GRP routines. Each element of the
*  array contains CMN__SZNAM characters.
      CALL PSX_CALLOC( GRP__INITN*GRP__SZNAM, '_CHAR',
     :                  CMN_NMPNT( SLOT ), STATUS )

      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set the contents of NAMES blank.  NB, the final argument specifies
*  the length of each character string in the mapped NAMES array, and
*  is required by UNIX. There is no corresponding dummy argument in the
*  code for GRP1_SETC.
      CALL GRP1_SETC( 1, GRP__INITN, GRP__INITN,
     :                %VAL( CNF_PVAL( CMN_NMPNT( SLOT ) ) ),
     :                ' ', STATUS,
     :                %VAL( CNF_CVAL( GRP__SZNAM ) ) )

*  Get a pointer to an integer array (known as MOD_GROUP), storing it
*  in common (the size will always be the same as NAMES).
      CALL PSX_CALLOC( GRP__INITN, '_INTEGER', CMN_MGPNT( SLOT ),
     :                 STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set the contents of MOD_GROUP to GRP__NOID.
      CALL GRP1_SETI( 1, GRP__INITN, GRP__NOID, GRP__INITN,
     :                %VAL( CNF_PVAL( CMN_MGPNT( SLOT ) ) ), STATUS )

*  Get a pointer to an integer array (known as MOD_INDEX), storing it
*  in common (the size will always be the same as NAMES).
      CALL PSX_CALLOC( GRP__INITN, '_INTEGER', CMN_MIPNT( SLOT ),
     :                 STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set the contents of MOD_INDEX to zero.
      CALL GRP1_SETI( 1, GRP__INITN, 0, GRP__INITN,
     :                %VAL( CNF_PVAL( CMN_MIPNT( SLOT ) ) ), STATUS )

*  Get a pointer to an integer array (known as LEVEL), storing it
*  in common (the size will always be the same as NAMES).
      CALL PSX_CALLOC( GRP__INITN, '_INTEGER', CMN_LVPNT( SLOT ),
     :                 STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set the contents of LEVEL to zero.
      CALL GRP1_SETI( 1, GRP__INITN, 0, GRP__INITN,
     :                %VAL( CNF_PVAL( CMN_LVPNT( SLOT ) ) ), STATUS )

*  Get a pointer to an integer array ( known as FILE_INDEX). This is
*  extended as necessary by other GRP routines.
      CALL PSX_CALLOC( GRP__INITN, '_INTEGER', CMN_INPNT( SLOT ),
     :                 STATUS )

*  Set the contents of FILE_INDEX to zero.
      CALL GRP1_SETI( 1, GRP__INITN, 0, GRP__INITN,
     :                %VAL( CNF_PVAL( CMN_INPNT( SLOT ) ) ), STATUS )

*  The number of used entries in these arrays (called the "group size")
*  will not be the same as the total array size if there are any spare
*  entries. Initialise the group size to zero.
      CMN_GSIZE( SLOT ) = 0

*  If all is OK, indicate that the group is in use.
      IF ( STATUS .EQ. SAI__OK ) CMN_USED( SLOT ) = .TRUE.

*  If an error has occurred ensure the group slot is zero, and give a
*  context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         SLOT = 0
         CALL ERR_REP( 'GRP1_GTSLT_ERR2',
     :                 'GRP1_GTSLT: Unable to create a new group',
     :                 STATUS )
      END IF

      END
