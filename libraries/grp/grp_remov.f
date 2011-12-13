      SUBROUTINE GRP_REMOV( IGRP1, NAME, IGRP2, STATUS )
*+
*  Name:
*     GRP_REMOV

*  Purpose:
*     Remove all occurrences of a given name from a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_REMOV( IGRP1, NAME, IGRP2, STATUS )

*  Description:
*     A new group is created by copying the contents of an existing
*     group, excluding any occurrences of a specified name. Note, a name
*     with a given index in the input group will in general have a
*     different index in the output group. The new group inherits the
*     type, control characters and case sensitivity flag of the old
*     group, but does not inherit any owner/slave relationships (see
*     routine GRP_SOWN). If the input group is no longer required, it
*     should be deleted using GRP_DELET.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the input group.
*     NAME = CHARACTER * ( * )(Given)
*        The name to be removed. Leading blanks are significant, and
*        case is also significant unless the group has been marked as
*        case insensitive by calling GRP_SETCS.
*     IGRP2 = INTEGER (Returned)
*        A GRP identifier for the created group. Returned equal to
*        GRP__NOID if an error occurs.
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
*     2-JUL-2009 (DSB):
*        Honour the group case sensitivity flag.
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

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_CHARS( GRP__MAXG ) = CHARACTER*(GRP__NCHAR) (Read and Write)
*           The control characters used to define the syntax of group
*           expressions. A set of characters stored in a single string
*           for each group. The order is defined by global constants
*           GRP__PINDC, GRP__PDELC, GRP__PCOMC, etc (see GRP_PAR).
*        CMN_FLSIZ( GRP__MAXG ) = INTEGER (Read)
*           The size of the FILES array associated with each group.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.
*        CMN_TYPE( GRP__MAXG ) = CHARACTER (Read)
*           Group types.
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Read and Write)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Arguments Given:
      INTEGER IGRP1
      CHARACTER NAME*(*)

*  Arguments Returned:
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.
      LOGICAL CHR_SIMLR          ! Case-insensitive string comparison

*  Local Variables:
      INTEGER DEPTH              ! Depth at which the retrieved element
                                 ! was given.
      CHARACTER ELEM*(GRP__SZNAM)! The retrieved element.
      CHARACTER FILE*(GRP__SZFNM)! An indirection file name.
      INTEGER I                  ! Loop count.
      INTEGER IFILE              ! Index within the FILES array of the
                                 ! indirection file from which the
                                 ! retreived element was read.
      INTEGER J                  ! Index at which the next name will be
                                 ! stored in the output group.
      CHARACTER LNAME*(GRP__SZNAM)! Local copy of the supplied name.
      INTEGER MODGRP             ! Modified group which gave rise to the
                                 ! retrieved element.
      INTEGER MODIND             ! Modified index which gave rise to the
                                 ! retrieved element.
      INTEGER SLOT1              ! Index within common arrays at which
                                 ! the supplied group properties are
                                 ! stored.
      INTEGER SLOT2              ! Index within common arrays at which
                                 ! the created group properties are
                                 ! stored.

*.

*  Ensure that an invalid identifier is returned if an error condition
*  exists on entry.
      IGRP2 = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP1, SLOT1, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Create a new (initially empty) group, with the same TYPE as the
*  input group.  Get the index of the next free slot in the common
*  arrays.  Information describing the global properties of the new
*  group will be stored in this slot. An array is created in temporary
*  workspace to hold the contents of the group.
      CALL GRP1_GTSLT( CMN_TYPE( SLOT1 ), SLOT2, STATUS )

*  Make the new group the same size as the old group.
      CALL GRP1_PTELM( SLOT2, CMN_GSIZE( SLOT1 ), ' ', 0, 0, GRP__NOID,
     :                 0, STATUS )

*  If the group is case insensitive produce an upper case copy of the
*  supplied name.
      LNAME = NAME
      IF( CMN_UPPER( SLOT1 ) ) CALL CHR_UCASE( LNAME )

*  Initialise the index at which the next name will be stored in the
*  output group.
      J = 1

*  Loop round all the names in the input group.
      DO I = 1, CMN_GSIZE( SLOT1 )

*  Get the element.
         CALL GRP1_GTELM( SLOT1, I, ELEM, DEPTH, IFILE, MODGRP, MODIND,
     :                    STATUS )

*  Copy the name to the output so long as it is not the same as the
*  supplied name.
         IF( ( .NOT. CMN_UPPER( SLOT1 ) .AND. (ELEM .NE. LNAME ) ) .OR.
     :       ( CMN_UPPER( SLOT1 ) .AND.
     :        .NOT. CHR_SIMLR( ELEM, LNAME ) ) ) THEN
            CALL GRP1_PTELM( SLOT2, J, ELEM, DEPTH, IFILE, MODGRP,
     :                       MODIND, STATUS )

*  Increment the position at which the next name will be stored in the
*  output group.
            J = J + 1

         END IF

      END DO

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Reduce the size of the output group.
      CMN_GSIZE( SLOT2 ) = J - 1
      CALL GRP1_TRUNC( SLOT2, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set up the case sensitivity flag and control characters for the
*  new group.
      CMN_UPPER( SLOT2 ) = CMN_UPPER( SLOT1 )
      CMN_CHARS( SLOT2 ) = CMN_CHARS( SLOT1 )

*  Copy the FILES array from the input to the output group.
      DO I = 1, CMN_FLSIZ( SLOT1 )
         CALL GRP1_GTIND( SLOT1, I, FILE, STATUS )
         CALL GRP1_PTIND( SLOT2, FILE, I, STATUS )
      END DO

*  Create an encoded identifier for the new group.
      CALL GRP1_EXPID( SLOT2, IGRP2, STATUS )

*  If an error occurred, attempt to delete the new group and give a
*  context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP2, STATUS )
         CALL ERR_REP( 'GRP_REMOV_ERR1',
     :   'GRP_REMOV: Unable to remove names from a group.' , STATUS )
      END IF

      END
