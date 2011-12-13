      SUBROUTINE GRP_PURGE( IGRP1, IGRP2, STATUS )
*+
*  Name:
*     GRP_PURGE

*  Purpose:
*     Purge duplicate entries from a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_PURGE( IGRP1, IGRP2, STATUS )

*  Description:
*     This routine creates a new group based on a given existing group.
*     The contents of the existing group are copied to the new group,
*     but any duplicated names are only included once. The check for
*     duplication is case sensitive unless the group has been declared
*     case insensitive by a call to GRP_SETCS. The new group inherits
*     the type, control characters case sensitivity flag of the old
*     group, but does not inherit any owner/slave relationships (see
*     routine GRP_SOWN).
*
*     Note, indices determined from the old group will in general not
*     point to the same name in the new group. The old group should be
*     deleted using GRP_DELET if it is no longer required.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        The GRP identifier for an existing group.
*     IGRP2 = INTEGER (Returned)
*        A GRP identifier for the created group. This group is a purged
*        form of the group identified by IGRP1. A value of GRP__NOID is
*        returned if an error occurs.
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
*        CMN_CHARS( GRP__MAXG ) = CHARACTER*(GRP__NCHAR) (Read and Write)
*           The control characters used to define the syntax of group
*           expressions. A set of characters stored in a single string
*           for each group. The order is defined by global constants
*           GRP__PINDC, GRP__PDELC, GRP__PCOMC, etc (see GRP_PAR).
*        CMN_FLSIZ( GRP__MAXG ) + INTEGER (Read and Write )
*           The current size of each groups FILES array.
*        CMN_INPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped FILE_INDEX array of each group.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.
*        CMN_LVPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped LEVEL array of each group.
*        CMN_MGPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped MOD_GROUP array of each group.
*        CMN_MIPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped MOD_INDEX array of each group.
*        CMN_NMPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped NAMES array of each group.
*        CMN_SIZE( GRP__MAXG ) = INTEGER (Read)
*           The size of the array components in each GROUP structure.
*        CMN_TYPE( GRP__MAXG ) = CHARACTER (Read)
*           Group types.
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Read and Write)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Arguments Given:
      INTEGER IGRP1

*  Arguments Returned:
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      CHARACTER FILE*(GRP__SZFNM)! Indirection file name.
      INTEGER I                  ! Loop count.
      INTEGER SLOT1              ! Index within common arrays at which
                                 ! the properties of the supplied group
                                 ! are stored.
      INTEGER SLOT2              ! Index within common arrays at which
                                 ! the properties of the created group
                                 ! are stored.

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

*  Cause the group to be extended so that its size is the same as that
*  of the input group.
      CALL GRP1_PTELM( SLOT2, CMN_GSIZE( SLOT1 ), ' ', 0, 0,
     :                 GRP__NOID, 0, STATUS )

*  Call GRP1_IPURG to do the work.  NB, the final 2 arguments specify
*  the lengths of the character strings in the mapped character arrays,
*  and are required by UNIX. There are no corresponding dummy arguments
*  in the code for GRP1_IPURG.
      CALL GRP1_IPURG( CMN_UPPER( SLOT1 ), CMN_SIZE( SLOT1 ),
     :           %VAL( CNF_PVAL( CMN_NMPNT( SLOT1 ) ) ),
     :           %VAL( CNF_PVAL( CMN_MGPNT( SLOT1 ) ) ),
     :           %VAL( CNF_PVAL( CMN_MIPNT( SLOT1 ) ) ),
     :           %VAL( CNF_PVAL( CMN_LVPNT( SLOT1 ) ) ),
     :           %VAL( CNF_PVAL( CMN_INPNT( SLOT1 ) ) ),
     :           CMN_GSIZE( SLOT1 ),
     :           CMN_SIZE( SLOT2 ),
     :           %VAL( CNF_PVAL( CMN_NMPNT( SLOT2 ) ) ),
     :           %VAL( CNF_PVAL( CMN_MGPNT( SLOT2 ) ) ),
     :           %VAL( CNF_PVAL( CMN_MIPNT( SLOT2 ) ) ),
     :           %VAL( CNF_PVAL( CMN_LVPNT( SLOT2 ) ) ),
     :           %VAL( CNF_PVAL( CMN_INPNT( SLOT2 ) ) ),
     :           CMN_GSIZE( SLOT2 ), STATUS ,
     :           %VAL( CNF_CVAL( GRP__SZNAM ) ),
     :           %VAL( CNF_CVAL( GRP__SZNAM ) ) )

*  Set the case sensitivity flag and control characters for the output
*  group.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CMN_UPPER( SLOT2 ) = CMN_UPPER( SLOT1 )
         CMN_CHARS( SLOT2 ) = CMN_CHARS( SLOT1 )

*  Now copy the FILES array associated with the input group to the
*  output group.
         DO I = 1, CMN_FLSIZ( SLOT1 )
            CALL GRP1_GTIND( SLOT1, I, FILE, STATUS )
            CALL GRP1_PTIND( SLOT2, FILE, I, STATUS )
         END DO

      END IF

*  Create an encoded identifier for the new group.
      CALL GRP1_EXPID( SLOT2, IGRP2, STATUS )

*  If an error occurred, try to delete the new group and give a
*  context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP2, STATUS )
         CALL ERR_REP( 'GRP_PURGE_ERR1',
     :       'GRP_PURGE: Unable to purge a group of duplicate entries.',
     :          STATUS )
      END IF

      END
