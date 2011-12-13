      SUBROUTINE GRP_COPY( IGRP1, INDXLO, INDXHI, REJECT, IGRP2,
     :                     STATUS )
*+
*  Name:
*     GRP_COPY

*  Purpose:
*     Copy a section of an existing group to a new group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_COPY( IGRP, INDXLO, INDXHI, REJECT, IGRP2, STATUS )

*  Description:
*     A new group is created by copying a section of an existing group
*     specified by the range of indices supplied in INDXLO and INDXHI.
*     The whole group is used if INDXLO and INDXHI are both zero.
*     The output group can be formed in one of two ways:
*
*     1) All names from the input group are copied to the output group
*     except for those with indices in the given range.
*
*     2) Only those names from the input group which have indices within
*     the given range are copied to the output group.
*
*     The method used is determined by the argument REJECT. Note, a
*     name with a given index in the input group will have a different
*     index in the output group if INDXLO is not 1 (or zero). The new
*     group inherits the type, control characters and case sensitivity
*     flag of the old group, but does not inherit any owner/slave
*     relationships (see routine GRP_SOWN). If the input group is no
*     longer required, it should be deleted using GRP_DELET.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the input group.
*     INDXLO = INTEGER (Given)
*        The lowest index to reject or to copy.
*     INDXHI = INTEGER (Given)
*        The highest index to reject or to copy.
*     REJECT = LOGICAL ( Given)
*        If reject is .TRUE., then names in the given range are
*        rejected.  Otherwise, names in the given range are copied.
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
      INCLUDE 'GRP_ERR'          ! GRP error constants.

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
      INTEGER INDXLO
      INTEGER INDXHI
      LOGICAL REJECT

*  Arguments Returned:
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      CHARACTER ELEM*(GRP__SZNAM)! The retrieved element.
      INTEGER EDEP               ! Depth at which the retrieved element
                                 ! was given.
      INTEGER EIFILE             ! Index within a FILES array at which
                                 ! is stored the indirection file in
                                 ! which the retrieved element was
                                 ! given.
      INTEGER EMODGP             ! Modified group which gave rise to the
                                 ! retrieved element.
      INTEGER EMODIN             ! Modified index which gave rise to the
                                 ! retrieved element.
      CHARACTER FILE*(GRP__SZNAM)! File in which the retrieved element
                                 ! was given.
      INTEGER HILIM              ! Usable upper index limit.
      INTEGER I                  ! Loop count.
      INTEGER LOLIM              ! Usable lower index limit.
      INTEGER OUTSIZ             ! Size of output group.
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

*  If both indices are zero, use the whole group.
      IF( INDXLO .EQ. 0 .AND. INDXHI .EQ. 0 ) THEN
         LOLIM = 1
         HILIM = CMN_GSIZE( SLOT1 )

*  If any other out-of-range indices are given, report an error.
      ELSE IF( INDXLO .LT. 1 .OR. INDXLO .GT. CMN_GSIZE( SLOT1 ) ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'I', INDXLO )
         CALL MSG_SETI( 'S', CMN_GSIZE( SLOT1 ) )
         CALL ERR_REP( 'GRP_COPY_ERR1',
     :          'GRP_COPY: Lower group index (^I) out of bounds [1,^S]',
     :                 STATUS )

      ELSE IF( INDXHI .LT. 1 .OR. INDXHI .GT. CMN_GSIZE( SLOT1 ) ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'I', INDXHI )
         CALL MSG_SETI( 'S', CMN_GSIZE( SLOT1 ) )
         CALL ERR_REP( 'GRP_COPY_ERR2',
     :          'GRP_COPY: Upper group index (^I) out of bounds [1,^S]',
     :                 STATUS )

      ELSE IF( INDXHI .LT. INDXLO ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'IL', INDXLO )
         CALL MSG_SETI( 'IH', INDXHI )
         CALL ERR_REP( 'GRP_COPY_ERR3',
     :   'GRP_COPY: Lower group index (^IL) greater than upper group '//
     :   'index (^IH)', STATUS )

*  If the supplied indices are ok, use them.
      ELSE
         HILIM = INDXHI
         LOLIM = INDXLO

      END IF

*  If the supplied range of names is to be rejected...
      IF( REJECT ) THEN

*  Calculate the final size of the output group.
         OUTSIZ = CMN_GSIZE( SLOT1 ) + LOLIM - HILIM - 1

*  Copy all the names above the high index limit. Do them in reverse
*  order so that the output group is only extended once by GRP1_PTELM.
         DO I = CMN_GSIZE( SLOT1 ), HILIM + 1, -1
            CALL GRP1_GTELM( SLOT1, I, ELEM, EDEP, EIFILE, EMODGP,
     :                       EMODIN, STATUS )
            CALL GRP1_PTELM( SLOT2, I - HILIM - 1 + LOLIM, ELEM, EDEP,
     :                       EIFILE, EMODGP, EMODIN, STATUS )
         END DO

*  Now copy all the names with indices below the low index limit.
         DO I = 1, LOLIM - 1
            CALL GRP1_GTELM( SLOT1, I, ELEM, EDEP, EIFILE, EMODGP,
     :                       EMODIN, STATUS )
            CALL GRP1_PTELM( SLOT2, I, ELEM, EDEP, EIFILE, EMODGP,
     :                       EMODIN, STATUS )
         END DO

*  If the supplied range of names is to be copied...
      ELSE

*  Calculate the final size of the output group.
         OUTSIZ = HILIM - LOLIM + 1

*  Copy all the names above the high index limit. Do them in reverse
*  order so that the output group is only extended once by GRP1_PTELM.
         DO I = HILIM, LOLIM, -1
            CALL GRP1_GTELM( SLOT1, I, ELEM, EDEP, EIFILE, EMODGP,
     :                       EMODIN, STATUS )
            CALL GRP1_PTELM( SLOT2, I - LOLIM + 1, ELEM, EDEP,
     :                       EIFILE, EMODGP, EMODIN, STATUS )
         END DO

      END IF

*  Set up the case sensitivity flag and control characters for the
*  new group.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CMN_UPPER( SLOT2 ) = CMN_UPPER( SLOT1 )
         CMN_CHARS( SLOT2 ) = CMN_CHARS( SLOT1 )

*  Copy the FILES array from the input to the output group.
         DO I = 1, CMN_FLSIZ( SLOT1 )
            CALL GRP1_GTIND( SLOT1, I, FILE, STATUS )
            CALL GRP1_PTIND( SLOT2, FILE, I, STATUS )
         END DO

      END IF

*  Create an encoded identifier for the new group.
      CALL GRP1_EXPID( SLOT2, IGRP2, STATUS )

*  If an error occurred, attempt to delete the new group and give a
*  context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP2, STATUS )
         CALL ERR_REP( 'GRP_COPY_ERR4',
     :   'GRP_COPY: Unable to copy a section of a group.' , STATUS )
      END IF

      END
