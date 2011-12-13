      SUBROUTINE GRP_GRPEX( GRPEXP, IGRP1, IGRP2, SIZE, ADDED, FLAG,
     :                      STATUS )
*+
*  Name:
*     GRP_GRPEX

*  Purpose:
*     Append a list of names contained within a supplied group
*     expression to a previously created group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_GRPEX( GRPEXP, IGRP1, IGRP2, SIZE, ADDED, FLAG, STATUS )

*  Description:
*     The supplied group expression is expanded to produce a list of
*     names which are appended to the end of the group identified by
*     IGRP2.
*
*     If the group expression contains any modification elements, then
*     the list of names added to the output group is based on the group
*     identified by IGRP1. If IGRP1 is invalid (equal to the symbolic
*     constant GRP__NOID for instance), then any elements with the
*     syntax of a modification element are stored in the output group
*     as a single literal name.
*
*     If the last character read from the group expression (or from a
*     text file if the last element of the group expression is an
*     indirection element) is equal to the current "flag" character for
*     the group IGRP2 (see routine GRP_SETCC), then argument FLAG is
*     returned set to true. Otherwise, it is returned set to false. The
*     calling application can use this flag for any purpose (eg it may
*     use it to indicate that the user wants to give more names). Note,
*     the flag character itself is not included in the returned group.

*  Arguments:
*     GRPEXP = CHARACTER * ( * ) (Given)
*        A group expression. This should not be longer than GRP__SZGEX.
*        If it is, the surplus characters will be ignored.
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the group to be used as the basis for
*        any modification elements which may be contained within the
*        supplied group expression. This can be set to the symbolic
*        constant GRP__NOID if modification elements are to be treated
*        as literal names.
*     IGRP2 = INTEGER (Given)
*        A GRP identifier for the group to which the new names are to
*        be appended.
*     SIZE = INTEGER (Returned)
*        The number of names in the returned group. It is returned equal
*        to 1 if an error status exists on entry. If an error occurs
*        during execution of this routine, then SIZE is returned equal
*        to the size of the group on entry (unless the group has zero
*        size on entry, in which case it is returned equal to 1).
*     ADDED = INTEGER (Returned)
*        The number of names added to the group by this routine.
*     FLAG = LOGICAL (Returned)
*        .TRUE. if the last character in the group expression is equal
*        to the current flag character for group IGRP2. Note, if this
*        is the case, then the flag character itself is not included
*        in the returned group. FLAG is returned .FALSE. if the last
*        character is not a flag character. Returned .FALSE. if an
*        error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
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
*     10-SEP-1992 (DSB):
*        FLAG set to .FALSE. before checking inherited status, and
*        before reporting any context message.
*     19-FEB-1993 (DSB):
*        SIZE set to 1 before checking inherited status. SIZE returned
*        equal to the size of the group on entry if any error occurs,
*        unless the group had zero size on entry, in which case it is
*        returned equal to 1.
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
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.

*  Arguments Given:
      CHARACTER GRPEXP*(*)
      INTEGER IGRP1
      INTEGER IGRP2

*  Arguments Returned:
      INTEGER SIZE
      INTEGER ADDED
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER OLDSIZ             ! Original size of group.
      INTEGER SLOT1              ! Index within common arrays at which
                                 ! the properties of IGRP1 are stored.
      INTEGER SLOT2              ! Index within common arrays at which
                                 ! the properties of IGRP2 are stored.

*.

*  Ensure FLAG is returned .FALSE. and SIZE returned equal to 1 if an
*  error has already occured.
      FLAG = .FALSE.
      SIZE = 1

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied IGRP2 identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP2, SLOT2, STATUS )

*  Initialise the local record of the current group size to 1.
      OLDSIZ = 1

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Save the current group size.
      OLDSIZ = CMN_GSIZE( SLOT2 )

*  If IGRP1 is a valid GRP identifier then get the corresponding slot
*  number, otherwise store a slot number of zero. No error is reported
*  if IGRP1 is invalid.
      CALL GRP1_ID2SL( IGRP1, SLOT1 )

*  Attempt to append the names specified by the group expression to the
*  output group.
      CALL GRP1_GRAPP( SLOT1, SLOT2, GRPEXP, FLAG, STATUS )

*  Calculate the returned argument values.
      SIZE = CMN_GSIZE( SLOT2 )
      ADDED = SIZE - OLDSIZ

*  If an error occurred, ensure that FLAG is returned .FALSE. and SIZE
*  is returned equal to the greater of 1 and the input group size. Give
*  a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         FLAG = .FALSE.
         SIZE = MAX( 1, OLDSIZ )
         CALL MSG_SETC( 'GX', GRPEXP )
         CALL ERR_REP( 'GRP_GRPEX_ERR1',
     :      'GRP_GRPEX: Unable to read names from group expression ^GX',
     :                  STATUS )
      END IF

      END
