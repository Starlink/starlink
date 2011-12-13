      SUBROUTINE GRP_PUT( IGRP, SIZE, NAMES, INDEX, STATUS )
*+
*  Name:
*     GRP_PUT

*  Purpose:
*     Put a given set of literal names into a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_PUT( IGRP, SIZE, NAMES, INDEX, STATUS )

*  Description:
*     The given names are stored in the group in the form in which they
*     are supplied (including any control characters). They overwrite
*     any previous names stored at the specified indices. The group is
*     extended if the range of indices extends beyond the current size
*     of the group. The names can be appended to the end of the group
*     by giving INDEX a value of zero or one greater than the current
*     size of the group. An error is reported if the names are added
*     beyond the end of the group (i.e.  if adding the names would
*     result in a gap within the group for which no names would be
*     defined).

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     SIZE = INTEGER (Given)
*        The size of the NAMES array.
*     NAMES( SIZE ) = CHARACTER * ( * ) (Given)
*        The names to be stored in the group. The first name is stored
*        at the index given by INDEX, the last is stored at index
*        INDEX+SIZE-1.
*     INDEX = INTEGER (Given)
*        The index at which to store the first name. A value of zero
*        causes the names to be appended to the end of the group.
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

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER IGRP
      INTEGER SIZE
      CHARACTER NAMES( SIZE )*(*)
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER I                  ! Index into the group NAMES array.
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
      INTEGER START              ! Lowest index at which to store a new
                                 ! name.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If an index of zero is given, use one more than the current group
*  size.
      IF( INDEX .EQ. 0 ) THEN
         START = CMN_GSIZE( SLOT ) + 1

*  If the given index was outside the bounds of the group, report an
*  error.
      ELSE IF( INDEX .LT. 0 .OR. INDEX .GT. CMN_GSIZE( SLOT ) + 1 ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'IND', INDEX )
         CALL MSG_SETI( 'SIZ', CMN_GSIZE( SLOT ) + 1 )
         CALL ERR_REP( 'GRP_PUT_ERR1',
     :  'GRP_PUT: Supplied index (^IND) is outside the valid range '//
     :  ' [1,^SIZ]', STATUS )
         GO TO 999

*  If the index was inside the bounds of the group, use it.
      ELSE
         START = INDEX

      END IF

*  Loop round each index value. Start at the highest index so that the
*  group will only be extended once.
      DO I = START + SIZE - 1, START, -1

*  Call GRP1_PTELM to do the work.
         CALL GRP1_PTELM( SLOT, I, NAMES( I - START + 1 ), 0, 0,
     :                    GRP__NOID, 0, STATUS )

      END DO

*  If an error occurred give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_PUT_ERR2',
     :                 'GRP_PUT: Unable to add names to a group',
     :                  STATUS )
      END IF

      END
