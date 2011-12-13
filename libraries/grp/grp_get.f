      SUBROUTINE GRP_GET( IGRP, INDEX, SIZE, NAMES, STATUS )
*+
*  Name:
*     GRP_GET

*  Purpose:
*     Returns a set of names contained in a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_GET( IGRP, INDEX, SIZE, NAMES, STATUS )

*  Description:
*     The names with indices between INDEX and INDEX+SIZE-1 (inclusive)
*     contained in the given group are returned. An error is reported if
*     the bounds of the group are exceeded, and STATUS is returned
*     equal to GRP__OUTBN. If the group is case insensitive (as
*     established by a call to GRP_SETCS) then the names are converted
*     to upper case before being returned.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     INDEX = INTEGER (Given)
*        The lowest index for which the corresponding name is required.
*     SIZE = INTEGER (Given)
*        The number of names required.
*     NAMES( SIZE ) = CHARACTER * ( * ) (Returned)
*        The names held at the given positions in the group. The
*        corresponding character variables should have declared length
*        specified by the symbolic constant GRP__SZNAM. If the declared
*        length is shorter than this, the returned names may be
*        truncated, but no error is reported.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     11-JUL-2008 (TIMJ):
*        Report upper bound when exceeding it.
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
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.
*        CMN_NMPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped NAMES array of each group.
*        CMN_SIZE( GRP__MAXG ) = INTEGER (Read)
*           The size of the mapped NAMES array of each group.
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Read)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Arguments Given:
      INTEGER IGRP
      INTEGER INDEX
      INTEGER SIZE

*  Arguments Returned:
      CHARACTER NAMES( SIZE )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER I                  ! Index into the group NAMES array.
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each index value.
      DO I = INDEX, INDEX + SIZE - 1

*  If the index is outside the bounds of the group, report an error.
         IF( I .LE. 0 .OR. I .GT. CMN_GSIZE( SLOT ) ) THEN
            STATUS = GRP__OUTBN
            CALL MSG_SETI( 'I', I )
            CALL MSG_SETI( 'MX', CMN_GSIZE( SLOT ) )
            CALL ERR_REP( 'GRP_GET_ERR1',
     :      'GRP_GET: Attempt to access a name outside the bounds of'//
     :      ' the group. (^I/^MX)', STATUS )
            GO TO 999

*  Otherwise, call GRP1_GETC to do the work.  NB, the final argument
*  specifies the length of each character string in the mapped NAMES
*  array, and is required by UNIX. There is no corresponding dummy
*  argument in the code for GRP1_GETC.
         ELSE
            CALL GRP1_GETC( CMN_GSIZE( SLOT ),
     :                      %VAL( CNF_PVAL( CMN_NMPNT( SLOT ) ) ),
     :                      I, NAMES( I - INDEX + 1 ), STATUS,
     :                      %VAL( CNF_CVAL( GRP__SZNAM ) ) )

*  If the group is case insensitive, convert the name to upper case.
            IF( CMN_UPPER( SLOT ) ) CALL CHR_UCASE(
     :                                         NAMES( I - INDEX + 1 ) )

         END IF

      END DO

*  If an error has occurred give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_GET_ERR2',
     :                 'GRP_GET: Unable to get names from a group',
     :                  STATUS )
      END IF

      END
