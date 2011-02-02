      SUBROUTINE GRP_MSG( TOKEN, IGRP, INDEX, STATUS )
*+
*  Name:
*     GRP_MSG

*  Purpose:
*     Assign an element of a group to a message token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_MSG( TOKEN, IGRP, INDEX, STATUS )

*  Description:
*     The routine assigns a specified element of a GRP group to a message
*     token for use in constructing messages with the ERR_ and MSG_
*     routines (see SUN/104).

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        Name of the message token.
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     INDEX = INTEGER (Given)
*        The index of the element to assign to the message token.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine attempts to execute even if STATUS is bad on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-FEB-2011 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER TOKEN*(*)
      INTEGER IGRP
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

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
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Read)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Local Variables:
      INTEGER SLOT
      CHARACTER NAME*(GRP__SZNAM)

*.

*  Initialise
      NAME = ' '

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If the index is outside the bounds of the group, report an error.
         IF( INDEX .LE. 0 .OR. INDEX .GT. CMN_GSIZE( SLOT ) ) THEN
            STATUS = GRP__OUTBN
            CALL MSG_SETI( 'I', INDEX )
            CALL MSG_SETI( 'MX', CMN_GSIZE( SLOT ) )
            CALL ERR_REP( 'GRP_GET_ERR1', 'GRP_MSG: Attempt to access'//
     :                    ' a name outside the bounds of the group. '//
     :                    '(^I/^MX)', STATUS )

*  Otherwise, call GRP1_GETC to get the string.  NB, the final argument
*  specifies the length of each character string in the mapped NAMES
*  array, and is required by UNIX. There is no corresponding dummy
*  argument in the code for GRP1_GETC.
         ELSE
            CALL GRP1_GETC( CMN_GSIZE( SLOT ),
     :                      %VAL( CNF_PVAL( CMN_NMPNT( SLOT ) ) ),
     :                      INDEX, NAME, STATUS,
     :                      %VAL( CNF_CVAL( GRP__SZNAM ) ) )

*  If the group is case insensitive, convert the name to upper case.
            IF( CMN_UPPER( SLOT ) ) CALL CHR_UCASE( NAME )

         END IF
      END IF

*  End the current error reporting environment.
      CALL ERR_END( STATUS )

*  Assign the string to the message token. Do it as the last act so that
*  there is no chance of the token being annulled.
      CALL MSG_SETC( TOKEN, NAME )

      END
