      SUBROUTINE GRP_INFOC( IGRP, INDEX, ITEM, VALUE, STATUS )
*+
*  Name:
*     GRP_INFOC

*  Purpose:
*     Retrieve an item of character information about a name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_INFOC( IGRP, INDEX, ITEM, VALUE, STATUS )

*  Description:
*     This routine returns an item of character information about a
*     single name from a group. The item can be any one of those
*     described under argument ITEM.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP group identifier.
*     INDEX = INTEGER (Given)
*        An index within the group specified by IGRP.  If the supplied
*        value is outside the bounds of the group, then a blank value
*        is returned for VALUE, and an error is reported.
*     ITEM = CHARACTER * ( * ) (Given)
*        The name of an item of information. This can be any of the
*        following (abbreviations are not allowed):
*
*        NAME  -  The name itself. If the group is case insensitive
*        (as established by a call to routine GRP_SETCS) then the name
*        is returned in upper case.
*
*        FILE  -  The text file within which the name was explicitly
*        given. If the name was not specified within a file then FILE
*        is returned blank.
*
*     VALUE = CHARACTER * ( * ) (Returned)
*        The requested item of information. If the supplied character
*        variable is too short, the string is truncated. If an error
*        occurs a blank value is returned.
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
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Read)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Arguments Given:
      INTEGER IGRP
      INTEGER INDEX
      CHARACTER ITEM*(*)

*  Arguments Returned:
      CHARACTER VALUE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Constants:
      INTEGER SZITEM             ! Max. length of an item name.
      PARAMETER ( SZITEM = 4 )

*  Local Variables:
      INTEGER DEPTH              ! No. of levels of indirection at
                                 ! which the name was specified.
      INTEGER IFILE              ! Index of the file name within the
                                 ! FILES array (see routine GRP1_PTIND).
      CHARACTER LITEM*(SZITEM)   ! Local copy of ITEM.
      INTEGER MODGRP             ! Identifier of group used as basis
                                 ! for modification element.
      INTEGER MODIND             ! Index of name within MODGRP from
                                 ! which the requested name was derived.
      CHARACTER NAME*(GRP__SZNAM)! The name.
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.

*.

*  Ensure that a blank value is returned if an error condition
*  exists on entry.
      VALUE =  ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If the index is outside the bounds of the group, report an error.
      IF( INDEX .LE. 0 .OR. INDEX .GT. CMN_GSIZE( SLOT ) ) THEN
         VALUE = ' '

         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'I', INDEX )
         CALL MSG_SETI( 'S', CMN_GSIZE( SLOT ) )
         CALL ERR_REP( 'GRP_INFOC_ERR1',
     :               'GRP_INFOC: Group index (^I) out of bounds [1,^S]',
     :                 STATUS )


*  Otherwise get the information.
      ELSE
         CALL GRP1_GTELM( SLOT, INDEX, NAME, DEPTH, IFILE, MODGRP,
     :                    MODIND, STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN

*  Store the relevant item.
            LITEM = ITEM
            CALL CHR_UCASE( LITEM )

            IF( LITEM( : 4 ) .EQ. 'NAME' ) THEN
               VALUE = NAME
               IF( CMN_UPPER( SLOT ) ) CALL CHR_UCASE( VALUE )

            ELSE IF( LITEM( : 4 ) .EQ. 'FILE' ) THEN
               CALL GRP1_GTIND( SLOT, IFILE, VALUE, STATUS )

            ELSE
               STATUS = GRP__BADIT
               CALL MSG_SETC( 'ITEM', ITEM )
               CALL ERR_REP( 'GRP_INFOC_ERR2',
     :      'GRP_INFOC: Unknown item of information requested - ^ITEM.',
     :                       STATUS )
            END IF

         END IF

      END IF

*  If an error occurred, give a context message and ensure a blank value
*  is returned.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'IT', ITEM )
         CALL ERR_REP( 'GRP_INFOC_ERR3',
     :      'GRP_INFOC: Unable to get the "^IT" attribute of a group '//
     :      'name.' , STATUS )
         VALUE = ' '
      END IF

      END
