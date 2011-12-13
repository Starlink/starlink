      SUBROUTINE GRP_INFOI( IGRP, INDEX, ITEM, VALUE, STATUS )
*+
*  Name:
*     GRP_INFOI

*  Purpose:
*     Retrieve an item of integer information about a name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_INFOI( IGRP, INDEX, ITEM, VALUE, STATUS )

*  Description:
*     This routine returns an item of integer information about a
*     single name from a group. The item can be any one of those
*     described under argument ITEM.
*
*     It can also return the current number of active GRP identifiers.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP group identifier.
*     INDEX = INTEGER (Given)
*        An index within the group specified by IGRP.  If the supplied
*        value is outside the bounds of the group, then a "null" value
*        is returned for VALUE as described below, and an error is
*        reported.
*     ITEM = CHARACTER * ( * ) (Given)
*        The name of an item of information. This can be any of the
*        following (abbreviations are not allowed):
*
*        MODGRP  -  If the name was specified by means of a
*        modification element, then the the GRP identifier of the group
*        used as a basis for the modification element is returned in
*        VALUE. If the name was not specified by a modification
*        element, then GRP__NOID is returned.  If INDEX is outside the
*        bounds of the group, then a value of GRP__NOID is returned.
*
*        MODIND  -  If the name was specified by means of a
*        modification element, then the index of the original name
*        (upon which the returned name was based) is returned in VALUE.
*        This is an index into the group identified by MODGRP. If
*        MODGRP is returned equal to GRP__NOID, then MODIND will be
*        zero.
*
*        DEPTH  -  The number of levels of indirection at which the
*        name was specified is returned in VALUE. Names given
*        explicitly within a group expression have a DEPTH value of
*        zero. Names given explicitly within a DEPTH zero indirection
*        element have DEPTH 1. Names given explicitly within a DEPTH 1
*        indirection element, have DEPTH 2, etc. If INDEX is out of
*        bounds, then zero is returned.
*
*        NGRP - The number of GRP group identifiers currently in use.
*        The values of the IGRP and INDEX arguments are ignored.
*
*        ACTIVE - Like "NGRP", except that in addition to returning the
*        number of GRP group identifiers currently in use, the numerical
*        identifier values are also listed on standard output (using
*        MSG_OUT).
*
*        ALLSIMPLE - A value of 1 is returned if all elements within the
*        group were specified explicitly (i.e. by a literal name rather
*        than by indirection or modification). Otherwise a value of zero
*        is returned. The value of the INDEX argument is ignored.
*
*     VALUE = INTEGER (Returned)
*        The requested item of information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     29-SEP-2005 (DSB):
*        Added NGRP item.
*     9-JAN-2006 (DSB):
*        Added ACTIVE item.
*     21-OCT-2009 (DSB):
*        Added ALLSIMPLE item.
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
*        CMN_CHK( GRP__MAXG ) = INTEGER (Read)
*           The GRP identifier associated with each group.
*        CMN_USED( GRP__MAXG ) = LOGICAL (Read)
*           Whether each slot in the common arrays is currently in use.

*  Arguments Given:
      INTEGER IGRP
      INTEGER INDEX
      CHARACTER ITEM*(*)

*  Arguments Returned:
      INTEGER VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Constants:
      INTEGER SZITEM             ! Max. length of an item name.
      PARAMETER ( SZITEM = 9 )

*  Local Variables:
      INTEGER DEPTH              ! No. of levels of indirection at
                                 ! which the name was specified.
      INTEGER IEL                ! Element index.
      INTEGER IFILE              ! The index within the groups FILES
                                 ! array at which the indirection file
                                 ! name is stored.
      CHARACTER LITEM*(SZITEM)   ! Local copy of ITEM.
      INTEGER MODGRP             ! Identifier of group used as basis
                                 ! for modification element.
      INTEGER MODIND             ! Index of name within MODGRP from
                                 ! which the requested name was derived.
      CHARACTER NAME*(GRP__SZNAM)! The name.
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.

*.

*  Set up null values.
      DEPTH = 0
      MODGRP = GRP__NOID
      MODIND = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get an upper case version of the item label.
      LITEM = ITEM
      CALL CHR_UCASE( LITEM )

*  The NGRP and ACTIVE items are special in that they do not use the
*  supplied GRP identifier or name index, so check for these now, before
*  we check the validity of the supplied GRP identifier.
      IF( LITEM( : 4 ) .EQ. 'NGRP' ) THEN

*  For NGRP, just count the number of used slots in the common arrays.
         VALUE = 0
         DO SLOT = 1, GRP__MAXG
            IF( CMN_USED( SLOT ) ) VALUE = VALUE + 1
         END DO

*  All done, so jump to the end.
         GO TO 999

      ELSE IF( LITEM( : 6 ) .EQ. 'ACTIVE' ) THEN

*  For ACTIVE, also display the identifier values.
         CALL MSG_SETC( 'ID', ' ' )
         VALUE = 0
         DO SLOT = 1, GRP__MAXG
            IF( CMN_USED( SLOT ) ) THEN
               VALUE = VALUE + 1
               IF( VALUE .GT. 1 ) CALL MSG_SETC( 'ID', ', ' )
               CALL MSG_SETI( 'ID', CMN_CHK( SLOT ) )
            END IF
         END DO

         IF( VALUE .GT. 1 ) THEN
            CALL MSG_SETI( 'N', VALUE )
            CALL MSG_OUT( ' ', 'There are ^N currently active GRP '//
     :                    'identifiers: ^ID', STATUS )

         ELSE IF( VALUE .EQ. 1 ) THEN
            CALL MSG_OUT( ' ', 'There is 1 currently active GRP '//
     :                    'identifier: ^ID', STATUS )

         ELSE
            CALL MSG_OUT( ' ', 'There are no currently active GRP '//
     :                    'identifiers', STATUS )
         END IF

*  All done, so jump to the end.
         GO TO 999
      END IF

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  The ALLSIMPLE item ignored the INDEX value, so handle it now before we
*  check the validity of INDEX.
      IF( LITEM( : 9 ) .EQ. 'ALLSIMPLE' ) THEN

*  Assume all elements are simple.
         VALUE = 1

*  Loop round all elements. Break out of the loop early if an element is
*  found that is not simple.
         IEL = 1
         DO WHILE( IEL .LE. CMN_GSIZE( SLOT ) )

*  Get infor,mation about the element.
            CALL GRP1_GTELM( SLOT, IEL, NAME, DEPTH, IFILE, MODGRP,
     :                       MODIND, STATUS )

*  If it was specified by indirection or modification, set the returned
*  value to zero and set "I" so that the loop will exit.
            IF( MODIND .NE. 0 .OR. DEPTH .NE. 0 ) THEN
               VALUE = 0
               IEL = CMN_GSIZE( SLOT ) + 1

*  If the element was simple, advance to check the next element.
            ELSE
               IEL = IEL + 1
            END IF
         END DO

*  If the index is outside the bounds of the group, report an error.
      ELSE IF( INDEX .LE. 0 .OR. INDEX .GT. CMN_GSIZE( SLOT ) ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'I', INDEX )
         CALL MSG_SETI( 'S', CMN_GSIZE( SLOT ) )
         CALL ERR_REP( 'GRP_INFOI_ERR1', 'GRP_INFOI: Group index (^I)'//
     :                 ' out of bounds [1,^S]', STATUS )

*  Otherwise get the information from the group.
      ELSE
         CALL GRP1_GTELM( SLOT, INDEX, NAME, DEPTH, IFILE, MODGRP,
     :                    MODIND, STATUS )

      END IF

*  If all has gone OK, store the relevant item.
      IF( STATUS .EQ. SAI__OK ) THEN

         IF( LITEM( : 6 ) .EQ. 'MODGRP' ) THEN
            VALUE = MODGRP

         ELSE IF( LITEM( : 6 ) .EQ. 'MODIND' ) THEN
            VALUE = MODIND

         ELSE IF( LITEM( : 5 ) .EQ. 'DEPTH' ) THEN
            VALUE = DEPTH

         ELSE IF( LITEM( : 9 ) .NE. 'ALLSIMPLE' ) THEN
            STATUS = GRP__BADIT
            CALL MSG_SETC( 'ITEM', ITEM )
            CALL ERR_REP( 'GRP_INFOI_ERR2', 'GRP_INFOI: Unknown item '//
     :                    'of information requested - ^ITEM.', STATUS )
         END IF

      END IF

*  If an error occurred, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'IT', ITEM )
         CALL ERR_REP( 'GRP_INFOI_ERR3', 'GRP_INFOI: Unable to get '//
     :                 'the "^IT" attribute of a group name.' , STATUS )
      END IF

      END
