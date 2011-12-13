      SUBROUTINE CCD1_SETSP( IGNDF, KEY, MAXOG, OGNDF, NOGRP, OGKEY,
     :                       STATUS )
*+
*  Name:
*     CCD1_SETSP

*  Purpose:
*     Splits a group of NDFs into related groups.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_SETSP( IGNDF, KEY, MAXOG, OGNDF, NOGRP, OGKEY, STATUS )

*  Description:
*     This routine takes an NDG group of NDFs and divides it into an
*     array of subgroups according to Set membership characteristics.
*     Two types of splitting are possible, according to the value
*     of the KEY argument: if KEY='NAME' they are split so that all
*     members of each output group have the same value of the
*     .CCDPACK.MORE.SET.NAME extension item, and if KEY='INDEX'
*     then all members of each output group have the same value
*     of the .CCDPACK.MORE.SET.INDEX extension item.  NDFs with
*     no .CCDPACK.MORE.SET item are considered to have a NAME of
*     ' ' and INDEX of 0, so any NDFs which have not been put into
*     any Set will all group together.
*
*     If KEY='NONE' then the input group will not be reordered; a
*     single output group, a copy of the input group, will be returned.

*  Arguments:
*     IGNDF = INTEGER (Given)
*        The NDG identifier of a group of NDFs to be split into
*        subgroups.
*     KEY = CHARACTER * ( * ) (Given)
*        The name of the Set characteristic on which the groups should
*        be split: either 'NAME', 'INDEX' or 'NONE'.
*     MAXOG = INTEGER (Given)
*        The maximum number of output groups which may result from the
*        splitting operation.  If there are more distinct keys than
*        this in the members of IGNDF an error will result.
*     OGNDF( MAXOG ) = INTEGER (Returned)
*        The first NOGRP returned elements of this array will be NDG
*        identifiers of groups of NDFs grouped according to the
*        specified key.
*     NOGRP = INTEGER (Returned)
*        The number of output groups into which the input group was split
*        (and hence the number of elements returned in OGNDF).  If
*        KEY='NONE' this will be 1.
*     OGKEY = INTEGER (Returned)
*        A group whose I'th element contains the the key value
*        (Set NAME or INDEX characteristic) common to the NDFs returned
*        in the group in the I'th element of OGNDF.  If KEY='INDEX'
*        the index is converted to a string in the normal way to
*        for the name.  If KEY='NONE' the first element will be an
*        empty string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Arguments Given:
      INTEGER IGNDF
      CHARACTER * ( * ) KEY
      INTEGER MAXOG

*  Arguments Returned:
      INTEGER OGNDF( MAXOG )
      INTEGER NOGRP
      INTEGER OGKEY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER GOTKEY             ! Key group index matching current key value
      INTEGER I                  ! Loop index
      INTEGER INDF               ! NDF identifier for current NDF
      INTEGER IOUT               ! Index of current NDF in output group
      INTEGER JFRM               ! Frame index (dummy)
      INTEGER NCHAR              ! Number of characters in string
      INTEGER NIGRP              ! The number of items in the input group
      INTEGER NOUT               ! Number of entries in output group
      INTEGER SINDEX             ! Value of Set INDEX characteristic
      CHARACTER * ( GRP__SZNAM ) FIELDS( 6 ) ! NDG supplemental information
      CHARACTER * ( GRP__SZNAM ) KEYVAL ! Value of key for current NDF
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of NDF
      CHARACTER * ( GRP__SZNAM ) SNAME ! Value of Set NAME characteristic

*.

*  Initialise error returned values.
      NOGRP = 0
      OGKEY = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the number of items in the input group.
      NIGRP = 0
      CALL GRP_GRPSZ( IGNDF, NIGRP, STATUS )

*  Initialise a group to hold the key values.
      CALL GRP_NEW( 'CCD:SETKEYS', OGKEY, STATUS )

*  Treat the case of no sort key specially.
      IF ( KEY .EQ. 'NONE' ) THEN

*  There will only be a single output group.
         NOGRP = 1

*  Create the output group which is a copy of the input group.
         CALL GRP_COPY( IGNDF, 1, NIGRP, .FALSE., OGNDF( 1 ), STATUS )

*  Copy the supplementary information from the input to the output group.
         DO I = 1, NIGRP
            CALL NDG_GTSUP( IGNDF, I, FIELDS, STATUS )
            CALL NDG_PTSUP( OGNDF( 1 ), I, FIELDS, STATUS )
         END DO

*  Enter a blank string as the only entry in the keys group.
         CALL GRP_PUT( OGKEY, 1, ' ', 1, STATUS )

*  Treat the case in which grouping is by Set Index or Set Name.
      ELSE IF ( KEY .EQ. 'INDEX' .OR. KEY .EQ. 'NAME' ) THEN

*  Loop over all items in the input group.
         DO I = 1, NIGRP

*  Get the NDF identifier for the current item.
            CALL NDG_NDFAS( IGNDF, I, 'READ', INDF, STATUS )

*  Get the NDF's Set characteristics.
            CALL CCD1_SETRD( INDF, AST__NULL, SNAME, SINDEX, JFRM,
     :                       STATUS )

*  Annul the NDF identifier.
            CALL NDF_ANNUL( INDF, STATUS )

*  Construct the appropriate key.
            KEYVAL = ' '
            NCHAR = 1
            IF ( KEY .EQ. 'NAME' ) THEN
               CALL CHR_CTOC( SNAME, KEYVAL, NCHAR )
               NCHAR = MAX( 1, NCHAR )
            ELSE IF ( KEY .EQ. 'INDEX' ) THEN
               CALL CHR_ITOC( SINDEX, KEYVAL, NCHAR )
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ',
     :'CCD1_SETSP: Invalid KEY value (programming error)', STATUS )
            END IF

*  Locate the key in the group of keys encountered so far.
            IF ( NOGRP .GT. 0 ) THEN
               CALL GRP_INDEX( KEYVAL( 1:NCHAR ), OGKEY, 1, GOTKEY,
     :                         STATUS )
            ELSE
               GOTKEY = 0
            END IF

*  If we have not encountered this key before, initialise a new group
*  and copy this NDF into it.  Doing it this way rather than creating
*  a new group with GRP_NEW ensures that the new group inherits
*  control characters etc of the old group.
            IF ( GOTKEY .EQ. 0 ) THEN
               NOGRP = NOGRP + 1

*  Check that we have not exceeded the maximum number of output groups.
               IF ( NOGRP .GT. MAXOG ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'MAXOG', MAXOG )
                  CALL ERR_REP( 'CCD1_SETSP_MAXOG',
     :'CCD1_SETSP: Too many output groups (maximum is ^MAXOG)', STATUS )
                  GO TO 99
               END IF

*  Create a new group with the new entry in it.
               GOTKEY = NOGRP
               IOUT = 1
               CALL GRP_COPY( IGNDF, I, I, .FALSE., OGNDF( GOTKEY ),
     :                        STATUS )
               CALL GRP_PUT( OGKEY, 1, KEYVAL( 1:NCHAR ), NOGRP,
     :                       STATUS )

*  If we have encountered this group before, just copy this NDF into
*  it.
            ELSE
               CALL GRP_GET( IGNDF, I, 1, NDFNAM, STATUS )
               CALL GRP_GRPSZ( OGNDF( GOTKEY ), NOUT, STATUS )
               IOUT = NOUT + 1
               CALL GRP_PUT( OGNDF( GOTKEY ), 1, NDFNAM, IOUT, STATUS )
            END IF

*  Copy supplementary information from the input NDG item to the
*  output one.
            CALL NDG_GTSUP( IGNDF, I, FIELDS, STATUS )
            CALL NDG_PTSUP( OGNDF( GOTKEY ), IOUT, FIELDS, STATUS )
         END DO

*  Unrecognised grouping.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'KEY', KEY )
         CALL ERR_REP( 'CCD1_SETSP_BADKEY',
     :                 'CCD1_SETSP: Bad group key ^KEY', STATUS )
      END IF

*  Error exit label.
 99   CONTINUE
      END
* $Id$
