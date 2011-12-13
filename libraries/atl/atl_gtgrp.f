      SUBROUTINE ATL_GTGRP( PARAM, IGRP, STATUS )
*+
*  Name:
*     ATL_GTGRP

*  Purpose:
*     Obtain lines of text from a parameter, and store them in a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_GTGRP( PARAM, IGRP, STATUS )

*  Description:
*     Currently this routine expects the parameter to be associated with:
*
*     1 - a text file (the returned group contains the lines of the file).
*     2 - a FITS file (the returned group contains the FITS headers).
*
*     In future it may be possible to add other ways of using the
*     parameter (i.e. by associating it with objects other than text
*     files).

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     IGRP = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     NG: Norman Gray (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     19-JUN-2001 (NG):
*        Added option to read FITS file headers.
*     19-JUN-2001 (DSB):
*        Re-formatted NGs changes to use ATOOLS coding style.
*     30-MAY-2006 (DSB):
*        Moved into ATL library and changed prefix from "ATL1_" to "ATL_".
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR

*  Local Variables:
      CHARACTER FNAME*120
      CHARACTER FTCARD*80
      CHARACTER GRPEXP*(GRP__SZGEX)
      INTEGER ADDED
      INTEGER FTBKSZ
      INTEGER FTCNUM
      INTEGER FTSTAT
      INTEGER FTUNIT
      INTEGER I
      INTEGER IAT
      INTEGER IDX
      INTEGER IPAR
      INTEGER SIZE
      LOGICAL FLAG
      LOGICAL FTMORE
      LOGICAL ISFITS

*.

*  Initialise.
      IGRP = GRP__NOID

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new group.
      CALL GRP_NEW( ' ', IGRP, STATUS )

*  Get the value of the parameter using SUBPAR to avoid interpretation of
*  the string by the parameter system.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, FNAME, STATUS )

*  Work out if the file is a FITS file, by seeing if it has a
*  .fit or .fits or .fits-and-anything extension
      ISFITS = .FALSE.
      IDX = 0

*  Find last dot in filename
      DO I = 1, LEN( FNAME )
         IF( FNAME( I : I ) .EQ. '.' ) IDX = I
      END DO

*  Check the string following the last dot.
      IF( IDX .GT. 0 .AND. IDX .LT. LEN( FNAME ) ) THEN
         ISFITS = CHR_SIMLR( FNAME( IDX : IDX + 3 ), '.FIT' )
      END IF

*  Create a new group, filling it with the contents of the file header.
      IF( ISFITS ) THEN

*  Open the FITS file.
         FTBKSZ = 1
         FTSTAT = 0
         CALL FTGIOU( FTUNIT, FTSTAT )
         CALL FTOPEN( FTUNIT, FNAME, 0, FTBKSZ, FTSTAT )

*  There was an error opening the file -- perhaps it isn't a FITS
*  file after all.  So set ISFITS to false, so we have another
*  go with the other method.
         IF( FTSTAT .NE. 0 ) THEN
            FTMORE = .FALSE.
            ISFITS = .FALSE.
         END IF

*  Read the header into the GRP group.
         FTCNUM = 1
         DO WHILE( FTMORE )
            CALL FTGREC( FTUNIT, FTCNUM, FTCARD, FTSTAT )
            FTCNUM = FTCNUM + 1
            IF( FTSTAT .NE. 0 ) FTMORE = .FALSE.
            IF( FTCARD( 1 : 3 ) .EQ. 'END' ) FTMORE = .FALSE.
            CALL GRP_PUT( IGRP, 1, FTCARD, 0, STATUS )
            IF( STATUS .NE. SAI__OK ) FTMORE = .FALSE.
         END DO

         CALL FTCLOS( FTUNIT, FTSTAT )

      END IF

*  Rather than an else, here, test ISFITS again, in case it was reset
*  within the previous block.
      IF( .NOT. ISFITS ) THEN

*  Ensure the group is empty.
         CALL GRP_SETSZ( IGRP, 0, STATUS )

*  Form a group expression containing an indirection element which will
*  cause GRP to read the specified file.
         GRPEXP = '^'
         IAT = 1
         CALL CHR_APPND( FNAME, GRPEXP, IAT )

*  Switch off all control characters so that nothing gets interpreted by
*  GRP.
         CALL GRP_SETCC( IGRP, 'COM,DEL,NAM,SEP,OPEN_N,CLOSE_N,FL,'//
     :                   'OPEN_K,CLOSE_K', '%%%%%%%%%', STATUS )

*  Read the file into the group.
         CALL GRP_GRPEX( GRPEXP( : IAT ), GRP__NOID, IGRP, SIZE, ADDED,
     :                   FLAG, STATUS )

      ENDIF

*  Delete the group if an error occurred.
      IF( STATUS .NE. SAI__OK ) CALL GRP_DELET( IGRP, STATUS )

*  Tell the user where the object came from.
      IF( IGRP .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
         IF( SIZE .GT. 0 ) THEN
            CALL MSG_SETC( 'FILE', FNAME )

            IF( ISFITS ) THEN
               CALL MSG_SETC( 'TYP', 'FITS' )
            ELSE
               CALL MSG_SETC( 'TYP', 'text' )
            END IF

            CALL ATL_NOTIF( '   AST data read from ^TYP file '//
     :                       '''^FILE''.', STATUS )
         END IF
      END IF

      END
