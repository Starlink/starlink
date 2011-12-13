      SUBROUTINE KPG1_CTCPK( CI, NAX, GI, NEL, KEYMAP, STATUS )
*+
*  Name:
*     KPG1_CTCPK

*  Purpose:
*     Copies values from catalogue columns to an AST KeyMap.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CTCPK( CI, NAX, GI, NEL, KEYMAP, STATUS )

*  Description:
*     This routine gets NEL values for a set of given CAT (see SUN/181)
*     columns, derived from rows 1 to NEL of a given catalogue,
*     selection, or index, and stores them in a KeyMap.

*  Arguments:
*     CI = INTEGER (Given)
*        The CAT identifier for the catalogue, selection or index
*        containing the required data.
*     NAX= INTEGER (Given)
*        The number of columns from which values are to be read.
*     GI( NAX ) = INTEGER (Given)
*        The CAT identifiers for the column, expressions or parameters
*        to be evaluated for rows 1 to NEL of the component identified
*        by CI.
*     NEL = INTEGER (Given)
*        The number of rows to copy.
*     KEYMAP = INTEGER (Given)
*        The KeyMap. Each column value is stored as a scalar string with
*        key "<colname>_<row index>".
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-MAY-2009 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'CAT_PAR'          ! CAT constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER CI
      INTEGER NAX
      INTEGER GI( NAX )
      INTEGER NEL
      INTEGER KEYMAP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER TEXT*512         ! Buffer for a column value
      CHARACTER KEY*50           ! Key for KeyMap entry
      INTEGER I                  ! Row index
      INTEGER J                  ! Column index
      LOGICAL NULL               ! Was no value available?
      INTEGER TLEN               ! Used length of TEXT
      INTEGER KLEN               ! Used length of KEY
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the buffer
      TEXT = ' '

*  Loop round each row.
      DO I = 1, NEL

*  Read the current row from the catalogue, selection or index into the
*  current row buffer.
         CALL CAT_RGET( CI, I, STATUS )

*  Loop round each column.
         DO J = 1, NAX

*  If this column ID is valid, get the column value.
            IF ( GI( J ) .NE. CAT__NOID ) THEN
               CALL CAT_EGT0C( GI( J ), TEXT, NULL, STATUS )

*  Use a blank value if the value is null.
               IF( NULL ) THEN
                  TEXT = ' '
                  TLEN = 1
               ELSE
                  TLEN = MAX( 1, CHR_LEN( TEXT ) )
               END IF

*  If this column ID is null, store a blank value.
            ELSE
               TEXT = ' '
               TLEN = 1
            END IF

*  Create the KeyMap entry key in the form "<colname>_<row index>" (upper
*  case).
            CALL CAT_TIQAC( GI( J ), 'NAME', KEY, STATUS )
            KLEN = CHR_LEN( KEY )
            CALL CHR_APPND( '_', KEY, KLEN )
            CALL CHR_PUTI( I, KEY, KLEN )
            CALL CHR_UCASE( KEY( : KLEN ) )

*  Add the entry to the KeyMap.
            CALL AST_MAPPUT0C( KEYMAP, KEY( : KLEN ), TEXT( : TLEN ),
     :                         ' ', STATUS )

*  Clear the buffer
            TEXT( : TLEN ) = ' '

         END DO

      END DO

      END
