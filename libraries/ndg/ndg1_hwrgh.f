      SUBROUTINE NDG1_HWRGH( INDF, STATUS )
*+
*  Name:
*     NDG1_HWRGH

*  Purpose:
*     Write GRP history to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_HWRGH( INDF, STATUS )

*  Description:
*     This routine appends a description of each currently registered GRP
*     group to the current History record in the supplied NDF. It then
*     unregisters each group. See NDF_ENDGH.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-OCT-2009 (DSB):
*        Original version (taken from NDG_ENDGH).
*     19-AUG-2010 (DSB):
*        Remove each group from the KeyMap after it has been written out.
*     21-AUG-2010 (DSB):
*        Now that entries are removed from the KeyMap, we need to access
*        keymap entry number 1 every time.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'GRP_PAR'

*  Global Variables:
      INCLUDE 'NDG_COM2'         ! Global GRP history information

*  Arguments Given:
      INTEGER INDF

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN

*  Local Constants:
      INTEGER MXLINE
      PARAMETER ( MXLINE = 20 )

*  Local Variables:
      CHARACTER ELEM*(GRP__SZNAM)
      CHARACTER LINES( MXLINE )*(NDF__SZHMX)
      CHARACTER PARAM*(AST__SZCHR)
      INTEGER ELEN
      INTEGER I
      INTEGER IAT
      INTEGER IEL
      INTEGER IGRP
      INTEGER ILINE
      INTEGER IPAR
      INTEGER JAT
      INTEGER NC
      INTEGER NEL
      INTEGER NLEFT
      INTEGER NPAR
      INTEGER NREM
*.

*  Check inherited status
      IF( STATUS .NE. SAI__OK ) RETURN

*  Loop round every entry in the GRP NDF history keymap.
      NPAR = AST_MAPSIZE( GHKMP_COM2, STATUS )
      DO IPAR = 1, NPAR

*  Since the current KeyMap entry is removed at the end of this do loop,
*  all later entries move down one slot, so we always access entry
*  number 1.
         PARAM = AST_MAPKEY( GHKMP_COM2, 1, STATUS )
         IF( AST_MAPGET0I( GHKMP_COM2, PARAM, IGRP, STATUS ) ) THEN

*  Initialise the first line to hold the parameter name.
            ILINE = 1
            LINES( 1 ) = ' '
            IAT = 0
            CALL CHR_APPND( 'Group:', LINES( 1 ), IAT )
            IAT = IAT + 1
            CALL CHR_APPND( PARAM, LINES( 1 ), IAT )
            CALL CHR_APPND( '="', LINES( 1 ), IAT )

*  Loop round every element in the group.
            CALL GRP_GRPSZ( IGRP, NEL, STATUS )
            DO IEL = 1, NEL
               CALL GRP_GET( IGRP, IEL, 1, ELEM, STATUS )

*  Get the total length of the element and initialise the number of
*  element characters still to be appended to the history text, and the
*  index of the next element character to write.
               ELEN = CHR_LEN( ELEM )
               NLEFT = ELEN
               JAT = 1

*  Loop round until the whole element has been appended to the history
*  text, or we have filled the array of lines.
               DO WHILE( NLEFT .GT. 0 .AND. ILINE .LE. MXLINE )

*  Determine how much room is left at the end of the current line.
                  NREM = NDF__SZHMX - IAT

*  If this is zero, start a new line.
                  IF( NREM .LE. 0 ) THEN
                     ILINE = ILINE + 1
                     IAT = 0
                     NREM = NDF__SZHMX
                  END IF

*  If we have filled the array of history text lines, append an ellipsis
*  to the end of the last line.
                  IF( ILINE .GT. MXLINE ) THEN
                     LINES( MXLINE )( NDF__SZHMX - 4 : ) =
     :                                                   ' ...'

*  Otherwise, append as much of the element as possible to the end of the
*  current line.
                  ELSE

*  Note the number of characters to write.
                     NC = MIN( NLEFT, NREM )
*  Write them.
                     CALL CHR_APPND( ELEM( JAT:JAT + NC - 1 ),
     :                               LINES( ILINE ), IAT )

*  Update the number of characters left to be written.
                     NLEFT = NLEFT - NC

*  Update the index of the next character to be written.
                     JAT = JAT + NC
                  END IF
               END DO

*  If there is not room to append a comma and space, or a closing quote to
*  the end of the line, start a new line.
               IF( IAT .GE. NDF__SZHMX - 1 ) THEN
                  ILINE = ILINE + 1
                  IAT = 0
               END IF

*  Check we have not filled the array of lines.
               IF( ILINE .LE. MXLINE ) THEN

*  Append a double quote if this is the last element in the group, or a
*  comma otherwise.
                  IF( IEL .EQ. NEL ) THEN
                     CALL CHR_APPND( '"', LINES( ILINE ), IAT )
                  ELSE
                     CALL CHR_APPND( ',', LINES( ILINE ), IAT )
                     IAT = IAT + 1
                  END IF

               END IF
            END DO

*  Delete the group and remove the entry from the KeyMap so that it cannot
*  be written out again.
            CALL GRP_DELET( IGRP, STATUS )
            CALL AST_MAPREMOVE( GHKMP_COM2, PARAM, STATUS )
         END IF

*  Append the array of lines describing the contents of the expanded group
*  to the current history record.
         CALL NDF_HPUT( ' ', '<APPEND>', .FALSE., ILINE, LINES, .FALSE.,
     :                  .FALSE., .FALSE., INDF, STATUS )
      END DO

      END
