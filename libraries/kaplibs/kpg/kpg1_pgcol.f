      SUBROUTINE KPG1_PGCOL( COL, LP, UP, COLIND, STATUS )
*+
*  Name:
*     KPG1_PGCOL

*  Purpose:
*     Obtains a marker colour, given a colour specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGCOL( COL, LP, UP, COLIND, STATUS )

*  Description:
*     This routine obtains a PGPLOT colour index to be used in an image
*     display from a supplied string. The interpretation of this string
*     provides a number of ways to specify the colour index requested.
*     The options are as follows.
*
*     - 'MAX'          -- The maximum (non-reserved) colour index, i.e.
*                         the highest colour index used for the display
*                         of an image.
*     - 'MIN'          -- The minimum non-reserved colour index, i.e.
*                         the lowest colour index used for the display
*                         of an image.
*     - An integer     -- The actual colour index. It is constrained
*                         between 0 and the highest colour index.
*     - A named colour -- Uses the named colour from the palette, and if
*                         it is not present, the nearest colour from the
*                         palette is selected.
*     - An HTML code   -- Has the form "#aabbcc" (or "@aabbcc" - for use
*                         in contexts where "#" is a comment character,
*                         e.g. KAPPA style files) where a, b and c are
*                         hexadecimal digits, and "aa", "bb" and "cc"
*                         give red, blue, and green intensities
*                         normalised to a maximum of "ff" (256).
*
*     An error is reported if the string does not conform to any of
*     these formats.

*  Arguments:
*     COL = CHARACTER * ( * ) (Given)
*        The string specifying the required colour.
*     LP = INTEGER (Given)
*        The lowest non-reserved colour index.
*     UP = INTEGER (Given)
*        The highest non-reserved colour index.
*     COLIND = INTEGER (Returned)
*        The colour index of the selected colour.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     - A PGPLOT image-display workstation must be open and active.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-FEB-1998 (DSB):
*        Original version, based on KPG1_MACOL by MJC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) COL
      INTEGER LP
      INTEGER UP

*  Arguments Returned:
      INTEGER COLIND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COLOUR*20

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the colour-index bounds.
      IF ( LP .GT. UP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'LP', LP )
         CALL MSG_SETI( 'UP', UP )
         CALL ERR_REP( 'KPG1_PGCOL_CIBOUND',
     :     'Upper colour-index bound (^UP) is less than lower bound '/
     :     /'(^LP).', STATUS )
         GOTO 999
      END IF

*  Remove leading blanks and convert to uppercase.
      COLOUR = COL
      CALL CHR_LDBLK( COLOUR )
      CALL CHR_UCASE( COLOUR )

*  Test for the various options.
*  =============================

*  MIN causes the minimum value to be used, namely '/'.  This requests
*  the lowest non-reserved colour.
      IF ( COLOUR .EQ. '/' .OR. COLOUR .EQ. 'MIN' ) THEN
         COLIND = LP

*  MAX causes the maximum value to be used, namely '{'.  This requests
*  the highest non-reserved colour.
      ELSE IF ( COLOUR .EQ. '{' .OR. COLOUR .EQ. 'MAX' ) THEN
         COLIND = UP

*  See whether it is an integer.
      ELSE
         CALL ERR_MARK
         CALL CHR_CTOI( COLOUR, COLIND, STATUS )

*  If the conversion was successful use the number as a colour index
*  constrained to be in the full colour table.
         IF ( STATUS .EQ. SAI__OK ) THEN
            COLIND = MIN( UP, MAX( 0, COLIND ) )

*  The value is not numeric so assume that it is a named colour from
*  the palette.  Find the nearest colour from the palette.  First
*  ignore the error.
         ELSE
            CALL ERR_ANNUL( STATUS )
            CALL KPG1_PLCIP( COLOUR, COLIND, STATUS )
         END IF
         CALL ERR_RLSE
      END IF

  999 CONTINUE

      END
