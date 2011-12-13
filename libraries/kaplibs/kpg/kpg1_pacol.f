      SUBROUTINE KPG1_PACOL( PNCOL, LP, UP, COLIND, STATUS )
*+
*  Name:
*     KPG1_PACOL

*  Purpose:
*     Obtains a marker colour.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PACOL( PNCOL, LP, UP, COLIND, STATUS )

*  Description:
*     This routine obtains a colour index. A string is obtained from the
*     parameter system.  The interpretation of this string provides a
*     number of ways to specify the colour index requested.  The options
*     are as follows.
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

*  Arguments:
*     PNCOL = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter to obtain the marker colour.
*        It should have type LITERAL.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1998 (DSB):
*        Original version, based on KPG1_MACOL by MJC.
*     25-MAY-2009 (DSB):
*        Remove the PAR_ restrictions on the acceptable characters that
*        can be included in the parameter value. Document HTML colour
*        code option.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PNCOL
      INTEGER LP
      INTEGER UP

*  Arguments Returned:
      INTEGER COLIND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COLOUR * ( 18 )  ! The colour obtained from the
                                 ! parameter system
      LOGICAL LOOP               ! Retry for a colour?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the colour-index bounds.
      IF ( LP .GT. UP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'LP', LP )
         CALL MSG_SETI( 'UP', UP )
         CALL ERR_REP( 'KPG1_PACOL_CIBOUND',
     :     'Upper colour-index bound (^UP) is less than lower bound '/
     :     /'(^LP).', STATUS )
         GOTO 999
      END IF

*  Loop until a permitted value has been obtained, or until an error
*  occurred.
      LOOP = .TRUE.
      DO WHILE ( STATUS .EQ. SAI__OK .AND. LOOP )

*  Obtain the string from the parameter system.
         CALL PAR_GET0C( PNCOL, COLOUR, STATUS )

*  Check that a value was obtained.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Remove blanks and convert to uppercase.
            CALL CHR_RMBLK( COLOUR )
            CALL CHR_UCASE( COLOUR )

*  Convert the string into an integer colour index.
            CALL KPG1_PGCOL( COLOUR, LP, UP, COLIND, STATUS )

*  Try again if the colour was not recognised.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )

*  Exit the loop.
            ELSE
               LOOP = .FALSE.
            END IF

*  Cancel the parameter when there is going to be another attempt to
*  obtain a value.
            IF ( LOOP ) CALL PAR_CANCL( PNCOL, STATUS )
         END IF
      END DO

  999 CONTINUE

      END
