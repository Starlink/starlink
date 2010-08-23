      SUBROUTINE KPG1_PGESC( TEXT, STATUS )
*+
*  Name:
*     KPG1_PGESC

*  Purpose:
*     Removes PGPLOT escape sequences from a text string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  KPG1_PGESC( TEXT, STATUS )

*  Description:
*     This routine removes PGPLOT escape sequences form a text string.
*     Any "\" characters in the string are removed, together with the
*     character following each "\".

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given and Returned)
*        The text string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1998 (DSB):
*        Original version.
*     15-APR-2005 (PWD):
*        Parameterise the PGPLOT escape character for better portability.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Local Constants:
      CHARACTER ESC*1            ! The PGPLOT escape character
*  Some compilers need '\\' to get '\', which isn't a problem as Fortran
*  will truncate the string '\\' to '\' on the occasions when that isn't
*  needed.
      PARAMETER( ESC = '\\' )

*  Arguments Given and Returned:
      CHARACTER TEXT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER C*1              ! Current character
      INTEGER IR                 ! Index of next character to be read
      INTEGER IW                 ! Index of next character to be written
      INTEGER TLEN               ! Significant length of supplied string

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the index of the next character to be read.
      IR = 1

*  Initialise the index of the next character to be written.
      IW = 1

*  Save the used length of the supplied string.
      TLEN = CHR_LEN( TEXT )

*  Loop round until all potentially significant characters have been read.
      DO WHILE( IR .LE. TLEN )

*  Save the current character.
         C = TEXT( IR : IR )

*  If it is not an escape character, store the read character in the
*  returned string, and increment the index of the next character to be
*  written.
         IF( C .NE. ESC ) THEN
            TEXT( IW : IW ) = C
            IW = IW + 1
*  If it is an escape character, do not store it in the string, but
*  increment the index of the next character to be read in order to skip the
*  character following the esscape character.
         ELSE
            IR = IR + 1
         END IF

*  Move on to read the next character.
         IR = IR + 1

      END DO

*  Fill the remainder of the string with spaces.
      TEXT( IW : ) = ' '

      END
