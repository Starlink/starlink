      SUBROUTINE CCD1_NXWRD( STRING, OFFSET, FIRST, LAST, NOTFND,
     :                       STATUS )
*+
*  Name:
*     CCD1_NXWRD

*  Purpose:
*     To find the next word in a string

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NXWRD( STRING, OFFSET, FIRST, LAST, NOTFND, STATUS )

*  Description:
*     The routine looks for the start of the next word, after OFFSET
*     characters, in the given string. Words are assumed to be
*     delimetered by spaces, commas or tabs. The routine is really a
*     wrap round calls to CHR_FIWE and CHR_FIWS trapping any status
*     returns.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be searched for a 'word'. The word is looked for
*        in STRING(OFFSET:).
*     OFFSET = INTEGER (Given)
*        The offset into the given string after which the word is to
*        located.
*     FIRST = INTEGER (Returned)
*        First character of the located word. Offset into STRING.
*     LAST = INTEGER (Returned)
*        Last character of the located word. Offset into STRING.
*     NOTFND = LOGICAL (Returned)
*        If a next word is not located then this is set true, otherwise
*        it is set false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1991 (PDRAPER):
*        Original version.
*     5-FEB-1992 (PDRAPER):
*        Changed to CCD1_ routine from ARD original.
*     12-SEP-1997 (PDRAPER):
*        Modified to return a word that extends to the end of 
*        the string correctly.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHR_ERR'          ! CHR status values.

*  Arguments Given:
      CHARACTER * ( * ) STRING
      INTEGER OFFSET

*  Arguments Returned:
      INTEGER FIRST
      INTEGER LAST
      LOGICAL NOTFND

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Word is not found by default.
      NOTFND = .TRUE.

*  Look for the first character of the word - first non delimeter.
      FIRST = OFFSET 
      CALL CHR_FIWS( STRING, FIRST, STATUS )

*  Check for error and report is an error for this stage.
      IF ( STATUS .EQ. CHR__ENDOFSENT .OR. STATUS .EQ. CHR__WRDNOTFND )
     :   THEN
            CALL ERR_ANNUL( STATUS )
      ELSE

*  Look for end of word. Note end of sentence isn't an error now (since
*  we have found the start of the word).
         LAST = FIRST 
         CALL CHR_FIWE( STRING, LAST, STATUS )
         IF ( STATUS .NE. CHR__WRDNOTFND ) NOTFND = .FALSE.
         IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
      END IF
      END 
* $Id$
