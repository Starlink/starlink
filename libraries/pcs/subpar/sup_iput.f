      INTEGER FUNCTION SUBPAR_IPUT( TEXT, PROMPT, RESLEN )
*+
*  Name:
*     SUBPAR_IPUT

*  Purpose:
*     To prompt the user for input
*     For use with HELP output routine
*     This is the Unix version

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SUBPAR_IPUT( TEXT, PROMPT, RESLEN )

*  Description:
*     The routine obtains input from the user after prompting.
*     The routine uses ICL_READA to prompt and obtain the response.
*     The paging line counter is reset and the function value set
*     appropriately.

*  Deficiencies:
*     This routine will always return the OK value.

*  Arguments:
*     TEXT = CHARACTER*(*) (Given)
*        Variable to receive the input text
*     PROMPT = CHARACTER*(*) (Returned)
*        The prompt to be used
*     RESLEN = INTEGER (Returned)
*        The used length of the reply

*  Returned Value:
*     SUBPAR_IPUT = INTEGER
*        1 if the routine is successful
*        An error status value if not (ls bit 0)

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1990 (AJC):
*        Original version.
*      3-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      CHARACTER*(*) PROMPT

*  Arguments Returned:
      CHARACTER*(*) TEXT
      INTEGER RESLEN

*  External References:
      INTEGER CHR_LEN            ! Used length of string
      EXTERNAL CHR_LEN

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! For help screen size

*  Local Variables:
      INTEGER PRLEN              ! Prompt string used length

*   Prompt and receive reply
      PRLEN = LEN( PROMPT )
      CALL ICL_READA( PROMPT, PRLEN, PROMPT, PRLEN,
     : TEXT, LEN(TEXT), ' ', 0 )

*   Find the used length of the string
      RESLEN = CHR_LEN( TEXT )

*   Reset line count
      SUBPARLCNT = SUBPARPGSZ

*   Set reply value - always good
      SUBPAR_IPUT = 1

      END

