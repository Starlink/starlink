      SUBROUTINE CCD1_APPND( STR1, STR2, LEN2, STATUS )
*+
*  Name:
*     CCD1_APPND

*  Purpose:
*     Copies one string into another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_APPND( STR1, STR2, LEN2, STATUS )

*  Description:
*     The string STR1 (or as much of it as there is room for) is
*     copied into the part of STR2 beginning at position LEN2+1.
*     LEN2 is updated to indicate the final length of STR2 after
*     this operation.

*  Arguments:
*     STR1  =  CHARACTER * ( * ) (Given)
*        Input string to be copied into the string that is to be
*        modified.
*     STR2  =  CHARACTER * ( * ) (Given and Returned)
*        String to be updated by adding the contents of STR1.
*     LEN2  =  INTEGER (Given and Returned)
*        The current position of the last non-blank character in STR2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1983-1984, 1988, 1991 Science & Engineering
*     Research Council. All Rights Reserved.

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
*     Sid Wright (UCL)
*     A. C. Davenhall (ROE)
*     AJC: Alan Chipperfield (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     07-Mar-1983:  Original.                                (UCL::SLW)
*     06-Nov-1984:  Documentation improved.                  (ROE::ACD)
*     13-Sep-1988:  Use LEN instead of CHR_SIZE
*                   Improve documentation                    (RAL::AJC)
*     29-OCT-1991 (PDRAPER):
*        Removed CHR_LEN used LEN instead.
*     29-OCT-1991 (PDRAPER):
*        Changed to standard prologue
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER STR1 * ( * )

*  Arguments Given and Returned:
      CHARACTER STR2 * ( * )
      INTEGER LEN2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SIZE1              ! Length of STR1
      INTEGER SIZE2              ! Length of STR2

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the length of the destination string
      SIZE2 = LEN( STR2 )

*  Get length of string to copy, truncate the possible number of
*  characters appended to lie within the maximum length of output
*  string.
      SIZE1 = MIN( LEN( STR1 ), SIZE2 - LEN2 )

*  Append STR1 to STR2.
      STR2( LEN2 + 1 : LEN2 + SIZE1 ) = STR1( 1 : SIZE1 )

*  Update the length of STR2.
      LEN2 = LEN2 + SIZE1

      END
* $Id$
