      LOGICAL FUNCTION CCD1_HVCON( LINE, STATUS )
*+
*  Name:
*     CCD1_HVCON

*  Purpose:
*     To check if a given line is terminated by a continuation
*     character.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CCD1_HVCON( LINE )

*  Description:
*     The routine checks to see if the last non - blank character
*     in the given string is the continuation character.

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        The character buffer which is to be checked for the presence of
*        a continuation character
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     CCD1_HVCON = LOGICAL
*        Set true if a continuation character has been located.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1992 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) LINE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Length of string exluding trailing
                                 ! blanks

*  Local Variables:
      INTEGER IAT                ! Position of last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the position of the last non-blank character.
      IAT = MAX( 1, CHR_LEN( LINE ) )

*  Is it the continuation character ?
      IF ( LINE( IAT : IAT ) .EQ. '-') THEN
         CCD1_HVCON = .TRUE.
      ELSE
         CCD1_HVCON = .FALSE.
      END IF

      END
* $Id$
