      SUBROUTINE SUBPAR_PROMT ( NAMECODE, PROMPT, STATUS )
*+
*  Name:
*     SUBPAR_PROMT

*  Purpose:
*     set a new prompt string for a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PROMT ( NAMECODE, PROMPT, STATUS )

*  Description:
*     Replace the prompt string for the indicated parameter by the given
*     string. This implements the SSE parameter system routine, but
*     without any text processing being done on the string.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        index to the parameter
*     PROMPT=CHARACTER*(*) (given)
*        the new prompt string
*     STATUS=INTEGER

*  Algorithm:
*     Put the given string into the common-block array holding prompts.

*  Implementation Deficiencies:
*     In the original SSE spec, token interpretation was done on the
*     prompt string using the MSG_ routines.

*  Copyright:
*     Copyright (C) 1985, 1993 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     25-NOV-1985 (BDK):
*        Original
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      INTEGER NAMECODE            ! pointer to the parameter

      CHARACTER*(*) PROMPT        ! the prompt string


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      PARPROM(NAMECODE) = PROMPT

      END
