      SUBROUTINE PARSECON_PAREND ( STATUS )
*+
*  Name:
*     PARSECON_PAREND

*  Purpose:
*     on ENDPARAMETER clear name.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_PAREND ( STATUS )

*  Description:
*     Clear the parameter name from the error report common block.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Set PRNAME to blank.

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
*     A.J.Chipperfield
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16.08.1990: Original (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      PRNAME = ' '

      END
