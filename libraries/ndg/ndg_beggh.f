      SUBROUTINE NDG_BEGGH( STATUS )
*+
*  Name:
*     NDG_BEGGH

*  Purpose:
*     Begin a GRP NDF history block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_BEGGH( STATUS )

*  Description:
*     This routine should be called to mark the start of a GRP NDF 
*     history block. The block should be ended by a matching 
*     call to NDG_ENDGH. See NDG_ENDGH for more details.
*
*     Note - GRP NDF history blocks must not be nested.

*  Arguments:
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
*     16-OCT-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Global Variables:
      INTEGER DHKMP              ! KeyMap holding NDF to which default 
                                 ! history has been written.
      INTEGER GHKMP              ! KeyMap holding GRP group contents
      COMMON /NDG_GH/ GHKMP, DHKMP

*  External References:
      EXTERNAL NDG1_HNDLR

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Indicate that the routine NDG1_HNDLR should be called whenever default
*  history has been written to an NDF.
      CALL NDF_HNDLR( 'DEF_HISTORY', NDG1_HNDLR, .TRUE., STATUS )

*  Create a AST KeyMap to hold integer identifiers for deep copies of the
*  registered GRP groups. The key for each entry is the associated
*  parameter name.
      GHKMP = AST_KEYMAP( ' ', STATUS )

*  Create a AST KeyMap to hold the paths to the NDFs to which default
*  history has been written.
      DHKMP = AST_KEYMAP( ' ', STATUS )

      END
