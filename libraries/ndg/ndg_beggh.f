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
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-OCT-2009 (DSB):
*        Original version.
*     16-JAN-2012 (DSB):
*        Record initial value of NDF_AUTO_HISTORY. Prior to this, a
*        default value of .FALSE was used which meant that NDF_AUTO_HISTORY
*        was switched off by the subsequent call to NDG_HLTGH.
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
      INCLUDE 'NDG_COM2'         ! Global GRP history information

*  External References:
      EXTERNAL NDG1_HNDLR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL OLD
      LOGICAL UGHKMP
      LOGICAL UDHKMP

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get sole access to the NDG globals
      CALL NDG1_GLOCK( .TRUE. )

*  Record the initial value of the AUTO_HISTORY tuning parameter.
      CALL NDF_GTUNE( 'AUTO_HISTORY', AUTO_COM2, STATUS )

*  Indicate that NDF event handlers needed to record the NDFs in which
*  GRP history should be stored have not yet been established, and then
*  establish them.
      STATE_COM2 = .FALSE.
      CALL NDG_HLTGH( .TRUE., OLD, STATUS )

*  Create a AST KeyMap to hold integer identifiers for deep copies of the
*  registered GRP groups. The key for each entry is the associated
*  parameter name.
      GHKMP_COM2 = AST_KEYMAP( ' ', STATUS )

*  Create a AST KeyMap to hold the paths to the NDFs to which default
*  history has been written.
      DHKMP_COM2 = AST_KEYMAP( ' ', STATUS )

*  Unlock them so they can be locked for use by another thread.
      CALL NDG1_ALOCK( .FALSE., GHKMP_COM2, UGHKMP, STATUS )
      CALL NDG1_ALOCK( .FALSE., DHKMP_COM2, UDHKMP, STATUS )

*  Allow other threads to access the NDG globals
      CALL NDG1_GLOCK( .FALSE. )

      END
