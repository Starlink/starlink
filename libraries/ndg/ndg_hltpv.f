      SUBROUTINE NDG_HLTPV( NEW, OLD, STATUS )
*+
*  Name:
*     NDG_HLTPV

*  Purpose:
*     Temporarily halt an NDF provenance block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_HLTPV( NEW, OLD, STATUS )

*  Description:
*     This routine can be called to stop subseqently accessed NDFs
*     being added to the list of NDFs that will receive updated
*     provenance information when NDG_ENDPV is called to end the
*     current provenance block.

*  Arguments:
*     NEW = LOGICAL (Read)
*        The new required provenance-recording state.
*     OLD = LOGICAL (Returned)
*        The provenance-recording state on entry to this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - A .FALSE. provenance-recording state means that any subseqently
*     accessed NDFs will not be added to the list of NDFs to receive
*     updated provenance information when NDG_ENDPV is called.
*     - A .TRUE. provenance-recording state means that any subseqently
*     accessed NDFs are added to the list of NDFs to receive updated
*     provenance information when NDG_ENDPV is called.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-OCT-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'NDG_COM1'         ! Global provenance information

*  External References:
      EXTERNAL NDG1_HNDLR

*  Arguments Given:
      LOGICAL NEW

*  Arguments Returned:
      LOGICAL OLD

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Return the old state.
      OLD = STATE_COM1

*  If provenance recording is currently disabled and is to be enabled...
      IF( .NOT. OLD .AND. NEW ) THEN

*  Indicate that the routine NDG1_HNDLR should be called whenever an NDF
*  is opened or closed, or has its data array mapped for read or update
*  access
         CALL NDF_HNDLR( 'READ_EXISTING_NDF', NDG1_HNDLR, .TRUE.,
     :                   STATUS )
         CALL NDF_HNDLR( 'WRITE_EXISTING_NDF', NDG1_HNDLR, .TRUE.,
     :                   STATUS )
         CALL NDF_HNDLR( 'UPDATE_EXISTING_NDF', NDG1_HNDLR, .TRUE.,
     :                   STATUS )
         CALL NDF_HNDLR( 'OPEN_NEW_NDF', NDG1_HNDLR, .TRUE., STATUS )
         CALL NDF_HNDLR( 'CLOSE_NDF', NDG1_HNDLR, .TRUE., STATUS )
         CALL NDF_HNDLR( 'READ_DATA', NDG1_HNDLR, .TRUE., STATUS )
         CALL NDF_HNDLR( 'UPDATE_DATA', NDG1_HNDLR, .TRUE., STATUS )

*  Save the new state
         STATE_COM1 = .TRUE.

*  If provenance recording is currently enabled and is to be disabled...
      ELSE IF( OLD .AND. .NOT. NEW ) THEN

*  Indicate that the routine NDG1_HNDLR should no longer be called
*  whenever an NDF is opened or closed.
         CALL NDF_HNDLR( 'READ_EXISTING_NDF', NDG1_HNDLR, .FALSE.,
     :                   STATUS )
         CALL NDF_HNDLR( 'WRITE_EXISTING_NDF', NDG1_HNDLR, .FALSE.,
     :                   STATUS )
         CALL NDF_HNDLR( 'UPDATE_EXISTING_NDF', NDG1_HNDLR, .FALSE.,
     :                   STATUS )
         CALL NDF_HNDLR( 'OPEN_NEW_NDF', NDG1_HNDLR, .FALSE., STATUS )
         CALL NDF_HNDLR( 'CLOSE_NDF', NDG1_HNDLR, .FALSE., STATUS )
         CALL NDF_HNDLR( 'READ_DATA', NDG1_HNDLR, .FALSE., STATUS )
         CALL NDF_HNDLR( 'UPDATE_DATA', NDG1_HNDLR, .FALSE., STATUS )

*  Save the new state
         STATE_COM1 = .FALSE.
      END IF

      END
