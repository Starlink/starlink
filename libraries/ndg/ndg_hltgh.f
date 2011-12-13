      SUBROUTINE NDG_HLTGH( NEW, OLD, STATUS )
*+
*  Name:
*     NDG_HLTGH

*  Purpose:
*     Temporarily halt a GRP history block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_HLTGH( NEW, OLD, STATUS )

*  Description:
*     This routine can be called to stop subseqently accessed NDFs
*     being added to the list of NDFs that will receive extra history
*     holding GRP group contents when NDG_ENDGH is called to end the
*     current GRP history block.
*
*     In addition, if the GRP history block is halted, this routine will
*     ensure that the AUTO_HISTORY tuning parameter used by the NDF
*     library is set to zero, thus preventing the automatic creation of
*     History components in any new NDFs. When the block is un-halted
*     this tuning parameter is set back to the value it had at the time
*     the block was halted.

*  Arguments:
*     NEW = LOGICAL (Read)
*        The new required GRP history-recording state.
*     OLD = LOGICAL (Returned)
*        The GRP history-recording state on entry to this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - A .FALSE. GRP history-recording state means that any subseqently
*     accessed NDFs will not be added to the list of NDFs to receive
*     extra history holding GRP group contents when NDG_ENDPV is called.
*     - A .TRUE. GRP history-recording state means that any subseqently
*     accessed NDFs are added to the list of NDFs to receive  extra
*     history holding GRP group contents when NDG_ENDPV is called.

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
      INCLUDE 'NDG_COM2'         ! Global GRP history information

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
      OLD = STATE_COM2

*  If provenance recording is currently disabled and is to be enabled...
      IF( .NOT. OLD .AND. NEW ) THEN

*  Indicate that the routine NDG1_HNDLR should be called whenever default
*  history has been written to an NDF.
         CALL NDF_HNDLR( 'DEF_HISTORY', NDG1_HNDLR, .TRUE., STATUS )

*  Re-establish the original value of the AUTO_HISTORY tuning parameter.
         CALL NDF_TUNE( AUTO_COM2, 'AUTO_HISTORY', STATUS )

*  Save the new state
         STATE_COM2 = .TRUE.

*  If provenance recording is currently enabled and is to be disabled...
      ELSE IF( OLD .AND. .NOT. NEW ) THEN

*  Indicate that the routine NDG1_HNDLR should no longer be called
*  whenever default history is written to an NDF.
         CALL NDF_HNDLR( 'DEF_HISTORY', NDG1_HNDLR, .FALSE., STATUS )

*  Save the current value of the AUTO_HISTORY tuning parameter, and store
*  a zero value.
         CALL NDF_GTUNE( 'AUTO_HISTORY', AUTO_COM2, STATUS )
         CALL NDF_TUNE( 0, 'AUTO_HISTORY', STATUS )

*  Save the new state
         STATE_COM2 = .FALSE.
      END IF

      END
