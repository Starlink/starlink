      SUBROUTINE ASTMIRRORVARS( STATUS )
*+
*  Name:
*     ASTMIRRORVARS

*  Purpose:
*     Make the current Frame in a FrameSet mirror the variant Mappings
*     in another Frame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTMIRRORVARS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application indicates that all access to the Variant attribute of
*     the current Frame in the supplied FrameSet should should be forwarded
*     to some other nominated Frame in the FrameSet. For instance, if
*     subsequently a value is set for the Variant attribute of the current
*     Frame, the current Frame will be left unchanged and the setting is
*     instead applied to the nominated Frame. Likewise, if the value of the
*     Variant attribute is requested, the value returned is the value stored
*     for the nominated Frame rather than the current Frame itself. Also,
*     calls to the astaddvariant command will result in new variants being
*     added to the nominated Frame rather than the current Frame.
*
*     This provides a mechanism for propagating the effects of variant
*     Mappings around a FrameSet. If a new Frame is added to a FrameSet
*     by connecting it to an existing Frame that has two or more variant
*     Mappings, then it may be appropriate to set the new Frame so that
*     it mirrors the variants Mappings of the original Frame. If this is
*     done, then it will be possible to select a specific variant Mapping
*     using either the original Frame or the new Frame.

*  Usage:
*     astmirrorvars this iframe result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     IFRAME = LITERAL (Read)
*        The index of the Frame within the FrameSet which is to be
*        mirrored by the current Frame. This value should lie in the range
*        from 1 to the number of Frames in the FrameSet (as given by its
*        Nframe attribute). If null (!) is supplied (or the current
*        Frame is specified), then any mirroring established by a previous
*        call to this command is disabled.
*     RESULT = LITERAL (Read)
*        An NDF or text file to receive the modified FrameSet. If an NDF
*        is supplied, the WCS FrameSet within the NDF will be replaced by
*        the new FrameSet, if possible.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the original FrameSet to which a
*        new Frame is to be added. If an NDF is supplied, the WCS
*        FrameSet will be used.

*  Notes:
*     - This application corresponds to the AST routine AST_MIRRORVARIANTS.
*     The name has been abbreviated due to a limitation on the length of
*     ADAM command names.

*  Copyright:
*     Copyright (C) 2013 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-APR-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAFRAMESET

*  Local Variables:
      INTEGER IFRAME
      INTEGER THIS
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a FrameSet.
      CALL KPG1_GTOBJ( 'THIS', 'FrameSet', AST_ISAFRAMESET, THIS,
     :                 STATUS )

*  Get the index of the variant Frame.
      CALL ATL1_GTFRM( 'IFRAME', THIS, IFRAME, STATUS )

*  Make the call.
      CALL AST_MIRRORVARIANTS( THIS, IFRAME, STATUS )

*  Write the modified FrameSet out.
      CALL ATL1_PTOBJ( 'RESULT', 'THIS', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'Error mirroring a variants Frame in '//
     :                 'a FrameSet.', STATUS )
      END IF

      END
