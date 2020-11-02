      SUBROUTINE ASTPERMAXES( STATUS )
*+
*  Name:
*     ASTPERMAXES

*  Purpose:
*     Permute the axis order in a Frame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTPERMAXES( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application permutes the order in which a Frame's axes occur.

*  Usage:
*     astpermaxes this perm result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     PERM() = _INTEGER (Read)
*        An array with one element for each axis of the Frame given
*        by parameter THIS. This should list the axes in their new order,
*        using the original axis numbering (which starts at 1 for the
*        first axis).
*     RESULT = LITERAL (Read)
*        An text file or NDF to receive the modified Frame or FrameSet.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the original FrameSet to which a
*        new Frame is to be added. If an NDF is supplied, the current
*        Frame of the WCS FrameSet will be used.

*  Notes:
*     - If a FrameSet is supplied for THIS, then the axes of the current
*     Frame will be permuted according to PERM, and all Mappings which
*     connect to the current Frame will be modified to take account of
*     the permutation.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     14-FEB-2001 (DSB):
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
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAFRAME

*  Local Variables:
      INTEGER PERM( NDF__MXDIM )
      INTEGER THIS
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a Frame or FrameSet.
      CALL KPG1_GTOBJ( 'THIS', 'Frame or FrameSet', AST_ISAFRAME, THIS,
     :                 STATUS )

*  Get the permutation array.
      CALL PAR_EXACI( 'PERM', AST_GETI( THIS, 'NAXES', STATUS ),
     :                PERM, STATUS )

*  Permute the axes of the Frame.
      CALL AST_PERMAXES( THIS, PERM, STATUS )

*  Write the modified FrameSet out.
      CALL ATL1_PTOBJ( 'RESULT', 'THIS', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTPERMAXES_ERR', 'Error permuting the axes '//
     :                 'of a Frame.', STATUS )
      END IF

      END
