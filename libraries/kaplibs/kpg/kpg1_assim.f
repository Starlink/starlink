      SUBROUTINE KPG1_ASSIM( IWCS, STATUS )
*+
*  Name:
*     KPG1_ASSIM

*  Purpose:
*     Simplifies a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSIM( IWCS, STATUS )

*  Description:
*     This routine simplifies a FrameSet by adding a copy of the Current
*     Frame into it, using a simplified version of the Mapping from
*     Base to Current Frame. The new Frame becomes the Current Frame.
*
*     This routine should be called before doing any plotting with a
*     Plot (remember, a Plot is a FrameSet) in order to avoid the possiblity
*     of intermediate Frames being used in which positions are undefined.
*
*     Care should be taken decding where to call this routine since
*     the simplification process can be expensive. Do not call it within
*     a deep nested loop!

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to the FrameSet.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-SEP-1998 (DSB):
*        Original version.
*     2-MAR-2007 (DSB):
*        Documentation and variable names changed to indicate that this
*        routine can be used with any class of FrameSet, not just a Plot.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER IWCS

*  Status:
      INTEGER STATUS               ! Global status

*  Local Variables:
      INTEGER FRM                  ! Pointer to current Frame
      INTEGER MAP                  ! Simplified Mapping from GRAPHICS to Current
      INTEGER MAP0                 ! Original Mapping from GRAPHICS to Current
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Gett he Mapping from the Base Frame in the FrameSet to the Current Frame.
      MAP0 = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )

*  Simplify it.
      MAP = AST_SIMPLIFY( MAP0, STATUS )

*  Get pointer to the current Frame.
      FRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Add a copy of the Current Frame into the FrameSet using the above
*  Mapping to connect it to the GRAPHICS (Base) Frame.
      CALL AST_ADDFRAME( IWCS, AST__BASE, MAP, FRM, STATUS )

*  Annul the pointers.
      CALL AST_ANNUL( MAP0, STATUS )
      CALL AST_ANNUL( MAP, STATUS )
      CALL AST_ANNUL( FRM, STATUS )

      END
