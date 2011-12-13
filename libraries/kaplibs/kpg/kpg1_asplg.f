      SUBROUTINE KPG1_ASPLG( KEYMAP, BLEDGE, X1, X2, Y1, Y2 )
*+
*  Name:
*     KPG1_ASPLG

*  Purpose:
*     Stores a given KeyMap for future use by KPG1_ASPLN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASPLG( KEYMAP, BLEDGE, X1, X2, Y1, Y2 )

*  Description:
*     This routine stores the supplied AST KeyMap pointer in common so
*     that subsequent calls to KPG1_ASPLN can use it. If KPG1_ASPLN has
*     been registered with a Plot using AST_GRFSET, then it will be
*     called by AST whenever AST needs to draw a line. It will then draw
*     the required line and store a description of the line in the AST
*     KeyMap supplied to this routine. Consequently, this routine should
*     usually be called before calling AST_GRFSET.

*  Arguments:
*     KEYMAP = INTEGER (Given)
*        The KeyMap in which to store descriptions of the lines drawn by
*        the AST Plot class.
*     BLEDGE = LOGICAL (Given)
*        If TRUE, then do not draw lines that touch an edge of the box
*        specified by X1, X2, Y1 and Y2.
*     X1 = REAL (Given)
*        The X value at the left hand edge. Unused if BLEDGE Is .FALSE.
*     X2 = REAL (Given)
*        The X value at the right hand edge. Unused if BLEDGE Is .FALSE.
*     Y1 = REAL (Given)
*        The Y value at the bottom edge. Unused if BLEDGE Is .FALSE.
*     Y2 = REAL (Given)
*        The Y value at the top edge. Unused if BLEDGE Is .FALSE.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JUN-2006 (DSB):
*        Original version.
*     12-JUN-2006 (DSB):
*        Avoid direct access to the KPG_AST common blocks.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Needed by KPG_AST

*  Arguments Given:
      INTEGER KEYMAP
      LOGICAL BLEDGE
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2
*.

*  Store the supplied values.
      CALL KPG1_SETASTPLN( KEYMAP )
      CALL KPG1_SETASTX1( X1 )
      CALL KPG1_SETASTY1( Y1 )
      CALL KPG1_SETASTX2( X2 )
      CALL KPG1_SETASTY2( Y2 )
      CALL KPG1_SETASTBLE( BLEDGE )

      END
