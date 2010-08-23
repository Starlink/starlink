      SUBROUTINE KPG1_LUTK4( LP, UP, WORK, MAP, STATUS )
*+
*  Name:
*     KPG1_LUTK4

*  Purpose:
*     Produce an AST Mapping from pen number to RGB intensity.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LUTK4( LP, UP, WORK, MAP, STATUS )

*  Description:
*     This routine produces an AST Mapping which maps one-dimensional pen
*     number into three-dimensional RGB intensity within the current PGPLOT
*     graphics device.

*  Arguments:
*     LP = INTEGER (Given)
*        The lowest PGPLOT colour index to use.
*     UP = INTEGER (Given)
*        The highest PGPLOT colour index to use.
*     WORK( LP:UP, 3 ) = DOUBLE PRECISION (Returned)
*        Work space.
*     MAP = INTEGER (Returned)
*        The Mapping pointer.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1999 (DSB):
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

*  Arguments Given:
      INTEGER LP
      INTEGER UP

*  Arguments Returned:
      DOUBLE PRECISION WORK( LP:UP, 3 )
      INTEGER MAP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CMAP1              ! Pointer to a CmpMap
      INTEGER CMAP2              ! Pointer to a CmpMap
      INTEGER I                  ! Loop count
      INTEGER NPEN               ! Number of pens
      INTEGER PERM( 3 )          ! Pointer to output permutation array
      INTEGER PMAP               ! Pointer to a PermMap
      INTEGER RGBMAP( 3 )        ! Pointers to (PEN -> Intensity) Mappings
      REAL R, G, B               ! Red, Green and Blue intensities
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the number of pens in use.
      NPEN = UP - LP + 1

*  Copy the RGB intensities (0-1) into the work array.
      DO I = LP, UP
         CALL PGQCR( I, R, G, B )
         WORK( I, 1 ) = DBLE( R )
         WORK( I, 2 ) = DBLE( G )
         WORK( I, 3 ) = DBLE( B )
      END DO

*  Create three LutMaps describing the Mapping from Pen Number to each of
*  the three primary colours.
      DO I = 1, 3
         RGBMAP( I ) = AST_LUTMAP( NPEN, WORK( LP, I ), DBLE( LP ),
     :                             1.0D0, ' ', STATUS )
      END DO

*  Combine these LutMaps in parallel,
      CMAP1 = AST_CMPMAP( RGBMAP( 1 ), RGBMAP( 2 ), .FALSE., ' ',
     :                    STATUS )
      CMAP2 = AST_CMPMAP( CMAP1, RGBMAP( 3 ), .FALSE., ' ', STATUS )

*  Create a PermMap which feeds input Axis 1 to all three output axes.
      PERM( 1 ) = 1
      PERM( 2 ) = 1
      PERM( 3 ) = 1
      PMAP = AST_PERMMAP( 1, 1, 3, PERM, 0.0D0, ' ', STATUS )

*  Combine this PermMap in series with the CmpMap to get the returned
*  Mapping.
      MAP = AST_CMPMAP( PMAP, CMAP2, .TRUE., ' ', STATUS )

*  Annul temporary AST objects.
      CALL AST_ANNUL( RGBMAP( 1 ), STATUS )
      CALL AST_ANNUL( RGBMAP( 2 ), STATUS )
      CALL AST_ANNUL( RGBMAP( 3 ), STATUS )
      CALL AST_ANNUL( CMAP1, STATUS )
      CALL AST_ANNUL( CMAP2, STATUS )
      CALL AST_ANNUL( PMAP, STATUS )

      END
