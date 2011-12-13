      SUBROUTINE ATL_TTLPL( IPLOT, STATUS )
*+
*  Name:
*     ATL_TTLPL

*  Purpose:
*     Display a Plot Title without using AST_GRID.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_TTLPL( IPLOT, STATUS )

*  Description:
*     This routine display the Plot Title at the top of the area covered
*     by the Plot, but does not draw anything else (e.g. axes, tick
*     marks, borders, labels, etc). It does not need the inverse
*     transformation from current to base Frame to be defined in the Plot.

*  Arguments:
*     IPLOT = INTEGER (Given)
*       The Plot.
*     STATUS = INTEGER (Given and Returned)
*       The global status value.

*  Copyright:
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-JUN-2006 (DSB):
*        Original version.
*     26-JUN-2006 (DSB):
*        Take account of the direction of the Y axis when deciding the Y
*        value for the title.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'AST_PAR'        ! AST functions and constants

*  Arguments Given:
      INTEGER IPLOT

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER ATTR( 5 )*6
      CHARACTER SATTR*20
      CHARACTER TATTR*20
      CHARACTER*(AST__SZCHR) TTL
      DOUBLE PRECISION POS( 2 )
      INTEGER I
      INTEGER IAT
      INTEGER IPLOTT
      LOGICAL FULL
      REAL LBND( 2 )
      REAL UBND( 2 )
      REAL UP( 2 )

      DATA ATTR / 'Width', 'Colour', 'Font', 'Size', 'Style' /

*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if no Title is to be drawn.
      IF( AST_GETL( IPLOT, 'DrawTitle', STATUS ) ) THEN

*  Otherwise, take a copy of the supplied Plot so that we can modify the
*  Plot attributes, etc, without affecting later behaviour.
         IPLOTT = AST_COPY( IPLOT, STATUS )

*  Ensure no clipping is applied.
         CALL AST_CLIP( IPLOTT, AST__NOFRAME, 0.0D0, 0.0D0, STATUS )

*  Get the Title to be displayed.
         TTL = AST_GETC( IPLOTT, 'Title', STATUS )

*  The Mapping from GRAPHICS coords to the original current Frame may not
*  be defined in both direction, so we set the current Frame to be the
*  GRAPHICS Frame.
         CALL AST_SETI( IPLOTT, 'Current', AST_GETI( IPLOTT, 'Base',
     :                                               STATUS ),
     :                  STATUS )

*  Draw a border using invisble ink. This sets up the bounding box.
         CALL AST_SETL( IPLOTT, 'Invisible', .TRUE., STATUS )
         FULL = AST_BORDER( IPLOTT, STATUS )
         CALL AST_SETL( IPLOTT, 'Invisible', .FALSE., STATUS )

*  Get the bounding box of the border.
         CALL AST_BOUNDINGBOX( IPLOTT, LBND, UBND, STATUS )

*  Copy the Title graphical style to the Strings graphical style.
         DO I = 1, 5
            TATTR = ' '
            IAT = 0
            CALL CHR_APPND( ATTR( I ), TATTR, IAT )
            CALL CHR_APPND( '(Title)', TATTR, IAT )
            SATTR = ' '
            IAT = 0
            CALL CHR_APPND( ATTR( I ), SATTR, IAT )
            CALL CHR_APPND( '(Strings)', SATTR, IAT )

            CALL AST_SETR( IPLOTT, SATTR, AST_GETR( IPLOTT, TATTR,
     :                                              STATUS ),
     :                     STATUS )
         END DO

*  Determine if the Plot has a reversed Y axis (i.e.smallest Y value at
*  the top). Then choose the Y value for the bottom edge of the Title.
*  The Direction value is set in the base Frame, but we have previously
*  made the base Frame also the current Frame in IPLOTT so we can just
*  use the Direction value form the IPLOTT current Frame.
         IF( AST_GETL( IPLOTT, 'Direction(2)', STATUS ) ) THEN
            POS( 2 ) = UBND( 2 ) + 0.05*( UBND( 2 ) - LBND( 2 ) )
         ELSE
            POS( 2 ) = LBND( 2 ) - 0.05*( UBND( 2 ) - LBND( 2 ) )
         END IF

*  Draw the title centred above the bounding box.
         POS( 1 ) = 0.5*( LBND( 1 ) + UBND( 1 ) )
         UP( 1 ) = 0.0
         UP( 2 ) = 1.0
         CALL AST_TEXT( IPLOTT, TTL, POS, UP, 'CB', STATUS )

*  Annul the temporary Plot.
         CALL AST_ANNUL( IPLOTT, STATUS )
      END IF

      END
