      SUBROUTINE ATL_PLROI( IPLOT, RPLOTS, STATUS )
*+
*  Name:
*     ATL_PLROI

*  Purpose:
*     Create a set of Plots associated with each ROI in a given Plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_PLROI( IPLOT, RPLOTS, STATUS )

*  Description:
*     This routine searches the supplied Plot for ROI Frames (see
*     ATL_AXTRM). For each ROI Frame found, it creates a new Plot 
*     that covers just the region of graphics coords occupied by the 
*     ROI. These new Plots are returned in an AST KeyMap.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        The supplied Plot to search for ROI Frames.
*     RPLOTS = INTEGER (Returned)
*        An AST KeyMap holding the Plots associated with the ROI Frames.
*        The key used to identify each Plot within the KeyMap is the Domain 
*        name of the corresponding ROI Frame.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-JUN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ATL_PAR'          ! ATL constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IPLOT

*  Arguments Retuned:
      INTEGER RPLOTS

*  Status:
      INTEGER STATUS           ! Global status

*  Local Variables:
      CHARACTER DOM*20         ! Domain attribute for a Frame
      CHARACTER IDENT*20       ! Ident attribute for a Frame
      DOUBLE PRECISION RLBND( ATL__MXDIM )! Region lower bounds   
      DOUBLE PRECISION RUBND( ATL__MXDIM )! Region upper bounds   
      INTEGER FRM              ! Pointer to a Frame in Plot
      INTEGER I                ! General variable
      INTEGER ICURR            ! Index of original current Frame
      INTEGER IFRM             ! Index of Frame in Plot
      INTEGER IPLOTR           ! Pointer to AST Plot for a Region of interest
      INTEGER JFRM             ! Index of Frame in Plot
      INTEGER NFRM             ! Number of Frames in Plot
      INTEGER NREG             ! Number of Regions found so far
      LOGICAL GRID             ! Draw a grid?
      REAL AXGAP1              ! Gap between major tick marks on axis 1
      REAL AXGAP2              ! Gap between major tick marks on axis 2
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the AstKeyMap to hold the returned Plots.
      RPLOTS = AST_KEYMAP( ' ', STATUS )

*  Note the original current Frame in the supplied Plot.
      ICURR = AST_GETI( IPLOT, 'Current', STATUS )

*  If a title is required, draw it now at the top of the whole pixel array
*  before we start to produce the annotated axes for each individual ROI.
      CALL ATL_TTLPL( IPLOT, STATUS )

*  See if each ROI is to have a grid or not. We arrange that a grid is
*  drawn only if one has been explicitly requested (ROI Regions can 
*  produce anomolous bad coords arounds the edges, thus causing the
*  default value for Grid to become non-zero).
      IF( AST_TEST( IPLOT, 'GRID', STATUS ) ) THEN
         GRID = AST_GETL( IPLOT,' GRID', STATUS )
      ELSE
         GRID = .FALSE.
      END IF

*  Loop round all Frames in the supplied Plot, counting the number of ROI
*  Frames encountered.
      NREG = 0
      NFRM = AST_GETI( IPLOT, 'NFRAME', STATUS )
      DO IFRM = 1, NFRM
         FRM = AST_GETFRAME( IPLOT, IFRM, STATUS )

*  Pass on unless this Frame is a Region with a Domain that begins with
*  "ROI"
         IF( AST_ISAREGION( FRM, STATUS ) )  THEN         
            DOM = AST_GETC( FRM, 'Domain', STATUS )
            IF( DOM( : 3 ) .EQ. 'ROI' ) THEN
               NREG = NREG + 1

*  Attempt to locate a Frame that has the same value for its Ident
*  attribute, making it the current Frame in the Plot.
               DO JFRM = 1, NFRM
                  CALL AST_SETI( IPLOT, 'Current', JFRM, STATUS )
                  IDENT = AST_GETC( IPLOT, 'Ident', STATUS )
                  IF( IDENT .EQ. DOM ) THEN

*  Create a new Plot that covers just the corresponding Region.
                     CALL AST_GETREGIONBOUNDS( FRM, RLBND, RUBND, 
     :                                         STATUS )
                     CALL ATL_CUTPL( IPLOT, IFRM, RLBND, RUBND,
     :                               IPLOTR, STATUS )

*  Ensure no title is drawn at the top of the ROI.
                     CALL AST_SETI( IPLOTR, 'DRAWTITLE', 0, STATUS )

*  Ensure the grid is drawn only if required.
                     CALL AST_SETL( IPLOTR, 'GRID', GRID, STATUS )

*  Suppress axis labels for all but the first.
                     IF( NREG .GT. 1 ) THEN
                        CALL AST_SETI( IPLOTR, 'TEXTLAB', 0, STATUS )
                        CALL AST_SETI( IPLOTR, 'NUMLAB', 0, STATUS )

*  Record the gaps between major ticks that will be used for the
*  first ROI.
                     ELSE
                        AXGAP1 = AST_GETR( IPLOTR, 'Gap(1)', STATUS )
                        AXGAP2 = AST_GETR( IPLOTR, 'Gap(2)', STATUS )
                     END IF

*  Ensure the gaps between major ticks are the same as used for the first
*  ROI.
                     CALL AST_SETR( IPLOTR, 'Gap(1)', AXGAP1, STATUS )
                     CALL AST_SETR( IPLOTR, 'Gap(2)', AXGAP2, STATUS )

*  Store the Plot in the returned KeyMap.
                     CALL AST_MAPPUT0A( RPLOTS, IDENT, IPLOTR, ' ', 
     :                                  STATUS )

*  Annul the Plot pointer.
                     CALL AST_ANNUL( IPLOTR, STATUS )

                  END IF
               END DO

            END IF

         END IF
         CALL AST_ANNUL( FRM, STATUS )
      END DO

*  Re-instate the original current Frame in the supplied Plot.
      CALL AST_SETI( IPLOT, 'Current', ICURR, STATUS )

      END
