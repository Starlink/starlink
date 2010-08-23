      SUBROUTINE KPG1_PGPIX( IPLOT, DOMAIN, LBND, UBND, NX, NY, COLI,
     :                       STATUS )
*+
*  Name:
*     KPG1_PGPIX

*  Purpose:
*     Displays an image using PGPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  KPG1_PGPIX( IPLOT, DOMAIN, LBND, UBND, NX, NY, COLI, STATUS )

*  Description:
*     This routine uses PGPIXL to display an array of colour indices
*     as a rectangular image. The area occupied by the array of colour
*     indices is specified within a nominated Domain. Two opposite
*     corners of this area are transformed into the Base Frame
*     of the Plot (which should correspond to the current PGPLOT world
*     co-ordinate sysytem), and the array of colour indices is drawn
*     between these two transformed positions.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        A pointer to an AST Plot. The Base Frame in this Plot should
*        correspond to the world co-ordinates in the current PGPLOT window.
*     DOMAIN = CHARACTER * ( * ) (Given)
*        The Domain in which the co-ordinates supplied in BOX are defined.
*     LBND( 2 ) = REAL (Given)
*        The lower bounds of the area covered by the supplied array of
*        colour indices, in the Domain given by DOMAIN.
*     UBND( 2 ) = REAL (Given)
*        The upper bounds of the area covered by the supplied array of
*        colour indices, in the Domain given by DOMAIN.
*     NX = INTEGER
*        Number of columns in the array of colour indices.
*     NY = INTEGER
*        Number of rows in the array of colour indices.
*     COLI( NX, NY ) = INTEGER (Given)
*        The array of colour indices.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1998 (DSB):
*        Original version.
*     2-JUN-2009 (DSB):
*        Ensure the image does not quite fill the PGPLOT viewport.
*        If the image exactly fills the viewport, rounding errors in
*        PGPLOT can cause PGPLOT to use incorrect array bounds.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER IPLOT
      CHARACTER DOMAIN*(*)
      REAL LBND( 2 )
      REAL UBND( 2 )
      INTEGER NX
      INTEGER NY
      INTEGER COLI( NX, NY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ICURR              ! Index of original Current Frame
      DOUBLE PRECISION XIN( 2 )  ! X co-ords in supplied Domain
      DOUBLE PRECISION XOUT( 2 ) ! X co-ords in GRAPHICS Domain
      DOUBLE PRECISION YIN( 2 )  ! Y co-ords in supplied Domain
      DOUBLE PRECISION YOUT( 2 ) ! Y co-ords in GRAPHICS Domain
      REAL DELTA                 ! Boundary to allow for rounding errors
      REAL X1, Y1                ! World coords at bottom left of PGPLOT
                                 ! viewport
      REAL X2, Y2                ! World coords at top right of PGPLOT
                                 ! viewport
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Note the index of the Current Frame in the Plot. This is changed by
*  AST_FINDFRAME.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Find the first Frame in the Plot with the specified Domain.
      IF( AST_FINDFRAME( IPLOT, AST_FRAME( 2, ' ', STATUS ), DOMAIN,
     :                   STATUS ) .NE. AST__NULL ) THEN

*  Transform the bottom left and top right corners from the given Domain
*  into the GRAPHICS Frame.
         XIN( 1 ) = DBLE( LBND( 1 ) )
         XIN( 2 ) = DBLE( UBND( 1 ) )
         YIN( 1 ) = DBLE( LBND( 2 ) )
         YIN( 2 ) = DBLE( UBND( 2 ) )
         CALL AST_TRAN2( IPLOT, 2, XIN, YIN, .FALSE., XOUT, YOUT,
     :                   STATUS )

*  If succesful, draw the image.
         IF( STATUS .EQ. SAI__OK ) THEN

*  First ensure that the image does not quite fill the PGPLTO viewport.
*  This is to allow for some rounding error in PGPLOT.
            CALL PGQWIN( X1, X2, Y1, Y2 )

            DELTA = 10*VAL__EPSR*( X2 - X1 )
            X1 = MAX( REAL( XOUT( 1 ) ), X1 + DELTA )
            X2 = MIN( REAL( XOUT( 2 ) ), X2 - DELTA )

            DELTA = 10*VAL__EPSR*( Y2 - Y1 )
            Y1 = MAX( REAL( YOUT( 1 ) ), Y1 + DELTA )
            Y2 = MIN( REAL( YOUT( 2 ) ), Y2 - DELTA )

*  Now draw the image
            CALL PGPIXL( COLI, NX, NY, 1, NX, 1, NY, X1, X2, Y1, Y2 )
         END IF

*  Re-instate the original Current Frame in the Plot (modified by
*  AST_FINDFRAME).
         CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

*  Report an error if no Frame with the specified Domain was found in the
*  Plot.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'DOM', DOMAIN )
         CALL ERR_REP( 'KPG1_PGPIX_1', 'KPG1_PGPIX: No ^DOM Frame is '//
     :                 'available (possible programming error).',
     :                 STATUS )
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
