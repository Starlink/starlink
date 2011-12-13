      SUBROUTINE KPS1_BFPRE( IPLOT, MAP, NPOS, MARK, MARKER,
     :                       NPAR, FPAR, STATUS )
*+
*  Name:
*     KPS1_BFPRE

*  Purpose:
*     Displays some BEAMFIT results graphically.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFPRE( IPLOT, MAP, NPOS, MARK, MARKER,
*                      NPAR, FPAR, STATUS )

*  Description:
*     This routine indicates graphically the location of the beam and
*     optionally the beam's size and orientation as an ellipse,
*     depending on argument MARK.

*     This routine should be used in the "Cursor" mode of BEAMFIT.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        The identfier of the plot obtained from graphics database.
*        It is only accessed in Cursor mode when Mark='ELLIPSE'.
*     MAP = INTEGER (Given)
*        The AST Mapping from the Frame in which the initial guess
*        positions are supplied, to the PIXEL Frame of the NDF.
*     MARK = CHARACTER * ( * ) (Given)
*        What positions are to be marked?  Can be "INITIAL", "FIT",
*        'ELLIPSE' or "NONE".
*     MARKER = INTEGER (Given)
*        The PGPLOT number for the marker type to mark the positions
*        specified by MARK.
*     NPAR = INTEGER (Given)
*        The maximum number of fit parameters.
*     FPAR( NPAR ) = DOUBLE PRECISION (Given)
*        The coefficients of the fit in the PIXEL Frame.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*      -  The BF constants used to define array dimensions are located in
*      the include file BF_PAR.
*      - The plotting style should be set before calling this routine.

*  Copyright:
*     Copyright (C) 2007 Particle Physics and Astronomy Research.
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 June 18 (MJC):
*        Original version.
*     2007 June 25 (MJC):
*        The fitted orientation is now in radians.
*     2011 May 11 (MJC):
*        Removed no-longer-used argument NAXIN.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'MSG_PAR'          ! Message-system public constants
      INCLUDE 'BF_PAR'           ! BEAMFIT constants

*  Arguments Given:
      INTEGER IPLOT
      INTEGER MAP
      INTEGER NPOS
      CHARACTER MARK*( * )
      INTEGER MARKER
      INTEGER NPAR
      DOUBLE PRECISION FPAR( NPAR )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.1415926535898 )

      DOUBLE PRECISION PIBY2
      PARAMETER ( PIBY2 = 0.5D0 * PI )

*  Local Variables:
      REAL AXRIN                 ! Axis ratio, PIXEL Frame
      REAL AXROUT                ! Axis ratio, Graphics Frame
      DOUBLE PRECISION BC( BF__NDIM ) ! Base co-ordinates
      LOGICAL BORDER             ! Result of AST_BORDER function
      DOUBLE PRECISION CENTRE( 2 )! Beam graphics position
      INTEGER CMARK              ! Marker to use when marking beam
                                 ! positions
      INTEGER BASFRM             ! Current Frame
      INTEGER ELL                ! Ellipse identifier
      INTEGER I                  ! Position index
      INTEGER ICURR              ! Current PLOT Frame
      DOUBLE PRECISION INCEN( BF__MXPOS, 2 )! Beam graphics positions
      INTEGER J                  ! Axis index
      INTEGER K                  ! Index
      INTEGER MAPI               ! Inverted mapping of MAP
      DOUBLE PRECISION ORIENT( 2 ) ! Orientastion of the ellipse
      REAL ORIN                  ! Orientation, PIXEL Frame
      REAL OROUT                 ! Orientation, Graphics Frame
      DOUBLE PRECISION PIXPOS( BF__MXPOS, BF__NDIM ) ! Pixel position
      DOUBLE PRECISION SEMIAX( 2 ) ! Semi-major axes
      REAL WIDIN                 ! Width (norrmally FWHM), PIXEL Frame
      DOUBLE PRECISION WIDOUT    ! Width, Graphics Frame

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied Mappings have the required transformations.
      IF ( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_BFPRE_ERR1','The Mapping required '//
     :                 'to map the supplied positions into the '//
     :                 'pixel Frame of the NDF is not defined.',
     :                 STATUS )

      ELSE IF ( ( MARK( 1 : 1 ) .EQ. 'F' .OR.
     :            MARK( 1 : 1 ) .EQ. 'E' ) .AND.
     :          .NOT. AST_GETL( MAP, 'TRANINVERSE', STATUS ) ) THEN
         CALL MSG_OUT( 'KPS1_BFPRE_MSG1','The Mapping required '//
     :                 'to map the beam positions into the '//
     :                 'graphics co-ordinate Frame is not defined.',
     :                 STATUS )
         CALL MSG_OUT( 'KPS1_BFPRE_MSG2','Beam positions will '//
     :                 'not be marked!', STATUS )
         GO TO 999

      END IF

*  Store the marker to use.
      CMARK = -999
      IF ( MARK( 1 : 1 ) .EQ. 'F' .OR.
     :     MARK( 1 : 1 ) .EQ. 'E' ) CMARK = MARKER

      IF ( MARK( 1 : 1 ) .EQ. 'E' ) THEN
         ICURR = AST_GETI( IPLOT, 'Current', STATUS )
         BASFRM = AST_GETFRAME( IPLOT, AST__BASE, STATUS )
      END IF

*  If a good fit was found, transform the position of the centre of the
*  beam to the reporting Frame.  The positions are the first and second
*  coefficients for each beam.
      DO I = 1, NPOS
         DO J = 1, BF__NDIM
            K = J + ( I - 1 ) * BF__NCOEF
            IF ( FPAR( K ) .NE. VAL__BADD ) THEN
               PIXPOS( I, J ) = FPAR( K )
            ELSE
               PIXPOS( I, J ) = AST__BAD
            END IF
         END DO
      END DO

*  Indicate the location of the beam
*  ---------------------------------

*  Mark the beam position on the screen if required.
      IF ( CMARK .GT. -32 ) THEN

*  First transform beam positions from pixel to graphics co-ordinates.
         CALL AST_TRANN( MAP, NPOS, BF__NDIM, BF__MXPOS, PIXPOS,
     :                   .FALSE., 2, BF__MXPOS, INCEN, STATUS )

*  Invert the mapping.
         MAPI = AST_COPY( MAP, STATUS )
         CALL AST_INVERT( MAPI, STATUS )

         DO I = 1, NPOS
            CENTRE( 1 ) = INCEN( I, 1 )
            CENTRE( 2 ) = INCEN( I, 2 )

*  Mark it.
            IF ( CENTRE( 1 ) .NE. AST__BAD .AND.
     :           CENTRE( 2 ) .NE. AST__BAD ) THEN
               CALL PGPT( 1, REAL( CENTRE( 1 ) ), REAL( CENTRE( 2 ) ),
     :                    CMARK )
            END IF

*  Indicate the size and orientation of the beam
*  ---------------------------------------------
            IF ( MARK( 1 : 1 ) .EQ. 'E' ) THEN

*  Assemble the relevant parameters in the required data types.
*  Sky position angles are already correct.  However, for Cartesian
*  systems the position angle is measured from positive Y through
*  negative X and KPS1_ELMAP wants the opposite polarity.
               K = ( I - 1 ) * BF__NCOEF
               WIDIN = REAL( MIN( FPAR( 3 + K ), FPAR( K + 4 ) ) )
               AXRIN = WIDIN /
     :                 REAL( MAX( FPAR( 3 + K ), FPAR( K + 4 ) ) )

               ORIN = REAL( 0.5D0 * PI - FPAR( K + 5 ) )

*  Transform Gaussian ellipse from pixel to graphics co-ordinates.
               CALL KPS1_ELMAP( .FALSE., BASFRM, FPAR( K + 1 ),
     :                          FPAR( K + 2 ), AXRIN, WIDIN, ORIN,
     :                          MAPI, INCEN( I, 1 ), INCEN( I, 2 ),
     :                          AXROUT, WIDOUT, OROUT, STATUS )

               IF ( AXROUT .GT. 1.0 ) AXROUT = 1.0 / AXROUT
               SEMIAX( 1 ) = DBLE( WIDOUT / AXROUT )
               SEMIAX( 2 ) = DBLE( WIDOUT )
               ORIENT( 1 ) = DBLE( OROUT )
               ORIENT( 2 ) = ORIENT( 1 )

*  Define an Ellipse in the current Frame of the Plot.
               ELL = AST_ELLIPSE( BASFRM, 1, CENTRE, SEMIAX, ORIENT,
     :                            AST__NULL, ' ', STATUS )

*  Add the Ellipse into the Plot, first remembering the current Frame.
*  The Ellipse becomes the new current Frame in the Plot.  Since it is
*  defined in the original current Frame of the Plot, we can use a
*  UnitMap to connect the Ellipse to the current Frame of the Plot.
               CALL AST_ADDFRAME( IPLOT, AST__BASE,
     :                            AST_UNITMAP( 2, '', STATUS ), ELL,
     :                            STATUS )

*  Draw the outline of the Ellipse.
               BORDER= AST_BORDER( IPLOT, STATUS )

*  For tidyness, delete the Ellipse from the Plot and re-instate the
*  original current Frame.
               CALL AST_REMOVEFRAME( IPLOT, AST__CURRENT, STATUS )
               CALL AST_SETI( IPLOT, 'Current', ICURR, STATUS )
            END IF
         END DO
      END IF

*  Tidy up.
*  =======
 999  CONTINUE

      END
