      SUBROUTINE KPS1_BFCRF( MAP, CFRM, NAXC, NBEAM, NCOEF, PP, PSIGMA,
     :                       RP, RSIGMA, STATUS )
*+
*  Name:
*     KPS1_BFCRF

*  Purpose:
*     Converts the pixel coefficients of the fit into the refrence Frame

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFCRF( MAP, CFRM, NAXC, NCOEF, PP, PSIGMA, RP,
*                      RSIGMA, STATUS )

*  Description:
*     This converts the spatial coefficients and their errors from 
*     PIXEL co-ordinates into the reporting Frame for the fitted beam
*     features.  In addition the standard deviation widths may be
*     swapped so that the third coefficent is the major axis and the
*     the fourth is the minor axis.  The orientation is made to lie
*     the range of 0 to pi radians by adding or subtracting pi 
*     radians, and for SkyFrames it is measured from the North via East,
*     converted from X-axis through Y.  Data-value coefficients (e.g. 
*     amplitude) are unchanged.

*     This routine makes it easier to write the results to a
*     logfile or to output parameters and is more efficient
*     avoiding duplication transformations caused by separate
*     outputting routines and flushed message tokens. 

*  Arguments:
*     MAP = INTEGER (Given)
*        The AST Mapping from the PIXEL Frame of the NDF to the
*        reporting Frame.
*     CFRM = INTEGER (Given) 
*        A pointer to the current Frame of the NDF.
*     NAXC = INTEGER (Given) 
*        The number of axes in CFRM.
*     NBEAM = INTEGER (Given)
*        The number of beam features fitted.
*     NCOEF = INTEGER (Given)
*        The number of coefficients per beam position.
*     PP( NCOEF, NBEAM ) = DOUBLE PRECISION (Given)
*        The fit parameters with spatial coefficients measured in the
*        PIXEL Frame.
*     PSIGMA( NCOEF, NBEAM ) = DOUBLE PRECISION (Given)
*        The errors in the fit parameters measuered in the PIXEL Frame.
*     RP( NCOEF, NBEAM ) = DOUBLE PRECISION (Returned)
*        The fit parameters with spatial coefficients measured in the
*        reporting Frame.
*     RSIGMA( NCOEF, NBEAM ) = DOUBLE PRECISION (Given)
*        The errors in the fit parameters measuered in the reporting
*        Frame.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Particle Physics and Astronomy Research 
*     Council.  All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 May 21 (MJC):
*        Original version created from KPS1_BFLOG with the aim of
*        avoiding code duplication.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message-system public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants 

*  Arguments Given:
      INTEGER MAP
      INTEGER CFRM
      INTEGER NAXC
      INTEGER NBEAM
      INTEGER NCOEF
      DOUBLE PRECISION PP( NCOEF, NBEAM )
      DOUBLE PRECISION PSIGMA( NCOEF, NBEAM )

*  Arguments Returned:
      DOUBLE PRECISION RP( NCOEF, NBEAM )
      DOUBLE PRECISION RSIGMA( NCOEF, NBEAM )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DBEAR ! Bearing of one position from another

*  Local Constants:
      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.1415926535898 )

      DOUBLE PRECISION R2D       ! Radians to degrees
      PARAMETER ( R2D = 180.0D0 / PI )

*  Local Variables:
      DOUBLE PRECISION A( NDF__MXDIM ) ! Start of distance (A to B)
      DOUBLE PRECISION B( NDF__MXDIM ) ! End of distance (A to B)
      DOUBLE PRECISION DX        ! Increment along first axis
      DOUBLE PRECISION DY        ! Increment along second axis
      INTEGER I                  ! Loop count
      INTEGER IB                 ! Beam loop count
      LOGICAL ISSKY              ! Is the current Frame a SkyFrame?
      INTEGER LAT                ! Index to latitude axis in SkyFrame
      INTEGER LON                ! Index to longitude axis in SkyFrame
      INTEGER MAJOR              ! Index to major-FWHM value and error
      INTEGER MINOR              ! Index to minor-FWHM value and error
      DOUBLE PRECISION POS( 2, NDF__MXDIM ) ! Reporting positions
      DOUBLE PRECISION PIXPOS( 2, 2 ) ! Pixel positions
      REAL THETA                 ! Orientation in degrees

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the latitude and longitude axes needed for the origin of the
*  orientation.  
      ISSKY =  AST_ISASKYFRAME( CFRM, STATUS ) 
      IF ( ISSKY ) THEN
         LAT = AST_GETI( CFRM, 'LatAxis', STATUS )
         LON = AST_GETI( CFRM, 'LonAxis', STATUS )
      END IF

*  Report each fit in turn.
      DO IB = 1, NBEAM

*  Copy the input to output.  This handles any undefined values and the
*  data-value related coefficients.
         DO I = 1, NCOEF
            RP( I, IB ) = PP( I, IB )
            RSIGMA( I, IB ) = PSIGMA( I, IB )
         END DO

*  Beam position
*  =============

*  Centre
*  ------

*  Transform the supplied positions and errors from the PIXEL Frame 
*  of the NDF to the reporting Frame.
         PIXPOS( 1, 1 ) = PP( 1, IB )
         PIXPOS( 1, 2 ) = PP( 2, IB )
         PIXPOS( 2, 1 ) = PP( 1, IB ) + PSIGMA( 1, IB )
         PIXPOS( 2, 2 ) = PP( 2, IB ) + PSIGMA( 2, IB )
         CALL AST_TRANN( MAP, 2, 2, 2, PIXPOS, .TRUE., NAXC, 2, POS,
     :                   STATUS )

*  Normalize the supplied current Frame position.
         CALL AST_NORM( CFRM, POS, STATUS )

         RP( 1, IB ) = POS( 1, 1 )
         RP( 2, IB ) = POS( 1, 2 )
         
         IF ( PSIGMA( 1, IB ) .NE. VAL__BADD ) THEN

*  Centre errors
*  -------------
*  Need to determine the distance for the error bars.  For convenience
*  we fudge it here if NAXC exceeds 2.  We're keeping higher axes 
*  fixed anyway.
            DO I = 1, NAXC
               A( I ) = POS( 1, I )
               B( I ) = A( I )
            END DO
            B( 1 ) = POS( 2, 1 )

            PSIGMA( 1, IB )  = AST_DISTANCE( CFRM, A, B, STATUS )

*  Now we swap the path to the second axis.
            B( 1 ) = A( 1 )
            B( 2 ) = POS( 2, 2 )
            PSIGMA( 2, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )
         END IF

*  FWHMs
*  =====

*  See which is major?  For simplicity this is determined in the
*  PIXEL domain, however, perhaps it should be done in the current
*  (reporting) Frame but what if the axes have different units?
*  We can review this if it's a problem in practice.
         IF ( PP( 3, IB ) .GE. PP( 4, IB ) ) THEN
            MAJOR = 3
            MINOR = 4
         ELSE
            MAJOR = 4
            MINOR = 3
         END IF

*  Widths
*  ------

*  Transform the supplied positions and widths from the PIXEL Frame 
*  of the NDF to the reporting Frame.
         PIXPOS( 1, 1 ) = PP( 1, IB )
         PIXPOS( 1, 2 ) = PP( 2, IB )
         PIXPOS( 2, 1 ) = PP( 1, IB ) + PP( MAJOR, IB )
         PIXPOS( 2, 2 ) = PP( 2, IB ) + PP( MINOR, IB )
         CALL AST_TRANN( MAP, 2, 2, 2, PIXPOS, .TRUE., NAXC, 2, POS,
     :                   STATUS )

*  Normalize the current Frame co-ordinates.
         CALL AST_NORM( CFRM, POS, STATUS )

*  Need to determine the distances for the beam widths.  Use the
*  reporting axis.
         DO I = 1, NAXC
            A( I ) = POS( 1, I )
            B( I ) = A( I )
         END DO
         B( 1 ) = POS( 2, 1 )
         RP( 3, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )

         B( 1 ) = A( 1 )
         B( 2 ) = POS( 2, 2 )
         RP( 4, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )

*  Errors in width
*  ---------------
*  Bad values for the errors in the FWHM indicate no value exists.
         IF ( PSIGMA( MAJOR, IB ) .NE. VAL__BADD ) THEN

*  Transform the supplied positions and width errors from the PIXEL 
*  Frame of the NDF to the reporting Frame.
            PIXPOS( 1, 1 ) = PP( 1, IB )
            PIXPOS( 1, 2 ) = PP( 2, IB )
            PIXPOS( 2, 1 ) = PP( 1, IB ) + PSIGMA( MAJOR, IB )
            PIXPOS( 2, 2 ) = PP( 2, IB ) + PSIGMA( MINOR, IB )
            CALL AST_TRANN( MAP, 2, 2, 2, PIXPOS, .TRUE., NAXC, 2, POS,
     :                      STATUS )

*  Normalize the current Frame co-ordinates.
            CALL AST_NORM( CFRM, POS, STATUS )

*  Need to determine the distance for the error bars.  For convenience
*  we fudge it here if NAXC exceeds 2.  We're keeping higher axes 
*  fixed anyway.
            DO I = 1, NAXC
               A( I ) = POS( 1, I )
               B( I ) = A( I )
            END DO
            B( 1 ) = POS( 2, 1 )

            PSIGMA( 3, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )

            B( 1 ) = A( 1 )
            B( 2 ) = POS( 2, 2 )
            PSIGMA( 4, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )
         END IF

*  Orientation
*  ===========

*  Transform the supplied positions and a unit vector along the
*  orientation from the centre measured in the PIXEL Frame of the 
*  NDF to the reporting Frame.
         PIXPOS( 1, 1 ) = PP( 1, IB )
         PIXPOS( 1, 2 ) = PP( 2, IB )
         PIXPOS( 2, 1 ) = PP( 1, IB ) + SIN( PP( 5, IB ) )
         PIXPOS( 2, 2 ) = PP( 2, IB ) + COS( PP( 5, IB ) )
         CALL AST_TRANN( MAP, 2, 2, 2, PIXPOS, .TRUE., NAXC, 2, POS,
     :                   STATUS )

*  Normalize the current Frame co-ordinates.
         CALL AST_NORM( CFRM, POS, STATUS )

*  Use spherical geometry for a Skyframe.
*  Reverse these if the axis order is swapped in the SkyFrame.
         IF ( ISSKY ) THEN

*   Obtain the bearing of the vector between the centre and
*   the unit displacement.
            THETA = SLA_DBEAR( POS( 1, LON ), POS( 1, LAT ),
     :                         POS( 2, LON ), POS( 2, LAT ) )

*  If it's not a SkyFrame assume Euclidean geometry.  Also by
*  definition orientation is 0 degrees for a cicrular beam.  The
*  transformation can lead to small perturbation of the original
*  zero degrees.
         ELSE
            DX = POS( 2, 1 ) - POS( 1, 1 )
            DY = POS( 2, 2 ) - POS( 1, 2 )
            IF ( DX .NE. 0.0D0 .OR. DY .NE. 0.0D0 .AND. 
     :          ABS( PP( 3, IB ) - PP( 4, IB ) ) .GT. VAL__EPSD ) THEN
               THETA = ATAN2( DY, DX )
            ELSE
               THETA = 0.0D0
            END IF
         END IF

*  Ensure that the result in the range 0 to 180 degrees.
         IF ( THETA .LT. 0 ) THEN
            THETA = ( THETA + PI )

         ELSE IF( THETA .GT. PI ) THEN
            THETA = ( THETA - PI )

         END IF

*  Note no need to revise the orientation error.
         RP( 5, IB ) = THETA
      END DO

      END
