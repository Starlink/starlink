      SUBROUTINE KPS1_BFFTG( M, N, XC, DATA, VAR, FVEC, STATUS )
*+
*  Name:
*     KPS1_BFFTG

*  Purpose:
*     Finds the residuals from multiple two-dimensional generalised
*     Gaussian fits.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFFTG( M, N, XC, DATA, VAR, FVEC, STATUS )

*  Description:
*     The supplied co-ordinates are converted to values corresponding
*     to a set of two-dimensional generalised Gauissian parameters 
*     defined by the argument XC (together with other information passed 
*     from KPS1_BFFT in common block BF_COM).  The resulting image
*     intensities are compared with the supplied data values and the
*     residuals in intensity at each position are returned in FVEC.

*  Arguments:
*     M = INTEGER (Given)
*        The number of data values.
*     N = INTEGER (Given)
*        The number of fit parameters supplied in XC.  The
*        remaining ones are defined by values stored in common.
*     XC( N ) = DOUBLE PRECISION (Given)
*        The parameters of the two-dimensional Gaussian fit for which
*        the residuals are required.  The full list of elements for
*        each Gaussian is as follows.
*           1  --  X pixel centre
*           2  --  Y pixel centre
*           3  --  Major axis standard deviation in pixels
*           4  --  Minor axis standard deviation in pixels
*           5  --  Position angle of the major axis in radians
*           6  --  Amplitude
*           7  --  Background (assumed to be a constant level)
*           8  --  Shape exponent
*
*        Where each fit parameter has been fixed by the user, the
*        remaining elements in XC are shuffled down to occupy the
*        location which otherwise would have been used by the fixed
*        Gaussian parameter.  Fixed parameter values are supplied
*        in common, as is the number of Gaussians.
*     DATA( M ) = DOUBLE PRECISION (Given)
*        The data values.
*     VAR( M ) = DOUBLE PRECISION (Given)
*        The variances of the data values.
*     FVEC( M ) = DOUBLE PRECISION (Returned)
*        The residuals.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2010, 2013 Science & Technology Facilities Council.
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
*     2007 February 14 (MJC):
*        Original version.
*     2007 April 27 (MJC):
*        Added FIXAMP and FIXRAT arguments, and concurrent fitting of
*        multiple Gaussians.
*     2007 May 14 (MJC):
*        Support fixed separations.
*     2010 July 5 (MJC):
*        Switched to generalised Gaussian fit by the introduction of
*        the shape exponent.
*     2013 July 15 (MJC):
*        Allow for circularity constraint.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'BF_PAR'           ! BEAMFIT constants

*  Global Variables:
      INCLUDE 'BF_COM'          ! Used for communicating with KPS1_BFFT
*        INTEGER NBEAMS (Read)
*           The number of beams.
*        PC( BF__NCOEF, BF__MXPOS ) = DOUBLE PRECISION (Read)
*           The initial guess parameter values, including any
*           fixed values supplied by the user.
*        ARATIO( BF__MXPOS - 1 ) = DOUBLE PRECISION (Read)
*           The amplitude ratios of the secondary to the primary beam
*           positions.
*        PIXOFF( BF__MXPOS - 1, BF__NDIM ) = DOUBLE PRECISION (Read)
*           The pixel offsets of the secondary beam positions with
*           respect to the primary beam position.
*        USEVAR = LOGICAL (Read)
*           Whether or not to use variance to weight the fit.
*        AMPC = LOGICAL (Read)
*           Amplitude fixed by user?
*        BACKC = LOGICAL (Read)
*           Was the background level set by the user?
*        FWHMC = LOGICAL (Read)
*           Was the FWHM fixed by the user?
*        CIRC = LOGICAL
*           Circular Gaussian demanded by the user?
*        LBND( BF__NDIM ) = INTEGER (Read)
*           The lower pixel bounds of the data and variance arrays.
*        UBND( BF__NDIM ) = INTEGER (Read)
*           The upper pixel bounds of the data and variance arrays.
*        ORIC = LOGICAL (Read)
*           Was the orientation fixed by the user?
*        POSC = LOGICAL (Read)
*           Were the centre co-ordinates of the Gaussian fixed by the
*           user?
*        RATIOC = LOGICAL (Read)
*           Amplitude ratios fixed by the user?
*        SEPARC = LOGICAL (Read)
*           Are the separations fixed?
*        SHAPEC = LOGICAL (Read)
*           Exponent of the Gaussian function fixed by user?

*  Arguments Given:
      INTEGER M
      INTEGER N
      DOUBLE PRECISION XC( N )
      DOUBLE PRECISION DATA( M )
      DOUBLE PRECISION VAR( M )

*  Arguments Returned:
      DOUBLE PRECISION FVEC( M )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION CT        ! Cosine of the orientation
      DOUBLE PRECISION E1        ! Work variable
      DOUBLE PRECISION E2        ! Work variable
      DOUBLE PRECISION E3        ! Work variable
      DOUBLE PRECISION E4        ! Work variable
      DOUBLE PRECISION E5        ! Work variable
      DOUBLE PRECISION E6        ! Work variable
      DOUBLE PRECISION GI        ! Sum of Gaussians' intensity at pixel
      INTEGER I                  ! Counter
      INTEGER IG                 ! Gaussian counter
      INTEGER J                  ! Counter
      INTEGER K                  ! Counter
      DOUBLE PRECISION ST        ! Sine of the orientation
      DOUBLE PRECISION XX        ! X pixel co-ordinate
      DOUBLE PRECISION YY        ! Y pixel co-ordinate

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      I = 0
      DO IG = 1, NBEAMS

*  Copy the new values for the parameters being optimized into the
*  current parameter array.
         IF ( .NOT. ( POSC .OR. ( SEPARC .AND. IG .GT. 1 ) ) ) THEN
            I = I + 2
            PC( 1, IG ) = XC( I - 1 )
            PC( 2, IG ) = XC( I )

*  Maintain offset from current primary beam's position.
         ELSE IF ( SEPARC .AND. IG .GT. 1 ) THEN
            PC( 1, IG ) = PC( 1, 1 ) + PIXOFF( IG - 1, 1 )
            PC( 2, IG ) = PC( 2, 1 ) + PIXOFF( IG - 1, 2 )
         END IF

*  A supplied FWHM need not imply circularity.
         IF ( .NOT. FWHMC ) THEN
            IF ( CIRC ) THEN
               I = I + 1
               PC( 3, IG ) = XC( I )
               PC( 4, IG ) = XC( I )
            ELSE
               I = I + 2
               PC( 3, IG ) = XC( I - 1 )
               PC( 4, IG ) = XC( I )
            END IF
         END IF

*  An attempt to keep the orientation constrained here fails.
*  The solution never converges.
         IF ( .NOT. ORIC ) THEN
            I = I + 1
            PC( 5, IG ) = XC( I )
         END IF

         IF ( IG .GT. 1 .AND. RATIOC ) THEN
            PC( 6, IG ) = PC( 6, 1 ) * ARATIO( IG - 1 )

         ELSE IF ( .NOT. AMPC ) THEN
            I = I + 1
            PC( 6, IG ) = XC( I )
         END IF

*  Use the same background for all beam positions.
         IF ( .NOT. BACKC .AND. IG .EQ. 1 ) THEN
            I = I + 1
            PC( 7, IG ) = XC( I )
         END IF

*  Copy latest shape exponent.
         IF ( .NOT. SHAPEC ) THEN
            I = I + 1
            PC( 8, IG ) = XC( I )
         END IF
         
      END DO

*  Variance weighting
*  ==================
      IF ( USEVAR ) THEN
         I = 0
         DO K = LBND( 2 ), UBND( 2 )
            YY = DBLE( K ) - 0.5D0
            DO J = LBND( 1 ), UBND( 1 )
               I = I + 1
               IF ( DATA( I ) .NE. VAL__BADD .AND.
     :               VAR( I ) .NE. VAL__BADD .AND.
     :               VAR( I ) .GT. VAL__SMLD ) THEN
                  XX = DBLE( J ) - 0.5D0

                  GI = 0.0D0
                  DO IG = 1, NBEAMS

*  Define useful variables.
                     CT = COS( PC( 5, IG ) )
                     ST = SIN( PC( 5, IG ) )

*  Evaluate sum of Gaussian fits.  Recall PC( 1, IG ) and PC( 2, IG )
*  are the x and y centre co-ordinates respectively for the IGth 
*  Gaussian fit.
                     E1 = ( XX - PC( 1, IG ) ) * CT +
     :                    ( YY - PC( 2, IG ) ) * ST
                     E2 = ( YY - PC( 2, IG ) ) * CT -
     :                    ( XX - PC( 1, IG ) ) * ST

*  PC( 3, IG ) and PC( 4, IG ) are the major- and minor-axis standard
*  deviations respectively for the IGth Gaussian fit.  PC( 8, IG ) is the
*  corresponding Gaussian shape exponent.  For normal Gaussians just
*  square.  For arbitrary shaped Gaussians, scale the exponent
*  accordingly.  The squaring avoids have to record and restore the sign
*  of negative E1 and E2.
                     E3 = E1 * E1 / ( PC( 3, IG ) * PC( 3, IG ) )
                     E4 = E2 * E2 / ( PC( 4, IG ) * PC( 4, IG ) )
                     IF ( ABS( PC( 8, IG ) - 2.0D0 ) .GT. 1.D-6 ) THEN
                        E3 = E3 ** ( PC( 8, IG ) / 2.0D0 )
                        E4 = E4 ** ( PC( 8, IG ) / 2.0D0 )
                     END IF

                     E5 = EXP( -0.5 * ( E3 + E4 ) )
                     E6 = E5 * PC( 6, IG ) + PC( 7, IG )
                     GI = GI + E6
                  END DO

*  Weight the residuals.
                  FVEC( I ) = ( GI - DATA( I ) ) / VAR( I )
               ELSE
                  FVEC( I ) = 0.D0
               END IF
            END DO
         END DO

*  Equal weighting
*  ===============
      ELSE
         I = 0
         DO K = LBND( 2 ), UBND( 2 )
            YY = DBLE( K ) - 0.5D0
            DO J = LBND( 1 ), UBND( 1 )
               I = I + 1
               IF ( DATA( I ) .NE. VAL__BADD ) THEN
                  XX = DBLE( J ) - 0.5D0

                  GI = 0.0D0
                  DO IG = 1, NBEAMS

*  Define useful variables.
                     CT = COS( PC( 5, IG ) )
                     ST = SIN( PC( 5, IG ) )

*  Evaluate sum of Gaussian fits.  Recall PC( 1, IG ) and PC( 2, IG ) 
*  are the x and y centre co-ordinates respectively for the IGth 
*  Gaussian.
                     E1 = ( XX - PC( 1, IG ) ) * CT +
     :                    ( YY - PC( 2, IG ) ) * ST
                     E2 = ( YY - PC( 2, IG ) ) * CT -
     :                    ( XX - PC( 1, IG ) ) * ST

*  PC( 3, IG ) and PC( 4, IG ) are the major- and minor-axis standard
*  deviations respectively for the IGth Gaussian fit.  PC( 8, IG ) is the
*  corresponding Gaussian shape exponent.  For normal Gaussians just
*  square.  For arbitrary shaped Gaussians, scale the exponent
*  accordingly.  The squaring avoids have to record and restore the sign
*  of negative E1 and E2.
                     E3 = E1 * E1 / ( PC( 3, IG ) * PC( 3, IG ) )
                     E4 = E2 * E2 / ( PC( 4, IG ) * PC( 4, IG ) )
                     IF ( ABS( PC( 8, IG ) - 2.0D0 ) .GT. 1.D-6 ) THEN
                        E3 = E3 ** ( PC( 8, IG ) / 2.0D0 )
                        E4 = E4 ** ( PC( 8, IG ) / 2.0D0 )
                     END IF

                     E5 = EXP( -0.5 * ( E3 + E4 ) )
                     E6 = E5 * PC( 6, IG ) + PC( 7, IG )
                     GI = GI + E6
                  END DO

*  Form the residuals.
                  FVEC( I ) = GI - DATA( I )
               ELSE
                  FVEC( I ) = 0.D0
               END IF
            END DO
         END DO
      END IF

      END
