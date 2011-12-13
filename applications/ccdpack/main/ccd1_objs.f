      SUBROUTINE CCD1_OBJS( FRAME, XDIM, YDIM, XORIG, YORIG, XPOS, YPOS,
     :                      INTENS, NOBJ, GSIGM, CROSS, COMIX, ELLIP,
     :                      ANGLE, BACK, SAT, NSIGMA, EXPAND, SCALE,
     :                      STATUS )
*+
*  Name:
*     CCD1_OBJS

*  Purpose:
*     To add PISA type model stars to an image at the positions and
*     with the intensities given.  These stars can also have an
*     ellipticity at a specific position angle.

*  Language:
*     Starlink-Fortran-77

*  Invocation:
*      CALL CCD1_OBJS( FRAME, XDIM, YDIM, XORIG, YORIG, XPOS, YPOS,
*                      INTENS, NOBJ, GSIGM, CROSS, COMIX, ELLIP,
*                      ANGLE, BACK, SAT, NSIGMA, EXPAND, SCALE,
*                      STATUS )

*  Description:
*     The routine generates model objects using the analytical function
*     used by PISAFIND ie. a mixed gaussian - lorentzian - exponential.
*     The objects positions and integrated intensities are given.  The
*     output objects can be elliptical. If the expand factor is true
*     then the objects are produced on an expanded image scale (
*     expanded by factor scale ) with preserved intensity. This
*     oversamples the objects and is useful if the output is to be used
*     for PSF resampling. If expand is set to false the output image is
*     at the normal scale. If scale is set to larger than one in this
*     mode then a pseudo integration is performed on a grid scale*scale
*     within each pixel. This improves the accuracy of the intensities
*     within each pixel. The output values are confined to be less
*     equal to a given saturation value. An illegal secondary function
*     is used for the model intensities, this is a hang-on from the
*     routine APM original.

*  Arguments:
*     FRAME( XDIM, YDIM ) = REAL (Returned)
*        The output array containing the generated objects.
*     XDIM, YDIM = INTEGER (Given)
*        The size of the output array.
*     XORIG, YORIG = INTEGER (Given)
*        The offsets of the start of the output array in the coordinates
*        of the output objects.
*     XPOS( NOBJ ) = DOUBLE PRECISION (Given)
*        X position  of objects
*     YPOS( NOBJ ) = DOUBLE PRECISION (Given)
*        Y position of objects
*     INTENS( NOBJ ) = DOUBLE PRECISION (Given)
*        Integrated intensities of objects.
*     NOBJ = INTEGER (Given)
*        The number of objects to generate.
*     GSIGM = _REAL (Read)
*        The value of the gaussian sigma.
*     CROSS = _REAL (Read)
*        The value of the percentage cross over point, from the
*        gaussian core to the exponential wings.
*     COMIX = _REAL (Read)
*        The value of the mixture ratio between the lorentzian
*        and the other functions
*     ELLIP( NOBJ )= DOUBLE PRECISION (Given)
*        The ellipticity of the generated objects.
*     ANGLE = REAL (Given)
*        The angle of the major axis of the output objects, measure in
*        degrees anti-clockwise with respect to the x-axis.
*     BACK = REAL (Given)
*        The background value for output array.
*     SAT = REAL (Given)
*        The highest possible value an output pixel can take. Any output
*        pixels with a value greater than this value will be set to it.
*     NSIGMA = REAL (Given)
*        The number of gaussian sigma to generate object out to.
*     EXPAND = LOGICAL (Given)
*        If true then the output data array is scale times larger than
*        the actual coordinates of the objects. The model data is
*        expanded by scale to fit this data frame. This mode essentially
*        produces an oversampled version of the output data. If false
*        then the data array is the same as the object coordinates. This
*        mode is the same as the oversampled mode except that the data
*        is then binned down by scale. This performs an integration
*        within each pixel if scale is larger than 1.
*     SCALE = INTEGER (Given)
*        The factor by which the data is to be oversampled by. See the
*        EXPAND description.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1992-1993 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1990 (PDRAPER):
*        Original version.
*     15-MAY-1992 (PDRAPER):
*        Changed to use passed lists of positions and intensities.
*     28-JUN-1993 (PDRAPER):
*        Added variable ellipticity option.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      COMMON /PM    / PARM1, PARM2, PARMN, PARSQ, CHANGE, PARRAD
      COMMON /PMN   / PARMN1, PARMN2, PARMNN, CMIX

*  Arguments Given:
      INTEGER XDIM, YDIM
      INTEGER XORIG, YORIG
      INTEGER NOBJ
      REAL GSIGM, CROSS, COMIX
      REAL ANGLE
      REAL BACK
      REAL SAT
      REAL NSIGMA
      LOGICAL EXPAND
      INTEGER SCALE
      DOUBLE PRECISION ELLIP( NOBJ )
      DOUBLE PRECISION XPOS( NOBJ )
      DOUBLE PRECISION YPOS( NOBJ )
      DOUBLE PRECISION INTENS( NOBJ )

*  Arguments Returned:
      REAL FRAME( XDIM, YDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL MAG                    ! intensity of star
      REAL PI, DEGRA, CMIX, PARMN1, PARM1, CHANGE, PARM2, PARRAD, PARMN,
     :     PARSQ, PARMN2, TEMP, PARMNN, A, B, RLIMSQ,
     :     EXTO2, CPHI, SPHI, ACONST, BCONST, X, Y, XOFF, YOFF, XOFFN,
     :     YOFFN, RADSQ, XX, YY, E, PHI, CROS
      INTEGER I, J, IEXT, K, IX, IY, IDX, IDY
      INTEGER XDUM, YDUM
      REAL RSCALE, RSCSQ

* External functions:
      REAL CCD1_TFUN
      EXTERNAL CCD1_TFUN
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

* set up some useful constants
      PI = 4.0*ATAN(1.0)
      DEGRA = PI/180.0
      RSCALE = REAL( SCALE )
      RSCSQ = RSCALE * RSCALE

*  Initialise frame
      DO 1 I = 1, XDIM
         DO 2 J = 1 , YDIM
            FRAME( I, J )= BACK
 2       CONTINUE
 1    CONTINUE

*  set up model parameters, these are as per-APM
      CMIX =  MIN( 0.999, MAX( 0.0, COMIX ) )
      CROS =  MIN( 100.0, MAX( 0.001, CROSS ) )
      PARM1 = -1.0 / ( GSIGM**2 )
      CHANGE = LOG( 0.01 * CROS )
      PARM2 = SQRT( -4.0 * CHANGE / GSIGM**2 )
      PARRAD = GSIGM * SQRT( -CHANGE )
      CROS = 0.01 * CROS
      PARMN = 1.0 /
     :       ( PI * GSIGM**2 * ( 1.0 + 0.5 * CROS/ LOG( 1.0 / CROS)))
      PARSQ = ( 5.0 * GSIGM )**2
      PARMN1 = -1.0 / ( GSIGM**2 )
      PARMN2 = 1.0 / ( LOG( 2.0 ) * GSIGM**2 )
      TEMP = LOG( 2.0 ) * LOG( 1.0 + PARMN2 * ( 4.0 * GSIGM )**2 )
      PARMNN = 1.0 / ( PI * GSIGM**2 * ( 1.0 - CMIX + TEMP * CMIX ))

*  scale the dimensions of the array to those of the sampling pseudo-array
      IF( EXPAND ) THEN
         XDUM = XDIM
         YDUM = YDIM
      ELSE
         XDUM = XDIM * SCALE
         YDUM = YDIM * SCALE
      END IF

      DO K = 1, NOBJ
*
*  set data for this object
         X = REAL ( XPOS( K ) )
         Y = REAL( YPOS( K ) )
         MAG = REAL( INTENS( K ) )
         E = REAL( ELLIP ( K ) )

*  elliptically symmetric profile
         IF ( E .NE. 0.0 )  THEN
            A = 1.0 / SQRT( 1.0 - E )
            B = SQRT( 1.0 - E )
            PHI = ANGLE * DEGRA
         ELSE
            PHI = 0.0
            E = 0.0
            A = 1.0
            B = 1.0
         END IF

*  go through list and add in contributions, scale nsigma to allow
*  for oversampling
         RLIMSQ = ( REAL( NSIGMA ) * RSCALE * GSIGM )**2
         IEXT = NINT( 2.0 * REAL( NSIGMA ) * RSCALE * GSIGM )
         EXTO2 = 0.5 + REAL( IEXT ) / 2.0
         CPHI = COS( PHI )
         SPHI = SIN( PHI )
         ACONST = 1.0 / ( A * A )
         BCONST = 1.0 / ( B * B )
*
*  correct for origin offset and scaling factor, this maps display
*  pixels to pseudo-array pixels. The pseudo array is the oversampled
*  size ( ie at 1/scale th of a pixel )
         X =  X * RSCALE + 1.5 - REAL( XORIG )
         Y =  Y * RSCALE + 1.5 - REAL( YORIG )
         DO I = 1, IEXT
            XOFF = I - EXTO2
            IX = NINT( X + XOFF )
            IF ( IX .GE. 1  .AND.  IX .LE. XDUM ) THEN
               DO J = 1, IEXT
                  YOFF = J - EXTO2
*  equivalent ellipse radius
                  XOFFN =  XOFF * CPHI + YOFF * SPHI
                  YOFFN = -XOFF * SPHI + YOFF * CPHI
                  RADSQ = XOFFN * XOFFN * ACONST +
     :                    YOFFN * YOFFN * BCONST
                  IF ( RADSQ .LE. RLIMSQ ) THEN
                     IY = NINT( Y + YOFF )
                     IF ( IY .GE. 1  .AND.  IY .LE. YDUM ) THEN

*  ix iy are map coordinates of point
                        XX = IX - X
                        YY = IY - Y
                        XOFFN = XX * CPHI + YY * SPHI
                        YOFFN = -XX * SPHI + YY * CPHI
                        RADSQ = XOFFN * XOFFN * ACONST +
     :                          YOFFN * YOFFN * BCONST

*  Scale this radius by the inverse scaling factor.
*  If expand is true then the scaling factor is real and the ix and iy
*  represent the real pixels coords, if expand is false then ix and iy
*  represent pseudo pixels and need to be scaled down. Contributions
*  are averaged by the scaling factor, if expand is false.
                        RADSQ = RADSQ / RSCSQ
                        IF ( .NOT. EXPAND ) THEN
                           IDX = IX / SCALE
                           IF ( IDX * SCALE .NE. IX ) IDX = IDX + 1
                           IDY = IY / SCALE
                           IF ( IDY * SCALE .NE. IY ) IDY = IDY + 1
                           FRAME( IDX, IDY ) = FRAME( IDX, IDY ) +
     :                                 MAG * CCD1_TFUN( RADSQ ) / RSCSQ
*  saturate if necessary
                           FRAME( IDX, IDY )=MIN( FRAME( IDX,IDY ),SAT )
                        ELSE

*  pixels values are correct for expanded frame.
                           FRAME( IX, IY ) = FRAME( IX, IY ) +
     :                                       MAG * CCD1_TFUN( RADSQ )
                           FRAME( IX, IY ) = MIN( FRAME( IX, IY ), SAT )
                        END IF
                     END IF
                  END IF
               END DO
            END IF
         END DO
      END DO

*  All done
      END
* $Id$
