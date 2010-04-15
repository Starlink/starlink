      SUBROUTINE SCULIB_3POS_CONFN (BSEP, PIXSEP, NPIX, NCFN, CONF,
     :  STATUS)
*+
*  Name:
*     SCULIB_3POS_CONFN

*  Purpose:
*     Generate a convolution function to remove the 3-position
*     chop function from raster scans.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_3POS_CONFN (BSEP, PIXSEP, NPIX, NCFN, CONF,
*    :  STATUS)

*  Description:
*     This routine will return the convolution function
*     required for deconvolving the 3-position chop from a scan.
*
*     This function consists of a series of delta functions at a spacing
*     equal to that between a -ve spike and the central spike of the
*     3-position chop. The height of the delta dunctions peaks at the
*     centre of the convolution function and falls off by 1 at a time for
*     the delta functions on either side, so that the envelope of the
*     function is an isosceles triangle.
*
*     The convolution function must cover the scan even when the centre of
*     the function is at one end of it. Hence, the convolution function
*     must be twice the length of the scan.
*
*     Since the raw data are not sampled such that the chop spacing is an
*     integer number of samples, the actual convolution function must be
*     rebinned onto the sample mesh by sinc interpolation.
*
*  Arguments:
*     BSEP = REAL (Given)
*        The beam separation in arcseconds
*     PIXSEP = REAL (Given)
*        The pixel separation in arcseconds
*     NPIX = INTEGER (Given)
*        The number of pixels in the x direction
*     NCFN = INTEGER (Returned)
*        The length of the convolution array
*     CONF( * ) = REAL (Returned)
*        The convolution function
*     STATUS = INTEGER (Given and returned)
*        Global status

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     30-OCT-1995: original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      REAL BSEP, PIXSEP
      INTEGER NPIX

*  Arguments Returned:
      REAL CONF( * )
      INTEGER NCFN
      INTEGER STATUS

*  Local Variables:
      REAL    DX                 ! distance from start of convolution array
                                 ! in pixels
      REAL    HDELTA             ! height of delta function
      INTEGER I                  ! DO loop counter
      INTEGER J                  ! DO loop counter
      INTEGER NDELTA             ! the number of delta functions in the
                                 ! convolution array
      REAL    PIXBSEP            ! the beam separation measured in
                                 ! pixels
      REAL    SUM                ! temporary sum of sinc interpolation
      REAL    XDELTA             ! distance of delta function from centre
                                 ! of convolution function in pixels
*  External References:
      REAL    SCULIB_SINC        ! SIN(PI*X)/(PI*X)

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  convolution function must be twice size of map, odd number of points
*  because function is symmetric about central pixel.

      NCFN = NPIX * 2 - 1

*  the half-beam separation, between a -ve spike and the central spike
*  (in map pixel units)

      PIXBSEP = ABS (BSEP / (2.0 * PIXSEP))

*  calculate the number of delta functions in the convolution function.
*  This is equal to the number of half-chops covered in the scan length,
*  plus 1 to give some overlap at the end of the scan, times 2 to cover
*  the whole convolution function, plus 1 for the central delta function.

      NDELTA = (INT (ABS(REAL(NPIX)/PIXBSEP)) + 1) * 2 + 1

*  cycle through pixels in convolution function

      DO I = 1, NCFN

         SUM = 0.0

*  the position of the pixel relative to the centre of the convolution
*  function

         DX = REAL (I - NPIX)

*  for each delta function

         DO J = 1, NDELTA

*  its position relative to the centre of the convolution function (in
*  pixels)

            XDELTA = REAL ((J-1) - (NDELTA-1)/2) * PIXBSEP

*  its height

            IF (J .LE. ((NDELTA-1)/2 + 1)) THEN
               HDELTA = REAL (J)
            ELSE
               HDELTA = REAL (NDELTA + 1 - J)
            END IF

*  add its contribution to this element of the convolution by sinc
*  interpolation

            SUM = SUM + HDELTA * SCULIB_SINC (DX - XDELTA)

         END DO

         CONF(I) = SUM

      END DO

      END

