      SUBROUTINE JCMT_3POS_CONFN (BSEP, PIXSEP, NPIX, NCFN, CONF)
*+
*  Name:
*     JCMT_3POS_CONFN

*  Purpose:
*     Generate a convolution function to remove the chop function from
*     3-position chopped maps.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_3POS_CONFN (BSEP, PIXSEP, NPIX, NCFN, CONF)

*  Description:
*     This routine will return the asymmetric convolution function
*     required for deconvolving a 3-position chopped map into a single beam
*     map.
*
*     This function consists of a series of delta functions at a spacing equal
*     to that between a -ve spike and the central spike of the 3-position chop.
*     The height of the delta functions has a peak at the centre of the
*     convolution function and falls off by 1 at a time for the delta functions
*     on either side so that the envelope of the function is an isosceles
*     triangle.
*
*     The convolution must cover the map even when the centre of the function
*     is at the left or right hand extremity of the map. Hence the convolution
*     function must be twice the length of the raw map.
*
*     Since the raw data is not sampled such that the chop spacing is an
*     integer number of samples, the actual convolution function must be
*     rebinned onto the sample mesh by sinc interpolation.
*
*  Arguments:
*     BSEP = DOUBLE PRECISION (Given)
*        The beam separation in arcseconds (measured between the 2 negative
*        spikes flanking the central spike).
*     PIXSEP = DOUBLE PRECISION (Given)
*        The pixel separation in arcseconds
*     NPIX = INTEGER (Given)
*        The number of pixels in the x direction
*     NCFN = INTEGER (Returned)
*        The length of the convolution array
*     CONF( * ) = REAL (Returned)
*        The convolution function

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*      6-JUL-1993 (REVAD::JFL): Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'ASTRO_PAR'        ! astronomical constants

*  Arguments Given:
      DOUBLE PRECISION BSEP, PIXSEP
      INTEGER NPIX

*  Arguments Returned:
      REAL CONF( * )
      INTEGER NCFN


*  Local Variables:
      REAL PIXBSEP               ! the beam separation measured in
                                 !  pixels
      INTEGER NDELTA             ! the number of delta functions in the
                                 ! convolution array
      INTEGER I, J               ! DO loops
      REAL SUM                   ! temporary sum of sinc interpolation
      REAL DX                    ! distance from start of convolution
                                 ! array in pixels
      REAL XDELTA                ! distance of delta function from centre of
                                 ! convolution function in pixels
      REAL HDELTA                ! height of delta function

*  Internal References:
      REAL SINC                  ! SIN(PI*X)/(PI*X)

*.

*  convolution function must be twice size of map, odd number of points
*  because function is symmetric about central pixel.

      NCFN = NPIX * 2 - 1

*  the half-beam separation between a -ve spike and the central spike in map
*  pixels

      PIXBSEP = ABS (BSEP / (2.0d0 * PIXSEP))

*  calculate the number of delta functions in the convolution
*  function. This is equal to the number of half-beams covered by the map size,
*  plus 1 to give some overlap at the end of the map, times 2 to cover the whole
*  convolution function, plus 1 for the central delta function.

      NDELTA = (INT (DBLE(NPIX) / PIXBSEP) + 1) * 2 + 1

*  cycle through pixels in convolution function

      DO I = 1, NCFN

         SUM = 0

*  the position of the pixel relative to the centre of the convolution function

         DX = REAL (I - NPIX)

*  for each delta function...

         DO J = 1, NDELTA

*  its position relative to the centre of the convolution function (in pixels)

            XDELTA = REAL ((J-1) - (NDELTA-1)/2) * PIXBSEP

*  its height

            IF (J .LE. ((NDELTA-1)/2 + 1)) THEN
               HDELTA = REAL (J)
            ELSE
               HDELTA = REAL (NDELTA + 1 - J)
            END IF

*  add its contribution to this element of the convolution function by sinc
*  interpolation

            SUM = SUM + HDELTA * SINC(DX-XDELTA)

         END DO

         CONF(I) = SUM

      END DO

      END

