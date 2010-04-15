      SUBROUTINE SCULIB_GENSYCONFN( BSEP, PIXSEP, NPIX, NCFN, CONF,
     :  STATUS )
*+
*  Name:
*     SCULIB_GENSYCONFN

*  Purpose:
*     Generate a convolution function which will set to zero the
*     spatial frequencies with no signal in Dual Beam maps.


*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_GENSYCONFN( BSEP, PIXSEP, NPIX, NCFN, CONF, STATUS )

*  Description:
*     This routine will return the symmetric convolution function
*     required for setting to zero the spatial frequencies that have zero
*     sensitivity in the raw chop-scan data. This function is not described
*     in Emerson et al., 1979. A&A 96, 92, which (I think) describes an
*     early version of the NOD2 algorithm before all the wrinkles had been
*     properly understood.
*
*     This convolution function has to set to zero those points in the FT of
*     the raw data at zero frequency and harmonics of 1/chop spacing. In
*     the Fourier domain this means multiplying by a function which is 1
*     at all points apart from those specified, where it is zero. This is
*     the sum of 2 functions, the first being 1 everywhere, the second
*     being -1 at the points to be zeroed. The convolution function required
*     is the sum of the inverse FTs of these two functions. The inverse
*     FT of the first function is simply a delta function at the origin,
*     that of the second is a series of negative spikes with the first at the
*     origin and the others separated by the chop spacing.
*
*     The convolution function must cover the map even when the centre of
*     the function is at the left or right hand end of the map. Hence the
*     convolution function must be twice the length of the raw map.
*
*     Since the raw data is not sampled such that the chop spacing is an
*     integer number of samples, the actual convolution function must be
*     rebinned onto the sample mesh by sinc interpolation.
*
*     Otherwise the only tricky part of the algorithm is the need to normalise
*     the two component functions such that in the Fourier domain the
*     addition of the 2 functions does result in zeroes at the desired points.
*     This is most easily achieved by concentrating on the zero spatial
*     frequency which is just the sum of all the points in each of the 2
*     functions. The first function is a delta function set to 1, so the
*     sum of that function is 1. The sum of the second function is that of
*     all the points in the convolution function from its centre to one end,
*     a length that corresponds to the map size. This is the length to be
*     used because, in the Fourier domain, the width of the sample to be
*     set to zero is 1/map_size.
*
*  Arguments:
*     BSEP = REAL (Given)
*        The beam separation in arcseconds
*     PIXSEP = REAL (Given)
*        The pixel separation in arcseconds
*     NPIX = INTEGER (Given)
*        The number of pixels in the x direction
*     UNBAL = REAL (Given)
*        The relative amplitudes of the right and left hand beams
*        UNBAL=amp(lhb)/abs(amp(rhb))
*     NCFN = INTEGER (Returned)
*        The length of the convolution array
*     CONF( * ) = REAL (Returned)
*        The convolution function
*     STATUS = INTEGER (Given and returned)
*        Global status

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     JFL: John Lightfoot (RoE)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     22-JAN-1990 (JBVAD::PAH):
*        Original version.
*      3-MAY-1991 (REVAD::JFL): Comments and description updated.
*     12-OCT-1995 (jfl@roe.ac.uk): Renamed to SCULIB_GENSYCONFN.
*      6-JAN-1997 (jfl@roe.ac.uk): Changed calculation of SUMT, added in
*                                  contribution from more delta functions
*                                  beyond the ends of the convolution
*                                  function range.
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
      REAL PIXBSEP               ! the beam separation measured in pixels
      INTEGER NDELTA             ! the number of delta functions in the
                                 ! convolution array
      INTEGER I                  ! counter
      REAL SUM                   ! temporary sum of sinc interpolation
      REAL SUMT                  ! temporary sum of sinc interpolation
      REAL DX                    ! distance from start of convolution
                                 ! array in pixels
      REAL B1                    ! position in pixels of the current
                                 ! delta function
      INTEGER J                  ! counter
      INTEGER NSINC              ! number of negative delta functions

*  External References:
      REAL SCULIB_SINC           ! SIN(PI*X)/(PI*X)


*.

      IF (STATUS .NE. SAI__OK) RETURN

*  convolution function must be twice size of map, odd number of points
*  because function is symmetric about central pixel.

      NCFN = NPIX*2 - 1

*  the beam separation in map pixels

      PIXBSEP = BSEP / PIXSEP

* !!!calculation of second component of convolution function first...

*  calculate the number of delta functions in the convolution function.
*  In principle the convolution function has infinite length but the
*  delta functions far off have such a small effect that they can be ignored.
*  Therefore, take into account only those delta functions covered by the
*  length of the convolution function plus 5 outside it.

      NSINC = INT (REAL(NPIX) / PIXBSEP) + 5  ! no. deltas on + side of origin
      NDELTA = NSINC*2 + 1                    ! total number over whole fn

*  cycle through pixels in convolution function

      DO I = 1, NCFN
         DX = REAL (I-NPIX)                ! position in pixels rel to centre
         SUM = 0.0
         B1 = 0.0 - REAL(NSINC)*PIXBSEP    ! position of first delta in pixels
                                           ! rel to centre

*  then, for each delta function...

         DO J = 1, NDELTA

*  ...add its contribution to this element of the convolution function by
*  sinc interpolation

            SUM = SUM + SCULIB_SINC(DX-B1)
            B1 = B1 + PIXBSEP
         END DO

*  the delta functions are negative

         CONF(I) = - SUM
      END DO

*  calculate area under half the convolution function (same length as map)

      SUMT = 0.0
      DO I = 1, NPIX
         SUMT = SUMT + CONF(I)
      END DO

*  normalize the convolution function so that sum over map length is -1

      DO I = 1, NCFN
         CONF(I) = CONF(I) / SUMT
      END DO

* !!! now add in the first component of the convolution function as described
*     above. This is a delta function of unit height at the centre pixel.

      CONF(NPIX) = CONF(NPIX) + 1.0

      END
