      SUBROUTINE SCULIB_WTFN_REGRID_2 (RES, IN_DATA,
     :     IN_VARIANCE, WEIGHT, USEVARWT, VARWT, X, Y, NPIX,
     :     PIXSPACE, NI, NJ, ICEN, JCEN,
     :     TOTAL_WEIGHT, CONV_DATA_SUM, CONV_VARIANCE_SUM,
     :     CONV_WEIGHT, WEIGHTSIZE, SCLSZ, WTFN, STATUS)
*+
*  Name:
*     SCULIB_WTFN_REGRID_2

*  Purpose:
*     Perform convolution

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_WTFN_REGRID_2 (RES, IN_DATA, IN_VARIANCE,
*    :  WEIGHT, USEVARWT, VARWT, X, Y, NPIX, PIXSPACE, NI, NJ, ICEN, JCEN,
*    :  TOTAL_WEIGHT, CONV_DATA_SUM, CONV_VARIANCE_SUM,
*    :  CONV_WEIGHT, WEIGHTSIZE, SCLSZ, WTFN, STATUS)

*  Description:
*     This routine convolves the input data with a weighting function
*     onto a regularly spaced output grid.
*     The weighting function is stored in a lookup table indexed
*     by the square of the distance and is parametrized by RES,
*     WEIGHTSIZE, SCLSZ and WTFN.
*
*     This is an image space implementation of fourier techniques.
*     In Fourier terms the technique could be described
*     as follows:-
*     -[1] Do a 2-d discrete Fourier transform of the data points. The result
*     is a repeating pattern made up of copies of the transform of
*     the map as a continuous function, each copied displaced from its
*     neighbours by 1/dx, where dx is the sample spacing of the input points
*     (assumed equal in x and y). Different copies of the 'continuous map'
*     transforms will overlap ('alias') if there is any power in the
*     map at frequencies greater than 1/(2dx). It is not possible to unravel
*     aliased spectra and this constraint leads to the Nyquist sampling
*     criterion.
*
*     -[2] We want to derive the 'continuous map' so that map values on the new
*     grid mesh can be derived. Do this by multiplying the transform of the
*     data by a function that has zero value beyond a radius of 0.5/dx from the
*     origin. This will get rid of all the repeats in the pattern and leave
*     just the transform of the 'continuous map'.
*
*     -[3] Do an inverse FT on the remaining transform for the points where you
*     wish the resampled points to be (note: FFTs implicitly assume that the
*     data being transformed DO repeat ad infinitum so we'd have to be careful
*     when using them to do this).
*
*     The analogue of these process steps in image space is as follows:
*     -[1] Nothing.
*     -[2] Convolve the input data with the FT of the function used to isolate
*     the `continuous map' transform.
*     -[3] Nothing.

*     If the method is done properly, the rebinned map is in fact the map on
*     the new sample mesh that has the same FT as the continuous function
*     going through the original sample points.
*
*     Convolution Functions:-
*     -[Bessel]
*     For good data and with no time constraint on reduction the best
*     convolution function would be one whose FT is a flat-topped cylinder in
*     frequency space, centred on the origin and of a radius such that
*     frequencies to which the telescope is not sensitive are set to zero.
*     Unfortunately, this function is a Bessel function, which extends to
*     infinity along both axes and has significant power out to a large radius.
*     To work correctly this would require an infinite map and infinite
*     computing time. However, a truncated Bessel function should work well on
*     a large map, except near the edges. Edge effects can be removed by
*     pretending that the map extends further - of course, this only works if
*     you know what data the pretend map area should contain, i.e. zeros.
*     Another problem with a Bessel function arises from the fact that it does
*     truncate the FT of the map sharply. If the data are good then there
*     should be nothing but noise power at the truncation radius and the
*     truncation of the FT should have no serious effect. However, if the data
*     has spikes (power at all frequencies in the FT) or suffers from seeing
*     effects such that the data as measured DO have power beyond the
*     truncation radius, then this will cause ringing in the rebinned map.
*     -[Gaussian]
*     In fact, any function that is finite in frequency space will have
*     infinite extent in image space (I think). As such they will all
*     drag in some power from the aliased versions of the map transform
*     and all suffer from edge effects and large compute times. Some are
*     worse than others, however. For example, a Gaussian can have most
*     of its power concentrated over a much smaller footprint than a
*     Bessel function, so the convolution calculation will be much
*     faster. It is also more robust in the presence of spikes and
*     seeing problems because it does not truncate the map FT as sharply
*     as the Bessel function - such effects give rise to smoother
*     defects in the rebinned map though the defects will still BE
*     there.

*  Arguments:
*     RES                            = INTEGER (Given)
*        number of resolution elements per scale length
*     IN_DATA (NPIX)                   = REAL (Given)
*        The input data values.
*     IN_VARIANCE (NPIX)               = REAL (Given)
*        Variance on IN_DATA.
*     WEIGHT                           = REAL (Given)
*        The weight of this dataset.
*     USEVARWT                         = LOGICAL (Given)
*        Are we using a different weight for each point
*     VARWT (NPIX)                     = REAL (Given)
*        The different weight for each pixel
*     X (NPIX)                         = DOUBLE PRECISION (Given)
*        The x coordinates of the input pixels.
*     Y (NPIX)                         = DOUBLE PRECISION (Given)
*        The y coordinates of the input pixels.
*     NPIX                             = INTEGER (Given)
*        the number of input pixels.
*     PIXSPACE                         = REAL (Given)
*        the pixel spacing of the output map.
*     NI                               = INTEGER (Given)
*        The number of output pixels in the x direction.
*     NJ                               = INTEGER (Given)
*        The number of output pixels in the y direction.
*     ICEN                             = INTEGER (Given)
*        the x index of the centre of the output array.
*     JCEN                             = INTEGER (Given)
*        the y index of the centre of the output array.
*     TOTAL_WEIGHT (NI,NJ)             = REAL (Given)
*        the `total weight' of each output pixel.
*     CONV_DATA_SUM (NI,NJ)            = REAL (Given and returned)
*        the convolution sum for each output pixel.
*     CONV_VARIANCE_SUM (NI,NJ)        = REAL (Given and returned)
*        the variance convolution sum for each output pixel.
*     CONV_WEIGHT (NI,NJ)              = REAL (Given and returned)
*        the convolution weight for each output pixel.
*     WEIGHTSIZE                       = INTEGER (Given)
*        radius of weight function in scale units (SCUIP__FILTRAD for BESSEL,
*        1 for LINEAR)
*     SCLSZ                            = REAL
*        1 scale length in the same units as PIXSPACE
*     WTFN (RES * RES * WEIGHTSIZE * WEIGHTSIZE) = REAL (Given)
*        convolution weighting function
*     STATUS                           = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     John Lightfoot (jfl@roe.ac.uk)
*     Tim Jenness (JAC)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.15  2005/03/23 03:48:21  timj
*     No longer use wavelength + diameter for determining resolution element. Use
*     scale+weightsize throughout
*
*     Revision 1.14  1999/08/19 03:37:33  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.13  1999/08/06 02:24:52  timj
*     Tweak headers for use with PROLAT.
*
*     21-AUG-1995: original version, adapted from JCMT_BESSEL_REGRID_2.

*  Bugs:

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values

*  Arguments Given:
      INTEGER RES
      INTEGER NPIX
      REAL    IN_DATA (NPIX)
      REAL    IN_VARIANCE (NPIX)
      REAL    WEIGHT
      REAL    WWEIGHT
      DOUBLE PRECISION X(NPIX)
      DOUBLE PRECISION Y(NPIX)
      REAL    PIXSPACE
      INTEGER NI
      INTEGER NJ
      INTEGER ICEN
      INTEGER JCEN
      REAL    SCLSZ
      REAL    TOTAL_WEIGHT (NI,NJ)
      LOGICAL USEVARWT
      INTEGER WEIGHTSIZE
      REAL    VARWT(NPIX)
      REAL    WTFN(RES * RES * WEIGHTSIZE * WEIGHTSIZE + 1)

*  Arguments Returned:
      REAL    CONV_DATA_SUM (NI, NJ)
      REAL    CONV_VARIANCE_SUM (NI, NJ)
      REAL    CONV_WEIGHT (NI, NJ)

*  Status:
      INTEGER STATUS

*  Local Constants:

*  Local Variables:
      INTEGER ICPIX                              ! index in convolution function
                                                 ! corresponding to RPIX
      INTEGER INEAR                              ! I index of output pixel
                                                 ! nearest to input
      INTEGER IOUT                               ! I pixel index in output array
      INTEGER JNEAR                              ! J index of output pixel
                                                 ! nearest to input
      INTEGER JOUT                               ! J pixel index in output array
      INTEGER PIX                                ! input pixel number
      INTEGER PIX_RANGE                          ! radius over which convolution
                                                 ! function is non-zero, in
                                                 ! pixels
      REAL    RES_ELEMENT                        ! critical sample spacing at
                                                 ! WAVELENGTH (radians)
      REAL    RPIX                               ! distance between input and
                                                 ! output pixel
      REAL    RTEMP                              ! scratch real
      REAL    WT                                 ! value of convolution
                                                 ! function at output pixel
      REAL    XINC                               ! x-axis pixel increment
      REAL    XPIX                               ! x offset of output pixel
      REAL    YINC                               ! y-axis pixel increment
      REAL    YPIX                               ! y offset of output pixel

      INTEGER FILTER1_SQ
      INTEGER  FILTER_RAD_SQ
      REAL    SCALE
      REAL    SCALESQ
      REAL    RES_SCAL
      REAL    RAD_OV_SCAL
      REAL    SMALL
*   local data
*.

      IF (STATUS .NE. SAI__OK) RETURN

* Set up small to prevent comparing REAL to 0.0

      SMALL = VAL__SMLR

*  set x and y axis pixel increments, x increases to left hence -ve.

      XINC = -PIXSPACE
      YINC = PIXSPACE

* Some time saving squares

      FILTER1_SQ = RES * RES
      FILTER_RAD_SQ = WEIGHTSIZE * WEIGHTSIZE

*  ..extent of convolution function in units of output pixels

      SCALE = 1.0 / SCLSZ

      SCALESQ = SCALE * SCALE

      RAD_OV_SCAL = REAL(FILTER_RAD_SQ) / SCALESQ
      RES_SCAL = REAL(FILTER1_SQ) * SCALESQ


      RTEMP = REAL(WEIGHTSIZE) * SCLSZ / PIXSPACE
      PIX_RANGE = INT (RTEMP) + 1

*  now do the convolution, looping over the input pixels

      DO PIX = 1, NPIX

         IF (IN_DATA(PIX) .NE. VAL__BADR) THEN

*  find the coords (INEAR, JNEAR) of the output pixel nearest to the current
*  input pixel.

            INEAR = NINT (REAL(X(PIX))/XINC) + ICEN
            JNEAR = NINT (REAL(Y(PIX))/YINC) + JCEN

*     Check that INEAR or JNEAR are allowed (they may be outside the
*     box

            IF ((INEAR .GT. 0) .AND. (INEAR .LE. NI) .AND.
     :           (JNEAR .GT. 0) .AND. (JNEAR .LE. NJ)) THEN

*  loop over x's and y's in output array that are covered by the convolution
*  function centred at (INEAR, JNEAR)

               DO JOUT = MAX(1,JNEAR-PIX_RANGE), MIN(NJ,JNEAR+PIX_RANGE)
                  DO IOUT = MAX(1,INEAR-PIX_RANGE),
     :                 MIN(NI,INEAR+PIX_RANGE)

*  add into the convolution result and weight arrays unless TOTAL_WEIGHT
*  of output is zero, signifying output pixel is beyond limits of
*  mapped area. The coaddition is normalised by the `total weight'
*  associated with this input pixel.

                     IF (TOTAL_WEIGHT (IOUT,JOUT) .GT. SMALL) THEN

*  work out x,y offset of current output pixel

                        YPIX = REAL (JOUT-JCEN) * YINC
                        XPIX = REAL (IOUT-ICEN) * XINC

*  distance between output and input pixels, in arcsec**2 units

                        RPIX = (YPIX-REAL(Y(PIX)))**2 +
     :                       (XPIX-REAL(X(PIX)))**2

* Now work out which part of the weight function to use
* I used to do this:
*     RPIX = RPIX * SCALESQ    ! Distance in scale lengths**2
*     IF (RPIX .LT. radius**2) THEN
*           ICPIX = NINT(FILTER1_SQ * RPIX)
*     etc..
*     Remove RPIX * SCALESQ since this is not really necessary

                        WT = 0.0
*     Is the distance close enough? If so find the weight value
                        IF (RPIX .LT. RAD_OV_SCAL) THEN
                           ICPIX = NINT(RES_SCAL * RPIX)
                           WT = WTFN(ICPIX+1)

* Change INEAR,JNEAR to IOUT,JOUT for TOTAL_WEIGHT?
                           WWEIGHT = WT * WEIGHT /
     :                          TOTAL_WEIGHT (INEAR,JNEAR)

                           IF (USEVARWT) WWEIGHT = WWEIGHT * VARWT(PIX)

                           CONV_WEIGHT (IOUT,JOUT) =
     :                          CONV_WEIGHT(IOUT,JOUT)
     :                          + WWEIGHT
                           CONV_DATA_SUM (IOUT,JOUT) =
     :                          CONV_DATA_SUM (IOUT,JOUT) +
     :                          WWEIGHT * IN_DATA(PIX)
                           CONV_VARIANCE_SUM (IOUT,JOUT) =
     :                          CONV_VARIANCE_SUM (IOUT,JOUT) +
     :                          (WWEIGHT)**2 * IN_VARIANCE (PIX)

                        END IF

                     END IF

                  END DO
               END DO

            END IF

         END IF

      END DO

      END
