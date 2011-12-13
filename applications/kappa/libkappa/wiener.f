      SUBROUTINE WIENER( STATUS )
*+
*  Name:
*     WIENER

*  Purpose:
*     Applies a Wiener filter to a 1- or 2-dimensional array.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WIENER( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application filters the supplied one- or two-dimensional
*     array using a Wiener filter.  It takes an array holding observed
*     data and another holding a Point-Spread Function as input and
*     produces an output restored array with potentially higher
*     resolution and lower noise.  Generally superior results can be
*     obtained using applications MEM2D or LUCY, but at the cost of
*     much more processing time.
*
*     The Wiener filter attempts to minimise the mean squared
*     difference between the undegraded image and the restored image.
*     To do this it needs to know the power spectrum of the undegraded
*     image (i.e. the power at each spatial frequency before the
*     instrumental blurring and the addition of noise).  Obviously,
*     this is not usually available, and instead the power spectrum of
*     some other image must be used (the `model' image).  The idea is
*     that a model image should be chosen for which there is some a
*     priori reason for believing it to have a power spectrum similar
*     to the undegraded image.  Many different suggestions have been
*     made for the best way to make this choice and the literature
*     should be consulted for a detailed discussion (for instance, see
*     the paper "Wiener Restoration of HST Images: Signal Models and
*     Photometric Behavior" by I.C. Busko in the proceedings of the
*     first Annual Conference  on Astronomical Data Analysis Software
*     and Systems, Tucson).  By default, this application uses a
*     `white' model image, i.e. one in which there is equal power at
*     all spatial frequencies.  The default value for this constant
*     power is the mean power per pixel in the input image.  There is
*     also an option to use the power spectrum of a supplied model
*     image.
*
*     The filter also depends on a model of the noise in the supplied
*     image.  This application assumes that the noise is 'white' and is
*     constant across the image.  You can specify the noise power
*     to use.  If a noise power of zero is supplied, then the Wiener
*     filter just becomes a normal inverse filter which will tend to
*     amplify noise in the supplied image.
*
*     The filtering is done by multiplying the Fourier transform of the
*     supplied image by the Fourier transform of the filter function.
*     The output image is then created by taking the inverse Fourier
*     transform of the product.  The Fourier transform of the filter
*     function is given by:
*
*                 *
*                H
*          -------------
*             2      Pn
*          |H|   +  ----
*                    Pg
*
*     where H is the Fourier transform of the supplied Point-Spread
*     Function, Pn is the noise power, Pg is the power in the model
*     image, and the asterisk represents complex conjugation.  If the
*     supplied model includes noise (as indicated by Parameter QUIET)
*     then Pn is subtracted from Pg before evaluating the above
*     expression.

*  Usage:
*     wiener in psf out xcentre ycentre

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF containing the observed data.  This image may
*        contain bad values, in which case the bad values will be
*        replaced by zero before applying the filter.  The resulting
*        filtered image is normalised by dividing each pixel value by
*        the corresponding weight of the good input pixels.  These
*        weights are found by filtering a mask image which holds the
*        value one at every good input pixel, and zero at every bad
*        input pixel.
*     MODEL = NDF (Read)
*        An NDF containing an image to use as the model for the power
*        spectrum of the restored image.  Any bad values in this image
*        are replaced by the mean of the good values.  If a null value
*        is supplied then the model power spectrum is taken to be
*        uniform with a value specified by Parameter PMODEL. [!]
*     OUT = NDF (Write)
*        The restored output array.  An extension named WIENER is added
*        to the output NDF to indicate that the image was created by
*        this application (see Parameter QUIET).
*     PMODEL = _REAL (Read)
*        The mean power per pixel in the model image.  This parameter
*        is only accessed if a null value is supplied for parameter
*        MODEL.  If a value is obtained for PMODEL then the model image
*        is assumed to have the specified constant power at all spatial
*        frequencies.  If a null (!) value is supplied, the value used is
*        the mean power per pixel in the input image. [!]
*     PNOISE = _REAL (Read)
*        The mean noise power per pixel in the observed data.  For
*        Gaussian noise this is equal to the variance.  If a null (!)
*        value is supplied, the value used is an estimate of the noise
*        variance based on the difference between adjacent pixel values in
*        the observed data. [!]
*     PSF = NDF (Read)
*        An NDF holding an estimate of the Point-Spread Function (PSF)
*        of the input array.  This could, for instance, be produced
*        using the KAPPA application "PSF".  There should be no bad
*        pixels in the PSF otherwise an error will be reported.  The
*        PSF can be centred anywhere within the array, but the location
*        of the centre must be specified using parameters XCENTRE and
*        YCENTRE.  The PSF is assumed to have the value zero outside
*        the supplied NDF.
*     QUIET = _LOGICAL (Read)
*        This specifies whether or not the image given for parameter
*        MODEL (or the value given for Parameter PMODEL), includes
*        noise.  If the model does not include any noise then a TRUE
*        value should be supplied for QUIET.  If there is any noise in
*        the model then QUIET should be supplied FALSE.  If a null (!)
*        value is supplied, the value used is FALSE, unless the image
*        given for Parameter MODEL was created by a previous run of WIENER
*        (as indicated by the presence of a WIENER extension in the NDF),
*        in which case the run time default is TRUE (i.e. the previous
*        run of WIENER is assumed to have removed the noise). [!]
*     THRESH = _REAL (Read)
*        The fraction of the PSF peak amplitude at which the extents of
*        the PSF are determined.  These extents are used to derive the
*        size of the margins that pad the supplied input array.  Lower
*        values of THRESH will result in larger margins being used.
*        THRESH must be positive and less than 0.5.  [0.0625]
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null (!) value means using the
*        title of the input NDF. [!]
*     WLIM = _REAL (Read)
*        If the input array contains bad values, then this parameter
*        may be used to determine the minimum weight of good input
*        values required to create a good output value.  It can be
*        used, for example, to prevent output pixels from being
*        generated in regions where there are relatively few good input
*        values to contribute to the restored result.  It can also be
*        used to `fill in' small areas (i.e. smaller than the PSF) of
*        bad pixels.
*
*        The numerical value given for WLIM specifies the minimum total
*        weight associated with the good pixels in a smoothing box
*        required to generate a good output pixel (weights for each
*        pixel are defined by the normalised PSF).  If this specified
*        minimum weight is not present, then a bad output pixel will
*        result, otherwise a smoothed output value will be calculated.
*        The value of this parameter should lie between 0.0 and
*        1.0.  WLIM=0 causes a good output value to be created even if
*        there is only one good input value, whereas WLIM=1 causes a
*        good output value to be created only if all input values are
*        good. [0.001]
*     XCENTRE = _INTEGER (Read)
*        The x pixel index of the centre of the PSF within the supplied
*        PSF array.  The suggested default is the middle pixel (rounded
*        down if there are an even number of pixels per line).
*     YCENTRE = _INTEGER (Read)
*        The y pixel index of the centre of the PSF within the supplied
*        PSF array.  The suggested default is the middle line (rounded
*        down if there are an even number of lines).

*  Examples:
*     wiener cenA star cenA_hires 11 13
*        This example deconvolves the array in the NDF called cenA,
*        putting the resulting array in the NDF called cenA_hires.
*        The PSF is defined by the array in NDF star, and the centre
*        of the PSF is at pixel (11,13).
*     wiener cenA star cenA_hires 11 13 pnoise=0
*        This example performs the same function as the previous
*        example, except that the noise power is given as zero.  This
*        causes the Wiener filter to reduce to a standard inverse
*        filter, which will result in more high frequencies being
*        present in the restored image.
*     wiener cenA star cenA_hires 11 13 model=theory quiet
*        This example performs the same function as the first example,
*        except that the power spectrum of the restored image is
*        modelled on that of NDF theory, which may for instance
*        contain a theoretical model of the object in NDF cenA,
*        together with a simulated star field.  The Parameter QUIET is
*        set to a TRUE value to indicate that the theoretical model
*        contains no noise.

*  Notes:
*     - The convolutions required by the Wiener filter are performed by
*     the multiplication of Fourier transforms.  The supplied input
*     array is extended by a margin along each edge to avoid problems
*     of wrap-around between opposite edges of the array.  The width of
*     this margin is about equal to the width of the significant part
*     of the PSF (as determined by Parameter THRESH).  The application
*     displays the width of these margins.  The margins are filled by
*     replicating the edge pixels from the supplied input NDFs.

*  Related Applications:
*     KAPPA: FOURIER, LUCY, MEM2D.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single-precision floating point.

*  Copyright:
*     Copyright (C) 1995, 1998, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-MAR-1995 (DSB):
*        Original version, based on MEM2D and Rhys Morris's WIENER.
*     1995 March 29 (MJC):
*        Added commentary and related-applications section, corrected
*        typo's, used a modern-style variable declaration and other
*        stylistic changes, shortened long lines, and made message
*        reporting conditional.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality required
      PARAMETER( NDIM = 2 )

*  Local Variables:
      LOGICAL BAD1               ! Are there bad pixels in NDF 2?
      LOGICAL BAD2               ! Are there bad pixels in NDF 2?
      LOGICAL BAD3               ! Are there bad pixels in NDF 2?
      LOGICAL BAD4               ! Are there bad pixels in NDF 2?
      INTEGER DIMS1( NDIM )      ! Dimensions sizes in NDF1
      INTEGER DIMS2( NDIM )      ! Dimensions sizes in NDF1
      INTEGER INDF1              ! NDF id. for input data array
      INTEGER INDF2              ! NDF id. for PSF array
      INTEGER INDF3              ! NDF id. for model array
      INTEGER INDF3S             ! NDF id. for section of model
      INTEGER INDF4              ! NDF id. for output array
      INTEGER IP2                ! Pointer to internal file 2
      INTEGER IP3                ! Pointer to internal file 3
      INTEGER IP4                ! Pointer to internal file 4
      INTEGER IP5                ! Pointer to internal file 5
      INTEGER IP6                ! Pointer to internal file 6
      INTEGER IPN1               ! Pointer to input data array
      INTEGER IPN2               ! Pointer to PSF array
      INTEGER IPN3S              ! Pointer to section of model
      INTEGER IPN4               ! Pointer to output array
      INTEGER IPW0               ! Pointer to work array
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of all axes in an NDF
      REAL MEANM                 ! Mean value in model array
      REAL MEANM2                ! Mean squared value in model
      REAL MEANO                 ! Mean value in observed data array
      REAL MEANO2                ! Mean squared value in observed data
      INTEGER N                  ! No. of elements in an internal file
      INTEGER NDIMS              ! Total no. of dimensions in an NDF
      INTEGER NEL1               ! No. of elements in NDF1
      INTEGER NEL2               ! No. of elements in NDF2
      INTEGER NEL3S              ! No. of elements in NDF3S
      INTEGER NEL4               ! No. of elements in NDF4
      INTEGER NLIN               ! No. of rows in each internal file
      INTEGER NPIX               ! No. of columns in each internal file
      REAL PG                    ! Power per pixel in the model image
      REAL PN                    ! Noise power per pixel
      INTEGER PSFXSZ             ! Approx. size of the PSF along X axis
      INTEGER PSFYSZ             ! Approx. size of the PSF along Y axis
      LOGICAL QUIET              ! Is the model devoid of noise?
      INTEGER SDIM1( NDIM )      ! Indices of significant axes in NDF1
      INTEGER SDIM2( NDIM )      ! Indices of significant axes in NDF2
      INTEGER SDIM3( NDIM )      ! Indices of significant axes in NDF3
      INTEGER SLBND1( NDIM )     ! Low bounds of INDF1 significant axes
      INTEGER SLBND2( NDIM )     ! Low bounds of INDF2 significant axes
      INTEGER SLBND3( NDIM )     ! Low bounds of INDF3 significant axes
      REAL STDEV                 ! Approximate noise level in input
      INTEGER SUBND1( NDIM )     ! High bounds of INDF1 significant axes
      INTEGER SUBND2( NDIM )     ! High bounds of INDF2 significant axes
      INTEGER SUBND3( NDIM )     ! High bounds of INDF3 significant axes
      REAL THRESH                ! Truncation threshold of PSF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of all axes in an NDF
      INTEGER WDIMS( NDIM )      ! Dimensions of work array.
      REAL WLIM                  ! Min. weight of good pixels required
      INTEGER XCEN               ! X coordinate of PSF centre.
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator ot WIENER extension
      INTEGER XMARG              ! Width of margins on x axes
      INTEGER YCEN               ! Y coordinate of PSF centre.
      INTEGER YMARG              ! Width of margins on y axes

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ontain the input NDFs.
*  ======================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the NDF containing the input data.
      CALL KPG1_GTNDF( 'IN', NDIM, .FALSE., 'READ', INDF1, SDIM1,
     :                 SLBND1, SUBND1, STATUS )

*  Find the dimensions of the input array.
      DIMS1( 1 ) = SUBND1( 1 ) - SLBND1( 1 ) + 1
      DIMS1( 2 ) = SUBND1( 2 ) - SLBND1( 2 ) + 1

*  Get the NDF containing the PSF.
      CALL KPG1_GTNDF( 'PSF', NDIM, .FALSE., 'READ', INDF2, SDIM2,
     :                 SLBND2, SUBND2, STATUS )

*  Map the PSF DATA array.
      CALL KPG1_MAP( INDF2, 'DATA', '_REAL', 'READ', IPN2, NEL2,
     :              STATUS )

*  See if the PSF contains any bad pixels.  If it does then abort.
      CALL NDF_BAD( INDF2, 'Data', .TRUE., BAD2, STATUS )
      IF ( BAD2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF2 )
         CALL ERR_REP( 'WIENER_ERR1', 'PSF ''^NDF'' contains some bad '/
     :                 /'pixels.', STATUS )
         GO TO 999
      END IF

*  Find the dimensions of the PSF.
      DIMS2( 1 ) = SUBND2( 1 ) - SLBND2( 1 ) + 1
      DIMS2( 2 ) = SUBND2( 2 ) - SLBND2( 2 ) + 1

*  Obtain parameter values relating to the PSF.
*  ============================================

*  Get the co-ordinates of the centre of the PSF within the array.  The
*  default is the centre of the PSF array.
      CALL PAR_GDR0I( 'XCENTRE', SLBND2( 1 ) + DIMS2( 1 ) / 2,
     :                SLBND2( 1 ), SUBND2( 1 ), .FALSE., XCEN, STATUS )
      CALL PAR_GDR0I( 'YCENTRE', SLBND2( 2 ) + DIMS2( 2 ) / 2,
     :                SLBND2( 2 ), SUBND2( 2 ), .FALSE., YCEN, STATUS )

*  Get the truncation threshold as fraction of the peak amplitude of
*  the PSF.
      CALL PAR_GDR0R( 'THRESH', 0.0625, VAL__SMLR, 0.5, .FALSE., THRESH,
     :                STATUS )

*  Find the PSF sizes and padded-array dimensions.
*  ===============================================

*  Get work space for use in the routine for finding the PSF sizes.
      WDIMS( 1 ) = MAX( DIMS2( 2 ), DIMS2( 1 ) )
      WDIMS( 2 ) = 2
      CALL PSX_CALLOC( WDIMS( 1 ) * WDIMS( 2 ), '_REAL', IPW0, STATUS )

*  Abort if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the approximate width of the PSF along both array axes.  The
*  internal array files will be padded with a margin of this size to
*  reduce edge effects caused by wrap-around in the convolution
*  routines.
      CALL KPG1_PSFSR( %VAL( CNF_PVAL( IPN2 ) ), DIMS2( 1 ), DIMS2( 2 ),
     :                 %VAL( CNF_PVAL( IPW0 ) ),
     :                 WDIMS( 1 ), WDIMS( 2 ), THRESH,
     :                 4, PSFXSZ, PSFYSZ, STATUS )

*  Release work space used by KPG1_PSFSR.
      CALL PSX_FREE( IPW0, STATUS )

*  Add this margin on to the input array dimensions to get the
*  dimensions of the internal arrays.
      XMARG = PSFXSZ
      YMARG = PSFYSZ
      NPIX = DIMS1( 1 ) + 2 * XMARG
      NLIN = DIMS1( 2 ) + 2 * YMARG

*  Find the FFT of the PSF.
*  ========================

*  The 2-d Hermitian FFT routines (KPG1_FFTFR and KPG1_FFTBR) require
*  a work array containing at least ( 3*MAX(M,N)+15 ) elements, where
*  M and N are the dimensions of the array being FFTed.  Ensure that
*  an internal file is large enough to provide at least this much work
*  space.
      NPIX = MAX( 6, NPIX )
      NLIN = MAX( 6, NLIN )
      N = NPIX * NLIN

*  Update the margin size accordingly.
      XMARG = ( NPIX - DIMS1( 1 ) ) / 2
      YMARG = ( NLIN - DIMS1( 2 ) ) / 2

*  Get a work array, and an array in which to store the Fourier
*  transform of the PSF.
      CALL PSX_CALLOC( N, '_REAL', IP2, STATUS )
      CALL PSX_CALLOC( N, '_REAL', IP3, STATUS )

*  Store the FFT of the PSF in the internal array <3>. Internal array
*  <2> is used as work space.
      CALL KPS1_WIEFP( DIMS2( 1 ), DIMS2( 2 ), %VAL( CNF_PVAL( IPN2 ) ),
     :                 NPIX, NLIN,
     :                 XCEN - SLBND2( 1 ) + 1, YCEN - SLBND2( 2 ) + 1,
     :                 %VAL( CNF_PVAL( IP3 ) ), %VAL( CNF_PVAL( IP2 ) ),
     :                 STATUS )

*  Release the NDF holding the PSF array.
      CALL NDF_ANNUL( INDF2, STATUS )

*  Report the array sizes.
*  =======================
*  Tell the user the size of the margins and the size of the internal
*  data files.
      CALL MSG_SETI( 'XMARG', XMARG )
      CALL MSG_OUTIF( MSG__NORM, 'WIENER_MSG1',
     :  '  X axis margin is ^XMARG pixels wide.', STATUS )
      CALL MSG_SETI( 'YMARG', YMARG )
      CALL MSG_OUTIF( MSG__NORM, 'WIENER_MSG2',
     :  '  Y axis margin is ^YMARG lines wide.', STATUS )
      CALL MSG_SETI( 'XSIZE', NPIX )
      CALL MSG_SETI( 'YSIZE', NLIN )
      CALL MSG_OUTIF( MSG__NORM, 'WIENER_MSG3',
     :  '  Internal file size is ^XSIZE by ^YSIZE.', STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Add margins to the input data array.
*  ====================================
*
*  Map the DATA array of the input NDF.
      CALL KPG1_MAP( INDF1, 'Data', '_REAL', 'READ', IPN1, NEL1,
     :              STATUS )

*  Get space to hold the internal version of the data array and copy
*  the input data array to the centre of the internal file, allowing
*  for the calculated margin.  The margins are filled by replicating
*  the edge pixels.  The mean observed data value is returned together
*  with the mean squared observed data value.  A flag is returned
*  indicating if any bad values were found.
      CALL PSX_CALLOC( N, '_REAL', IP4, STATUS )
      CALL KPS1_WIECP( XMARG, YMARG, DIMS1( 1 ), DIMS1( 2 ),
     :                 %VAL( CNF_PVAL( IPN1 ) ), NPIX, NLIN,
     :                 %VAL( CNF_PVAL( IP4 ) ), MEANO,
     :                 MEANO2, BAD1, STATUS )

*  Abort if there is no good data in the input array.
      IF ( MEANO .EQ. VAL__BADR .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'WIENER_ERR2', 'All values are bad in the '/
     :                 /'input array.', STATUS )
         GO TO 999
      END IF

*  Obtain the noise and noise power.
*  =================================

*  Find a rough estimate of the standard deviation of the noise in the
*  input array.
      CALL KPS1_WIECS( DIMS1( 1 ), DIMS1( 2 ), %VAL( CNF_PVAL( IPN1 ) ),
     :                 STDEV,
     :                 STATUS )

*  Unmap the DATA array.  This is done to keep the use of virtual
*  memory to a minimum at any one time, in view of the large amount of
*  VM needed for the internal files.
      CALL NDF_UNMAP( INDF1, 'Data', STATUS )

*  For Gaussian noise, the noise power is equal to the variance of the
*  noise.  Get the constant noise power to use, using the squared of
*  the rough estimate of the noise standard deviation obtained earlier
*  as the dynamic default.
      CALL PAR_GDR0R( 'PNOISE', STDEV**2, 0.0, VAL__MAXR, .TRUE., PN,
     :                STATUS )

*  Warn the user.
      CALL MSG_SETR( 'PN', PN )
      CALL MSG_OUTIF( MSG__NORM, 'WIENER_MSG4',
     :  '  Using a noise power of ^PN.', STATUS )

*  Obtain the model image and power.
*  =================================

*  Obtain space for the internal file which holds the model image.
      CALL PSX_CALLOC( N, '_REAL', IP6, STATUS )

*  Abort if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an NDF holding the model image to use.
      CALL KPG1_GTNDF( 'MODEL', NDIM, .FALSE., 'READ', INDF3, SDIM3,
     :                 SLBND3, SUBND3, STATUS )

*  If a null value was given, annul the error, and get a constant model
*  power.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Get the model power to use.  Use the mean power in the input image
*  as a default.  Constrain it to be larger than zero.
         CALL PAR_GDR0R( 'PMODEL', MEANO2, 10.0 * VAL__SMLR, VAL__MAXR,
     :                   .TRUE., PG, STATUS )

*  Warn the user.
         CALL MSG_SETR( 'PG', PG )
         CALL MSG_OUTIF( MSG__NORM, 'WIENER_MSG5',
     :     '  Using a constant model power of ^PG', STATUS )

*  Fill internal file 6 with the constant model-power value.
         CALL KPG1_FILLR( PG, N, %VAL( CNF_PVAL( IP6 ) ), STATUS )

*  If an NDF model was supplied, get its bounds.
      ELSE
         CALL NDF_BOUND( INDF3, NDIM, LBND, UBND, NDIMS, STATUS )

*  Get a section of the model image which matches the bounds of the
*  input data image, and map it.
         LBND( SDIM3( 1 ) ) = SLBND1( 1 )
         UBND( SDIM3( 1 ) ) = SUBND1( 1 )
         LBND( SDIM3( 2 ) ) = SLBND1( 2 )
         UBND( SDIM3( 2 ) ) = SUBND1( 2 )

         CALL NDF_SECT( INDF3, NDIM, LBND, UBND, INDF3S, STATUS )
         CALL KPG1_MAP( INDF3S, 'DATA', '_REAL', 'READ', IPN3S, NEL3S,
     :                 STATUS )

*  Copy it to the centre of internal file 6 . The margins are filled by
*  replicating the edge values.  The mean model value and the mean-
*  squared model value are returned, together with a flag indicating if
*  any bad values were found.
         CALL KPS1_WIECP( XMARG, YMARG, DIMS1( 1 ), DIMS1( 2 ),
     :                    %VAL( CNF_PVAL( IPN3S ) ), NPIX, NLIN,
     :                    %VAL( CNF_PVAL( IP6 ) ),
     :                    MEANM, MEANM2, BAD3, STATUS )

*  Abort if there is no good data in the model array.
         IF ( MEANM .EQ. VAL__BADR .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'WIENER_ERR3', 'All values are bad in the '/
     :                    /'model image.', STATUS )
            GO TO 999
         END IF

*  See if the model image was created by a previous run of WIENER.
*  This is assumed to be so if there is a WIENER extension in the NDF.
*  If the model was created by a previous run of WIENER then assume
*  that the noise power has already been removed.
         CALL NDF_XSTAT( INDF3, 'WIENER', QUIET, STATUS )

*  Release the model NDF.
         CALL NDF_ANNUL( INDF3, STATUS )

*  Replace the model image in file 6 with the power image in frequency
*  space (this is taken to be equal to the square of the modulus of the
*  complex Fourier transform value).  Any bad values are replaced by the
*  mean model value before the operation begins.  File 2 is used as work
*  space.
         CALL PSX_CALLOC( N, '_REAL', IP5, STATUS )
         CALL KPS1_WIEPW( N, NPIX, NLIN, MEANM, BAD3,
     :                    %VAL( CNF_PVAL( IP6 ) ),
     :                    %VAL( CNF_PVAL( IP2 ) ),
     :                    %VAL( CNF_PVAL( IP5 ) ), STATUS )
         CALL PSX_FREE( IP5, STATUS )

*  If a constant model power was given, assume that the noise power has
*  not yet been removed.
         QUIET = .FALSE.

      END IF

*  Obtain remaining parameters.
*  ============================

*  See if the model was noiseless ("quiet"), using a dynamic default
*  established above.
      CALL MSG_BLANK( STATUS )
      CALL PAR_DEF0L( 'QUIET', QUIET, STATUS )

      IF( STATUS .NE. SAI__OK ) GO TO 999

      CALL PAR_GET0L( 'QUIET', QUIET, STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Get the minimum fractional weight of good pixels required to produce
*  a good output pixel.  Ensure it is not too small.
      CALL PAR_GDR0R( 'WLIM', 0.01, 0.0, 1.0, .TRUE., WLIM, STATUS )
      WLIM = MAX( WLIM, 1.0E-5 )

*  Construct and apply the filter.
*  ===============================

*  Construct the filter.  Its FFT is returned in file 3.
      CALL KPS1_WIEFL( N, NPIX, NLIN, PN, QUIET,
     :                 %VAL( CNF_PVAL( IP6 ) ),
     :                 %VAL( CNF_PVAL( IP3 ) ), %VAL( CNF_PVAL( IP2 ) ),
     :                 STATUS )

*  Apply the filter to the supplied input image.  The filtered image is
*  returned in file 4.
      CALL KPS1_WIEAP( BAD1, WLIM, N, NPIX, NLIN,
     :                 %VAL( CNF_PVAL( IP3 ) ),
     :                 %VAL( CNF_PVAL( IP4 ) ), %VAL( CNF_PVAL( IP2 ) ),
     :                 %VAL( CNF_PVAL( IP6 ) ), STATUS )

*  Create the output NDF containing the filter image.
*  ==================================================

*  Create the output NDF, based upon the input NDF, and map its DATA
*  array.
      CALL LPG_PROP( INDF1, 'WCS,Axis,Quality,Units', 'OUT', INDF4,
     :               STATUS )
      CALL KPG1_MAP( INDF4, 'Data', '_REAL', 'WRITE', IPN4, NEL4,
     :              STATUS )

*  Copy the reconstructed array to the output DATA array.
      CALL KPS1_WIEOU( NPIX, NLIN, %VAL( CNF_PVAL( IP4 ) ),
     :                 XMARG, YMARG,
     :                 DIMS1( 1 ), DIMS1( 2 ), %VAL( CNF_PVAL( IPN4 ) ),
     :                 BAD4,
     :                 STATUS )

*  Create a WIENER extension in the output NDF, and add a logical
*  component called QUIET with the value TRUE.  This indicates that
*  the image is noiseless.
      CALL NDF_XNEW( INDF4, 'WIENER', 'WIENER', 0, 0, XLOC, STATUS )
      CALL NDF_XPT0L( .TRUE., INDF4, 'WIENER', 'QUIET', STATUS )
      CALL DAT_ANNUL( XLOC, STATUS )

*  Set the bad-pixel flag in the output NDF.
      CALL NDF_SBAD( BAD4, INDF4, 'DATA', STATUS )

*  Obtain a new title for the output NDF, with the default value
*  being the input array title.
      CALL KPG1_CCPRO( 'TITLE', 'Title', INDF1, INDF4, STATUS )

*  Closedown sequence.
*  ===================

 999  CONTINUE

*  Free the dynamic work arrays.
      CALL PSX_FREE( IP2, STATUS )
      CALL PSX_FREE( IP3, STATUS )
      CALL PSX_FREE( IP4, STATUS )
      CALL PSX_FREE( IP6, STATUS )

*  If an error occurred, delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF4, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error has occurred, give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WIENER_ERR', 'Error deconvolving an array ' /
     :                 /'using a Wiener filter.', STATUS )
      END IF

      END
