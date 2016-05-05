      SUBROUTINE LUCY( STATUS )
*+
*  Name:
*     LUCY

*  Purpose:
*     Performs a Richardson-Lucy deconvolution of a 1- or 2-dimensional
*     array.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LUCY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application deconvolves the supplied 1- or 2-dimensional
*     array using the Richardson-Lucy (R-L) algorithm.  It takes an
*     array holding observed data and another holding a Point-Spread
*     Function (PSF) as input and produces an output array with higher
*     resolution.  The algorithm is iterative, each iteration producing
*     a new estimate of the restored array which (usually) fits the
*     observed data more closely than the previous estimate (in the
*     sense that simulated data generated from the restored array is
*     closer to the observed data).  The closeness of the fit is
*     indicated after each iteration by a normalised chi-squared value
*     (i.e. the chi-squared per pixel).  The algorithm terminates when
*     the normalised chi-squared given by parameter AIM is reached, or
*     the maximum number of iterations given by parameter NITER have
*     been performed.  The current estimate of the restored array is
*     then written to the output NDF.
*
*     Before the first iteration, the restored array is initialised
*     either to the array given by parameter START, or, if no array is
*     given, to the difference between the mean value in the input data
*     array and the mean value in the background (specified by
*     parameters BACK and BACKVAL).  Simulated data is then created from
*     this trial array by smoothing it with the supplied PSF, and then
*     adding the background on.  The chi-squared value describing the
*     deviation of this simulated data from the observed data is then
*     found and displayed.  If the required chi-squared is not reached
*     by this simulated data, the first iteration commences, which
*     consists of creating a new version of the restored array and then
*     creating new simulated data from this new restored array (the
*     corresponding chi-squared value is displayed).  Repeated
*     iterations are performed until the required chi-squared is
*     reached, or the iteration limit is reached.  The new version of
*     the restored array is created as follows.
*
*        1 - A correction factor is found for each data value.  This is
*        the ratio of the observed data value to the simulated data
*        value.  An option exists to use the Snyder modification as
*        used by the LUCY program in the STSDAS package within IRAF.
*        With this option selected, the variance of the observed data
*        value is added to both the numerator and the denominator when
*        finding the correction factors.
*
*        2 - These correction factors are mapped into an array by
*        smoothing the array of correction factors with the transposed
*        PSF.
*
*        3 - The current version of the restored array is multiplied by
*        this correction factor array to produce the new version of the
*        restored array.
*
*     For further background to the algorithm, see L. B. Lucy,
*     Astron.J. 1974, Vol 79, No. 6.

*  Usage:
*     lucy in psf out [aim]

*  ADAM Parameters:
*     AIM = _REAL (Read)
*        The chi-squared value at which the algorithm should terminate.
*        Smaller values of AIM will result in higher apparent resolution
*        in the output array but will also cause noise in the observed
*        data to be interpreted as real structure.  Small values will
*        require larger number of iterations, so NITER may need to be
*        given a larger value.  Very-small values may be completely
*        un-achievable, indicated by chi-squared not decreasing (or
*        sometimes increasing) between iterations.  Larger values will
*        result in smoother output arrays with less noise.  [1.0]
*     BACK = NDF (Read)
*        An NDF holding the background value for each observed data
*        value.  If a null value is supplied, a constant background
*        value given by parameter BACKVAL is used. [!]
*     BACKVAL = _REAL (Read)
*        The constant background value to use if BACK is given a null
*        value. [0.0]
*     CHIFAC = _REAL (Read)
*        The normalised chi-squared value which is used to determine if
*        the algorithm should terminate is the mean of the following
*        expression (the mean is taken over the entire input array,
*        the margins used to pad the input array are excluded):
*
*           ( D - S )**2 / ( CHIFAC*S - V )
*
*        where D is the observed data value, S is the simulated data
*        value based on the current version of the restored array, V is
*        the variance of the error associated with D, and CHIFAC is the
*        value of parameter CHIFAC.  Using 0 for CHIFAC results in the
*        standard expression for chi-squared.  However, the algorithm
*        sometimes has difficulty fitting bright features and so may
*        not reach the required normalised chi-squared value.  Setting
*        CHIFAC to 1 (as is done by the LUCY program in the STSDAS
*        package within IRAF) causes larger data values to be given
*        less weight in the chi-squared calculation, and so encourages
*        lower chi-squared values. [1.0]
*     IN= NDF (Read)
*        The input NDF containing the observed data.
*     NITER = _INTEGER (Read)
*        The maximum number of iterations to perform. [50]
*     OUT = NDF (Write)
*        The restored output array.  The background specified by
*        parameters BACK and BACKVAL will have been removed from this
*        array.  The output is the same size as the input.  There is no
*        VARIANCE component in the output, but any QUALITY values are
*        propagated from the input to the output.
*     PSF = NDF (Read)
*        An NDF holding an estimate of the Point-Spread Function (PSF)
*        of the input array.  This could, for instance, be produced
*        using the KAPPA application `PSF'. There should be no bad
*        pixels in the PSF otherwise an error will be reported.  The
*        PSF can be centred anywhere within the array, but the location
*        of the centre must be specified using parameters XCENTRE and
*        YCENTRE.  The PSF is assumed to have the value zero outside
*        the supplied NDF.
*     SIGMA = _REAL (Read)
*        The standard deviation of the noise in the observed data.
*        This is only used if parameter VARIANCE is given the value
*        FALSE. If a null (!) value is supplied, the value used is
*        an estimate of the noise based on the difference between
*        adjacent pixel values in the observed data. [!]
*     START = NDF (Read)
*        An NDF containing an initial guess at the restored array.
*        This could, for instance, be the output from a previous run of
*        LUCY, in which case the deconvolution would continue from the
*        point it had previously reached.  If a null value is given,
*        then the restored array is initialised to a constant value
*        equal to the difference between the mean observed data value
*        and the mean background value. [!]
*     SNYDER = _LOGICAL (Read)
*        If TRUE then the variance of the observed data sample is added
*        to both the numerator and denominator when evaluating the
*        correction factor for each data sample.  This is the modified
*        form of the R-L algorithm used by the LUCY program in the
*        STSDAS package within IRAF. [TRUE]
*     THRESH = _REAL (Read)
*        The fraction of the PSF peak amplitude at which the extents of
*        the PSF are determined.  These extents are used to determine
*        the size of the margins used to pad the supplied input array.
*        Lower values of THRESH will result in larger margins being
*        used.  THRESH must be positive and less than 0.5.  [0.0625]
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null (!) value means using the
*        title of the input NDF. [!]
*     VARIANCE = _LOGICAL (Read)
*        If TRUE, then the variance of each input data sample will be
*        obtained from the VARIANCE component of the input NDF.  An
*        error is reported if this option is selected and the NDF has
*        no VARIANCE component.  If FALSE, then a constant variance
*        equal to the square of the value given for parameter SIGMA is
*        used for all data samples.  If a null (!) value is supplied,
*        the value used is TRUE if the input NDF has a VARIANCE
*        component, and FALSE otherwise. [!]
*     WLIM = _REAL (Read)
*        If the input array contains bad pixels, then this parameter
*        may be used to determine the number of good data values which
*        must contribute to an output pixel before a valid value is
*        stored in the restored array.  It can be used, for example, to
*        prevent output pixels from being generated in regions where
*        there are relatively few good data values to contribute to the
*        restored result. It can also be used to `fill in' small areas
*        (i.e. smaller than the PSF) of bad pixels.
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
*        good.  Values less than 0.5 will tend to reduce the number of
*        bad pixels, whereas values larger than 0.5 will tend to
*        increase the number of bad pixels.
*
*        This threshold is applied each time a smoothing operation is
*        performed.  Many smoothing operations are typically performed
*        in a run of LUCY, and if WLIM is larger than 0.5 the effects
*        of bad pixels will propagate further through the array at each
*        iteration.  After several iterations this could result in there
*        being no good data left.  An error is reported if this
*        happens. [0.001]
*     XCENTRE = _INTEGER (Read)
*        The x pixel index of the centre of the PSF within the supplied
*        PSF array.  If a null (!) value is supplied, the value used is
*        the middle pixel (rounded down if there are an even number of
*        pixels per line). [!]
*     YCENTRE = _INTEGER (Read)
*        The y pixel index of the centre of the PSF within the supplied
*        PSF array. If a null (!) value is supplied, the value used is
*        the middle line (rounded down if there are an even number of
*        lines). [!]

*  Examples:
*     lucy m51 star m51_hires
*        This example deconvolves the array in the NDF called m51,
*        putting the resulting array in the NDF called m51_hires.  The
*        PSF is defined by the array in NDF star (the centre of the
*        PSF is assumed to be at the central pixel).  The deconvolution
*        terminates when a normalised chi-squared value of 1.0 is
*        reached.
*     lucy m51 star m51_hires 0.5 niter=60
*        This example performs the same function as the previous
*        example, except that the deconvolution terminates when a
*        normalised chi-squared value of 0.5 is reached, giving higher
*        apparent resolution at the expense of extra spurious
*        noise-based structure.  The maximum number of iterations is
*        increased to 60 to give the algorithm greater opportunity to
*        reach the reduced chi-squared value.
*     lucy m51 star m51_hires2 0.1 start=m51_hires
*        This example continues the deconvolution started by the
*        previous example in order to achieve a normalised chi-squared
*        of 0.1.  The output array from the previous example is used to
*        initialise the restored array.

*  Notes:
*     - The convolutions required by the R-L algorithm are performed by
*     the multiplication of Fourier transforms.  The supplied input
*     array is extended by a margin along each edge to avoid problems
*     of wrap-around between opposite edges of the array.  The width of
*     this margin is about equal to the width of the significant part
*     of the PSF (as determined by parameter THRESH).  The application
*     displays the width of these margins.  The margins are filled by
*     replicating the edge pixels from the supplied input NDFs.
*
*     - The R-L algorithm works best for arrays which have zero
*     background.  Non-zero backgrounds cause dark rings to appear
*     around bright, compact sources.  To avoid this a background array
*     should be created before running LUCY and assigned to the
*     parameter BACK.  The SEGMENT and SURFIT applications within KAPPA
*     can be used to create such a background array.

*  Related Applications:
*     KAPPA: FOURIER, MEM2D, WIENER.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-MAR-1995 (DSB):
*        Original version, based on MEM2D and Rhys Morris's LUCY.
*     1995 April 6 (MJC):
*        Used conditional message reporting and modern-style variable
*        declarations.  Added Related Applications.  Corrected typo's
*        and made stylistic changes.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     5-MAY-2016 (DSB):
*        Variable MEANB was being used uninitialised if a null value was
*        supplied for parameter BACK. Fixed by putting the value of
*        parameter BACKVAL into variable MEANB rather than into variable
*        BCKVAL.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'MSG_PAR'          ! Message-system constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality required
      PARAMETER( NDIM = 2 )

*  Local Variables:
      REAL AIM                   ! Required normalised chi-squared
      LOGICAL BAD2               ! Are any bad pixels present in NDF2?
      LOGICAL BAD5               ! Are any bad pixels present in NDF5?
      REAL CHIFAC                ! Simulated data factor in chi-squared
      INTEGER DIMS1( NDIM )      ! Dimensions sizes in NDF1
      INTEGER DIMS2( NDIM )      ! Dimensions sizes in NDF1
      INTEGER INDF1              ! NDF id. for input data array
      INTEGER INDF2              ! NDF id. for PSF array
      INTEGER INDF3              ! NDF id. for background data array
      INTEGER INDF3S             ! NDF id. for section of background
      INTEGER INDF4              ! NDF id. for starting array
      INTEGER INDF4S             ! NDF id. for section of starting array
      INTEGER INDF5              ! NDF id. for output array
      INTEGER IP1                ! Pointer to internal file 1
      INTEGER IP2                ! Pointer to internal file 2
      INTEGER IP3                ! Pointer to internal file 3
      INTEGER IP4                ! Pointer to internal file 4
      INTEGER IP5                ! Pointer to internal file 5
      INTEGER IP6                ! Pointer to internal file 6
      INTEGER IP7                ! Pointer to internal file 7
      INTEGER IP8                ! Pointer to internal file 8
      INTEGER IPN1               ! Pointer to input data array
      INTEGER IPN2               ! Pointer to PSF array
      INTEGER IPN3S              ! Pointer to section of background
      INTEGER IPN4S              ! Pointer to section of starting array
      INTEGER IPN5               ! Pointer to output array
      INTEGER IPW0               ! Pointer to work array
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of all axes in an NDF
      REAL MEANB                 ! Mean value in background array
      REAL MEANI                 ! Mean value in starting array
      REAL MEANO                 ! Mean value in observed data array
      REAL MEANV                 ! Mean value in variance array
      INTEGER N                  ! No. of elements in an internal file
      INTEGER NDIMS              ! Total no. of dimensions in an NDF
      INTEGER NEL1               ! No. of elements in NDF1
      INTEGER NEL2               ! No. of elements in NDF2
      INTEGER NEL3S              ! No. of elements in NDF3S
      INTEGER NEL4S              ! No. of elements in NDF4S
      INTEGER NEL5               ! No. of elements in NDF5
      INTEGER NITER              ! Max. no. of iterations to perform
      INTEGER NLIN               ! No. of rows in each internal file
      INTEGER NPIX               ! No. of columns in each internal file
      INTEGER PSFXSZ             ! Approx. size of the PSF along X axis
      INTEGER PSFYSZ             ! Approx. size of the PSF along Y axis
      INTEGER SDIM1( NDIM )      ! Indices of significant axes in NDF1
      INTEGER SDIM2( NDIM )      ! Indices of significant axes in NDF2
      INTEGER SDIM3( NDIM )      ! Indices of significant axes in NDF3
      INTEGER SDIM4( NDIM )      ! Indices of significant axes in NDF4
      INTEGER SLBND1( NDIM )     ! Low bounds of INDF1 significant axes
      INTEGER SLBND2( NDIM )     ! Low bounds of INDF2 significant axes
      INTEGER SLBND3( NDIM )     ! Low bounds of INDF3 significant axes
      INTEGER SLBND4( NDIM )     ! Low bounds of INDF4 significant axes
      LOGICAL SNYDER             ! Use Snyder's modifed R-L algorithm?
      REAL STDEV                 ! Approximate noise level in input
      INTEGER SUBND1( NDIM )     ! High bounds of INDF1 significant axes
      INTEGER SUBND2( NDIM )     ! High bounds of INDF2 significant axes
      INTEGER SUBND3( NDIM )     ! High bounds of INDF3 significant axes
      INTEGER SUBND4( NDIM )     ! High bounds of INDF4 significant axes
      REAL THRESH                ! Truncation threshold of PSF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of all axes in an NDF
      LOGICAL VAR1               ! Does NDF1 have a VARIANCE component?
      LOGICAL VARN               ! Use variances from the input NDF?
      INTEGER WDIMS( NDIM )      ! Dimensions of work array.
      REAL WLIM                  ! Min. weight of good pixels required
      INTEGER XCEN               ! X pixel index of PSF centre.
      INTEGER XMARG              ! Width of margins on x axes
      INTEGER YCEN               ! Y pixel index of PSF centre.
      INTEGER YMARG              ! Width of margins on y axes

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

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
         CALL ERR_REP('LUCY_ERR1', 'PSF ''^NDF'' contains some bad '/
     :                /'pixels.', STATUS )
         GO TO 999
      END IF

*  Find the dimensions of the PSF.
      DIMS2( 1 ) = SUBND2( 1 ) - SLBND2( 1 ) + 1
      DIMS2( 2 ) = SUBND2( 2 ) - SLBND2( 2 ) + 1

*  Get the pixel indices of the centre of the PSF within the array.  The
*  default is the centre of the PSF array.
      CALL PAR_GDR0I( 'XCENTRE', SLBND2( 1 ) + DIMS2( 1 ) / 2,
     :                SLBND2( 1 ), SUBND2( 1 ), .TRUE., XCEN, STATUS )
      CALL PAR_GDR0I( 'YCENTRE', SLBND2( 2 ) + DIMS2( 2 ) / 2,
     :                SLBND2( 2 ), SUBND2( 2 ), .TRUE., YCEN, STATUS )

*  Get the truncation threshold as fraction of the peak amplitude of
*  the PSF.
      CALL PAR_GDR0R( 'THRESH', 0.0625, VAL__SMLR, 0.5, .FALSE., THRESH,
     :                STATUS )

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

*  Add this margin onto the input array dimensions to get the dimensions
*  of the internal arrays.
      XMARG = PSFXSZ
      YMARG = PSFYSZ
      NPIX = DIMS1( 1 ) + 2 * XMARG
      NLIN = DIMS1( 2 ) + 2 * YMARG

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
*  transfrom of the PSF.
      CALL PSX_CALLOC( N, '_REAL', IP2, STATUS )
      CALL PSX_CALLOC( N, '_REAL', IP3, STATUS )

*  Store the FFT of the PSF in the internal array <3>.  Internal array
*  <2> is used as work space.
      CALL KPS1_LUCFP( DIMS2( 1 ), DIMS2( 2 ), %VAL( CNF_PVAL( IPN2 ) ),
     :                 NPIX, NLIN,
     :                 XCEN - SLBND2( 1 ) + 1, YCEN - SLBND2( 2 ) + 1,
     :                 %VAL( CNF_PVAL( IP3 ) ), %VAL( CNF_PVAL( IP2 ) ),
     :                 STATUS )

*  Release the NDF holding the PSF array.
      CALL NDF_ANNUL( INDF2, STATUS )

*  Tell the user the size of the margins and the size of the internal
*  data files.
      CALL MSG_SETI( 'XMARG', XMARG )
      CALL MSG_OUTIF( MSG__NORM, 'LUCY_MSG1', '  X-axis margin is '/
     :                /'^XMARG pixels wide.', STATUS )
      CALL MSG_SETI( 'YMARG', YMARG )
      CALL MSG_OUTIF( MSG__NORM, 'LUCY_MSG2', '  Y-axis margin is '/
     :                /'^YMARG lines wide.', STATUS )
      CALL MSG_SETI( 'XSIZE', NPIX )
      CALL MSG_SETI( 'YSIZE', NLIN )
      CALL MSG_OUTIF( MSG__NORM, 'LUCY_MSG3', '  Internal file size '/
     :                /'is ^XSIZE by ^YSIZE.', STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Map the DATA array of the input NDF.
      CALL KPG1_MAP( INDF1, 'DATA', '_REAL', 'READ', IPN1, NEL1,
     :              STATUS )

*  Get space to hold the internal version of the data array and copy
*  the input data array to the centre of the internal file, allowing
*  for the calculated margin.  The margins are filled by replicating the
*  edge pixels.  The mean observed data value is returned.
      CALL PSX_CALLOC( N, '_REAL', IP4, STATUS )
      CALL KPS1_LUCCP( XMARG, YMARG, DIMS1( 1 ), DIMS1( 2 ),
     :                 %VAL( CNF_PVAL( IPN1 ) ), NPIX, NLIN,
     :                 %VAL( CNF_PVAL( IP4 ) ), MEANO,
     :                 STATUS )

*  Abort if there are no good data in the input array.
      IF ( MEANO .EQ. VAL__BADR .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'LUCY_ERR2', 'All values are bad in the '/
     :                 /'input array.', STATUS )
         GO TO 999
      END IF

*  Find a rough estimate of the noise in the input array.
      CALL KPS1_LUCCS( DIMS1( 1 ), DIMS1( 2 ), %VAL( CNF_PVAL( IPN1 ) ),
     :                 STDEV,
     :                 STATUS )

*  Unmap the DATA array.  This is done to keep the use of virtual memory
*  to a minimum at any one time, in view of the large amount of VM
*  needed for the internal files.
      CALL NDF_UNMAP( INDF1, 'DATA', STATUS )

*  Get space to hold the variance of each data value.
      CALL PSX_CALLOC( N, '_REAL', IP8, STATUS )

*  See if the NDF has a defined VARIANCE component.
      CALL NDF_STATE( INDF1, 'VARIANCE', VAR1, STATUS )

*  See from where the observed data variances are to be obtained.  If
*  the input NDF has a VARIANCE component, use it by default.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_DEF0L( 'VARIANCE', VAR1, STATUS )
         CALL PAR_GET0L( 'VARIANCE', VARN, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            VARN = VAR1
         END IF
      END IF

*  If variances are to be obtained from the NDF...
      IF ( VARN ) THEN

*  Report an error if there is no VARIANCE component in the NDF.
         IF ( .NOT. VAR1 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'LUCY_ERR3', 'The NDF supplied for '/
     :        /'parameter %IN has no VARIANCE component.', STATUS )
            GO TO 999
         END IF

*  Warn the user.
         CALL MSG_OUTIF( MSG__NORM, 'LUCY_MSG4', '  Using the noise '/
     :                   /'values in the VARIANCE component of the '/
     :                   /'input NDF.', STATUS )

*  Map the VARIANCE component of the input NDF.
         CALL KPG1_MAP( INDF1, 'VARIANCE', '_REAL', 'READ', IPN1, NEL1,
     :                 STATUS )

*  Copy the input variance array to the centre of the internal file,
*  allowing for the calculated margin.  The margins are filled by
*  replicating the edge values.  The mean variance value is returned.
         CALL KPS1_LUCCP( XMARG, YMARG, DIMS1( 1 ), DIMS1( 2 ),
     :                    %VAL( CNF_PVAL( IPN1 ) ), NPIX, NLIN,
     :                    %VAL( CNF_PVAL( IP8 ) ),
     :                    MEANV, STATUS )

*  Abort if there are no good data in the variance array.
         IF ( MEANV .EQ. VAL__BADR .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'LUCY_ERR4', 'All values are bad in the '/
     :                    /'input NDF VARIANCE array.', STATUS )
            GO TO 999
         END IF

*  Unmap the variance array.
         CALL NDF_UNMAP( INDF1, 'VARIANCE', STATUS )

*  Otherwise...
      ELSE

*  Get the constant noise value to use, using the rough estimate
*  obtained earlier as the dynamic default.  Constrain it to be larger
*  than zero.
         CALL PAR_GDR0R( 'SIGMA', STDEV, 10.0 * VAL__SMLR, VAL__MAXR,
     :                   .TRUE., STDEV, STATUS )

*  Warn the user.
         CALL MSG_SETR( 'SIGMA', STDEV )
         CALL MSG_OUTIF( MSG__NORM, 'LUCY_MSG5', '  Using a constant '/
     :                   /'noise value of ^SIGMA.', STATUS )

*  Fill internal file 8 with the variance value.
         CALL KPG1_FILLR( STDEV * STDEV, N, %VAL( CNF_PVAL( IP8 ) ),
     :                    STATUS )

      END IF

*  Obtain space for the internal file which holds the background data at
*  every pixel.
      CALL PSX_CALLOC( N, '_REAL', IP6, STATUS )

*  Abort if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an NDF holding the background array to use.
      CALL KPG1_GTNDF( 'BACK', NDIM, .FALSE., 'READ', INDF3, SDIM3,
     :                 SLBND3, SUBND3, STATUS )

*  If a null value was given, annul the error, and get a constant
*  background value.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Get the background value to use.
         CALL PAR_GET0R( 'BACKVAL', MEANB, STATUS )

*  Fill internal file 6 with the supplied constant-background value.
         CALL KPG1_FILLR( MEANB, N, %VAL( CNF_PVAL( IP6 ) ), STATUS )

*  If an NDF background was supplied, get its bounds.
      ELSE
         CALL NDF_BOUND( INDF3, NDIM, LBND, UBND, NDIMS, STATUS )

*  Get a section of the background array which matches the bounds of the
*  input data array, and map it.
         LBND( SDIM3( 1 ) ) = SLBND1( 1 )
         UBND( SDIM3( 1 ) ) = SUBND1( 1 )
         LBND( SDIM3( 2 ) ) = SLBND1( 2 )
         UBND( SDIM3( 2 ) ) = SUBND1( 2 )

         CALL NDF_SECT( INDF3, NDIM, LBND, UBND, INDF3S, STATUS )
         CALL KPG1_MAP( INDF3S, 'DATA', '_REAL', 'READ', IPN3S, NEL3S,
     :                 STATUS )

*  Copy it to the centre of internal file 6.  The margins are filled by
*  replicating the edge values.  The mean background value is returned.
         CALL KPS1_LUCCP( XMARG, YMARG, DIMS1( 1 ), DIMS1( 2 ),
     :                    %VAL( CNF_PVAL( IPN3S ) ), NPIX, NLIN,
     :                    %VAL( CNF_PVAL( IP6 ) ),
     :                    MEANB, STATUS )

*  Abort if there is no good data in the background array.
         IF ( MEANB .EQ. VAL__BADR .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'LUCY_ERR5', 'All values are bad in the '/
     :                    /'background array.', STATUS )
            GO TO 999
         END IF

*  Release the background NDF.
         CALL NDF_ANNUL( INDF3, STATUS )

      END IF

*  Obtain space for the internal file which holds the current
*  reconstruction.
      CALL PSX_CALLOC( N, '_REAL', IP1, STATUS )

*  Abort if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an NDF holding the initial guess for the reconstructed
*  array.
      CALL KPG1_GTNDF( 'START', NDIM, .FALSE., 'READ', INDF4, SDIM4,
     :                 SLBND4, SUBND4, STATUS )

*  If a null value was given, annul the error, and store the difference
*  between the mean observed data value and the mean background value
*  in internal file 1.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL KPG1_FILLR( MEANO - MEANB, N, %VAL( CNF_PVAL( IP1 ) ),
     :                    STATUS )

*  If a starting NDF was supplied, get its bounds.
      ELSE
         CALL NDF_BOUND( INDF4, NDIM, LBND, UBND, NDIMS, STATUS )

*  Get a section of the array which matches the bounds of the input
*  data array, and map it.
         LBND( SDIM4( 1 ) ) = SLBND1( 1 )
         UBND( SDIM4( 1 ) ) = SUBND1( 1 )
         LBND( SDIM4( 2 ) ) = SLBND1( 2 )
         UBND( SDIM4( 2 ) ) = SUBND1( 2 )

         CALL NDF_SECT( INDF4, NDIM, LBND, UBND, INDF4S, STATUS )
         CALL KPG1_MAP( INDF4S, 'DATA', '_REAL', 'READ', IPN4S, NEL4S,
     :                 STATUS )

*  Copy it to the centre of internal file 1.  The margins are filled by
*  replication the edge values.  The mean value in the supplied array
*  is returned.
         CALL KPS1_LUCCP( XMARG, YMARG, DIMS1( 1 ), DIMS1( 2 ),
     :                    %VAL( CNF_PVAL( IPN4S ) ), NPIX, NLIN,
     :                    %VAL( CNF_PVAL( IP1 ) ),
     :                    MEANI, STATUS )

*  Abort if there are no good data in the input array.
         IF ( MEANI .EQ. VAL__BADR .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'LUCY_ERR6', 'All values are bad in the '/
     :                    /'starting array DATA array.', STATUS )
            GO TO 999
         END IF

*  Release the starting NDF.
         CALL NDF_ANNUL( INDF4, STATUS )

      END IF

*  Obtain space for two more internal files.
      CALL PSX_CALLOC( N, '_REAL', IP5, STATUS )
      CALL PSX_CALLOC( N, '_REAL', IP7, STATUS )

*  Get the maximum number of iterations to perform.
      CALL PAR_GDR0I( 'NITER', 30, 0, VAL__MAXI, .TRUE., NITER, STATUS )

*  Get the target normalised chi-squared.
      CALL PAR_GDR0R( 'AIM', 1.0, 0.0, VAL__MAXR, .TRUE., AIM, STATUS )

*  Get the co-efficient for the simulated data in the denominator of the
*  chi-squared sum.
      CALL PAR_GDR0R( 'CHIFAC', 1.0, 0.0, VAL__MAXR, .TRUE., CHIFAC,
     :                 STATUS )

*  See if the Snyder modification is to be used.
      CALL PAR_GET0L( 'SNYDER', SNYDER, STATUS )

*  Get the minimum fractional weight of good pixels required to produce
*  a good output pixel.  Ensure it is not too small.
      CALL PAR_GDR0R( 'WLIM', 0.01, 0.0, 1.0, .TRUE., WLIM, STATUS )
      WLIM = MAX( WLIM, 1.0E-5 )

*  Do the deconvolution.
      CALL KPS1_LUCY( N, NPIX, NLIN, XMARG, YMARG, AIM, NITER, CHIFAC,
     :                WLIM, SNYDER, %VAL( CNF_PVAL( IP3 ) ),
     :                %VAL( CNF_PVAL( IP4 ) ),
     :                %VAL( CNF_PVAL( IP6 ) ), %VAL( CNF_PVAL( IP8 ) ),
     :                %VAL( CNF_PVAL( IP1 ) ),
     :                %VAL( CNF_PVAL( IP2 ) ), %VAL( CNF_PVAL( IP5 ) ),
     :                %VAL( CNF_PVAL( IP7 ) ), STATUS )

*  Create the output NDF, and map the DATA array.
      CALL LPG_PROP( INDF1, 'WCS,AXIS,QUALITY,UNITS', 'OUT', INDF5,
     :               STATUS )
      CALL KPG1_MAP( INDF5, 'DATA', '_REAL', 'WRITE', IPN5, NEL5,
     :              STATUS )

*  Copy the reconstructed array to the output DATA array.
      CALL KPS1_LUCOU( NPIX, NLIN, %VAL( CNF_PVAL( IP1 ) ),
     :                 XMARG, YMARG,
     :                 DIMS1( 1 ), DIMS1( 2 ), %VAL( CNF_PVAL( IPN5 ) ),
     :                 BAD5,
     :                 STATUS )

*  Set the bad pixel flag in the output NDF.
      CALL NDF_SBAD( BAD5, INDF5, 'DATA', STATUS )

*  Obtain a new title for the output NDF, with the default value
*  being the input array title.
      CALL KPG1_CCPRO( 'TITLE', 'Title', INDF1, INDF5, STATUS )

 999  CONTINUE

*  Free the dynamic work arrays.
      CALL PSX_FREE( IP1, STATUS )
      CALL PSX_FREE( IP2, STATUS )
      CALL PSX_FREE( IP3, STATUS )
      CALL PSX_FREE( IP4, STATUS )
      CALL PSX_FREE( IP5, STATUS )
      CALL PSX_FREE( IP6, STATUS )
      CALL PSX_FREE( IP7, STATUS )
      CALL PSX_FREE( IP8, STATUS )

*  If an error occurred, delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF5, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error has occurred, give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LUCY_ERR7', 'Error deconvolving an array ' /
     :                 /'using the Richardson-Lucy algorithm.', STATUS )
      END IF

      END
