      SUBROUTINE GAUSMOOTH( STATUS )
*+
*  Name:
*     GAUSMOOTH

*  Purpose:
*     Smooths a one- to three-dimensional NDF using a Gaussian filter.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GAUSMOOTH( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application smooths an NDF using a one- or two-dimensional
*     symmetrical Gaussian point spread function (PSF) of specified
*     width, or widths and orientation.  Each output pixel is the
*     PSF-weighted mean of the input pixels within the filter box.
*
*     The NDF may have up to three dimensions.  If it has three
*     dimensions, then the filter is applied in turn to each plane in
*     the cube and the result written to the corresponding plane in the
*     output cube.  The orientation of the smoothing plane can be
*     specified using the AXES parameter.

*  Usage:
*     gausmooth in out fwhm

*  ADAM Parameters:
*     AXES(2) = _INTEGER (Read)
*        This parameter is only accessed if the NDF has exactly three
*        significant pixel axes.  It should be set to the indices of the
*        NDF  pixel axes which span the plane in which smoothing is to
*        be applied.  All pixel planes parallel to the specified plane
*        will be smoothed independently of each other.  The dynamic
*        default comprises the indices of the first two significant
*        axes in the NDF. []
*     BOX() = _INTEGER (Read)
*        The x and y sizes (in pixels) of the rectangular region over
*        which the Gaussian PSF should be applied at each point.  The
*        smoothing PSF will be set to zero outside this rectangle,
*        which should therefore be sufficiently large not to truncate
*        the PSF too early.  A square region is defined should only one
*        size be given.  For a one-dimensional or circular Gaussian a
*        second size is ignored.  Two values are expected when an
*        elliptical PSF is requested (see the description of parameter
*        FWHM).
*
*        The values given will be rounded up to positive odd integers
*        if necessary.  If a null (!) value is supplied, the value used
*        is just sufficient to accommodate the Gaussian PSF out to a
*        radius of 3 standard deviations.  Note that the time taken to
*        perform the smoothing increases in approximate proportion to
*        the value of this parameter for a circular Gaussian, and in
*        proportion to the product of the two box sizes for an
*        elliptical Gaussian. [!]
*     FWHM() = _REAL (Read)
*        This specifies whether a circular or elliptical Gaussian
*        point-spread function is used in smoothing a two-dimensional
*        image.  If one value is given it is the full-width at
*        half-maximum of a one-dimensional or circular Gaussian PSF.
*        (Indeed only one value is permitted for a one-dimensional
*        array.)  If two values are supplied, this parameter becomes the
*        full-width at half-maximum of the major and minor axes of an
*        elliptical Gaussian PSF.  Values between 0.1 and 10000.0 pixels
*        should be given.  Note that unless a non-default value is
*        specified for the BOX parameter, the time taken to perform the
*        smoothing will increase in approximate proportion to the
*        value(s) of FWHM.  The suggested default is the current value.
*     IN = NDF (Read)
*        The input NDF containing the one-, two-, or three-dimensional
*        image to which Gaussian smoothing is to be applied.
*     ORIENT = _REAL (Read)
*        The orientation of the major axis of the elliptical Gaussian
*        PSF, measured in degrees in an anti-clockwise direction from
*        the x axis of the NDF.  ORIENT is not obtained if FWHM has one
*        value, i.e. a circular Gaussian PSF will be used to smooth the
*        image, or the input NDF is one-dimensional.  The suggested
*        default is the current value.
*     OUT = NDF (Write)
*        The output NDF which is to contain the smoothed image.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the input NDF to be used.  [!]
*     WLIM = _DOUBLE (Read)
*        If the input image contains bad pixels, then this parameter
*        may be used to determine the number of good pixels which must
*        be present within the PSF area before a valid output pixel is
*        generated.  It can be used, for example, to prevent output
*        pixels from being generated in regions where good pixels are
*        only present in the wings of the PSF.
*
*        By default, a null (!) value is used for WLIM, which causes
*        the pattern of bad pixels to be propagated from the input
*        image to the output image unchanged. In this case, smoothed
*        output values are only calculated for those pixels which are
*        not bad in the input image.
*
*        If a numerical value is given for WLIM, then it specifies the
*        minimum PSF-weighted fraction of good pixels which must be
*        present in the PSF area (i.e. box) in order to generate a good
*        output pixel.  The maximum value, in the absence of bad
*        pixels, is unity.  If the specified minimum fraction of good
*        input pixels is not present, then a bad output pixel will
*        result, otherwise a smoothed output value will be calculated.
*        The value of this parameter should lie between 1E-6 and 1.0.
*        [!]

*  Examples:
*     gausmooth image1 image2 5.0
*        Smooths the two-dimensional image held in the NDF structure
*        image1 using a symmetrical Gaussian PSF with a full-width at
*        half-maximum of 5 pixels.  The smoothed image is written to
*        image2.  If any pixels in the input image are bad, then the
*        corresponding pixels in the output image will also be bad.
*     gausmooth spectrum1 spectrum2 5.0 box=9
*        Smooths the one-dimensional image held in the NDF structure
*        spectrum1 using a symmetrical Gaussian PSF with a full-width
*        at half-maximum of 5, and is evaluated over a length of 9
*        pixels.  The smoothed image is written to spectrum2.  If any
*        pixels in the input image are bad, then the corresponding
*        pixels in the output image will also be bad.
*     gausmooth in=a out=b fwhm=3.5 box=31
*        Smooths the two-dimensional image held in the NDF structure a,
*        writing the result into the structure b. The Gaussian
*        smoothing PSF has a full-width at half-maximum of 3.5 pixels
*        and is evaluated over a large square of size 31x31 pixels.
*     gausmooth in=a out=b fwhm=[4,3] orient=52.7 box=[29,33]
*        Smooths the two-dimensional image held in the NDF structure a,
*        writing the result into the structure b.  The elliptical
*        Gaussian smoothing PSF has full-width at half-maximum of 4
*        pixels along its major axis and three pixels along its minor
*        axis, and is evaluated over a large rectangle of size 29x33
*        pixels.  The major axis of the PSF is oriented 52.7 degrees
*        anti-clockwise from the x axis of the data array.
*     gausmooth ngc1097 ngc1097s fwhm=7.2 wlim=0.1
*        Smooths the specified image data using a Gaussian PSF with a
*        full-width at half-maximum of 7.2.  An output value is
*        calculated for any pixel for which the PSF-weighted fraction
*        of good input pixels is at least 0.1.  This will cause the
*        smoothing operation to fill in moderately sized regions of bad
*        pixels.

*  Related Applications:
*     KAPPA: BLOCK, CONVOLVE, FFCLEAN, MATHS, MEDIAN, PSF; Figaro:
*     ICONV3, ISMOOTH, IXSMOOTH, MEDFILT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.  The bad-pixel flag is also written for the data and
*     variance arrays.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single-precision floating point, or double
*     precision, if appropriate.

*  Timing:
*     For a circular PSF, the execution time is approximately
*     proportional to the number of pixels in the image to be smoothed
*     and to the value given for the BOX parameter.  By default, this
*     latter value is proportional to the value given for FWHM.  For an
*     elliptical PSF, the execution time is approximately proportional
*     to the number of pixels in the image to be smoothed and to the
*     product of the values given for the BOX parameter.  By default,
*     these latter values are approximately proportional to the values
*     given for FWHM.  Execution time will be approximately doubled if
*     a variance array is present in the input NDF.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2000, 2004 Central Laboratory
*         of the Research Councils. All Rights Reserved.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     7-AUG-1990 (RFWS):
*        Original version.
*     24-SEP-1990 (RFWS):
*        Fixed error in the smoothing of the variance component.
*     25-SEP-1990 (RFWS):
*        Added calls to ERR_MARK and ERR_RLSE when checking for null
*        parameter values.
*     25-SEP-1990 (RFWS):
*        Converted to use full-width at half-maximum (FWHM) rather than
*        standard deviation (SIGMA).
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 4 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 November 13 (MJC):
*        Added support for elliptical Gaussian PSFs.
*     1995 April 5 (MJC):
*        Renamed from GAUSS to avoid clash with Figaro.  Made to work on
*        one-dimensional arrays.  Used lowercase examples and usage.
*        Added Related Applications and additional commentary.  Changed
*        default of TITLE to null.  Used PSX to obtain workspace.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     13-APR-2000 (DSB):
*        Corrected argument list for KPS1_PSEVL.
*     19-APR-2000 (DSB):
*        Increased upper limit on FWHM from 100 to 10000.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 June 9 (MJC):
*        Added support for smoothing all two-dimensional planes in a
*        three-dimensional cube.
*     07-FEB-2007 (PWD):
*        Corrected argument list for KPS1_PSEVL. Set missing "AMP"
*        argument to value 1.0.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality required
      PARAMETER( NDIM = 2 )

*  Local Variables:
      CHARACTER * ( 13 ) COMP    ! List of components to process
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Numeric type for output arrays
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Numeric type for processing
      DOUBLE PRECISION WLIM      ! Limit on weighted sum of good pixels
      INTEGER BOX( NDIM )        ! Smoothing box size
      INTEGER BOXDEF( NDIM )     ! Default sizes for the rectangular box
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER EL                 ! Number of mapped array elements
      INTEGER I                  ! Loop counter
      INTEGER IBOX               ! Smoothing box half-size
      INTEGER IERR               ! Index of the first type-conversion
                                 ! error
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF pixel axes
      INTEGER JBOX               ! Smoothing box half-size in y
                                 ! direction
      INTEGER NDFI               ! Identifier for input NDF
      INTEGER NDFIB              ! Section of input NDF to be smoothed
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NDFOB              ! Section of output NDF to be filled
      INTEGER NERR               ! Number of type-conversion errors
      INTEGER NOFWHM             ! Number of FWHM values
      INTEGER NVAL               ! Number of BOX values
      INTEGER PAXHI              ! Upper pixel bound of perp. axis
      INTEGER PAXLO              ! Lower pixel bound of perp. axis
      INTEGER PAXVAL             ! Current pixel value on perp. axis
      INTEGER PERPAX             ! Index of axis perp. to smoothing
                                 ! plane
      INTEGER PNTR1( 2 )         ! Pointers for mapped input arrays
      INTEGER PNTR2( 2 )         ! Pointers for mapped output arrays
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF pixel axes
      INTEGER WDIM               ! Dimension of workspace array
      INTEGER WPNTR1             ! Mapped workspace pointer
      INTEGER WPNTR2             ! Mapped workspace pointer
      INTEGER WPNTR3             ! Mapped workspace pointer
      LOGICAL BAD                ! Check for bad input pixels?
      LOGICAL BADDAT             ! Bad values stored in o/p data array?
      LOGICAL BADOUT             ! Bad pixels in output array?
      LOGICAL BADVAR             ! Bad values stored in o/p variance?
      LOGICAL CIRCUL             ! Circular Gaussian PSF?
      LOGICAL SAMBAD             ! Propagate bad pixels to same place?
      LOGICAL VAR                ! Variance array present?
      REAL AXISR                 ! Axis ratio of the elliptical PSF
      REAL FWHM( NDIM )          ! Gaussian PSF full-width at half-max.
      REAL K( 4 )                ! Constants in the polar co-ordinate
                                 ! description of the elliptical PSF
      REAL ORIENT                ! Orientation of the elliptical
                                 ! Gaussian PSF w.r.t. x axis in degrees
      REAL RORI                  ! Orientation of the elliptical
                                 ! Gaussian PSF w.r.t. x axis in radians
      REAL RXMAX                 ! Polar angle of the elliptical PSF for
                                 ! which the x co-ordinate is largest
      REAL RYMAX                 ! Polar angle of the elliptical PSF for
                                 ! which the y co-ordinate is largest
      REAL SIGMA( NDIM )         ! Gaussian PSF standard deviation
      REAL XMAX                  ! Largest x co-ordinate enclosing
                                 ! ellipse PSF at 3 std. deviations
      REAL YMAX                  ! Largest y co-ordinate enclosing
                                 ! ellipse PSF at 3 std. deviations

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the input NDF and obtain the significant dimensions to smooth.
*  =====================================================================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF, its significant axes up to the maximum two
*  dimensions for processing, and the NDF's bounds.  If the NDF
*  possesses three significant dimensions, obtain an iteration axis
*  through parameter AXES, so that planes along that axis can be
*  processed in sequence.
      CALL KPG1_GNDFP( 'IN', 'AXES', NDIM, 'READ', NDFI, SDIM, LBND,
     :                 UBND, PERPAX, STATUS )

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine its dimensions (note that only two significant dimensions
*  can be accommodated).  Then ignore non-significant dimensions.
      DIM( 1 ) = UBND( SDIM( 1 ) ) - LBND( SDIM( 1 ) ) + 1
      DIM( 2 ) = UBND( SDIM( 2 ) ) - LBND( SDIM( 2 ) ) + 1

*  Decide whether the Gaussian smoothing is circularly symmetric or not.
*  =====================================================================

*  Determine if the input NDF is one or two-dimensional.
      CIRCUL = DIM( 1 ) .EQ. 1 .OR. DIM( 2 ) .EQ. 1

*  Obtain up to two values of the FWHM for the Gaussian point-spread
*  function (PSF) to be applied.  Suggest a default of 2.0 and
*  constrain it to lie within sensible limits.
      IF ( CIRCUL ) THEN
         CALL PAR_GDR0R( 'FWHM', 2.0, 0.1, 10000.0, .FALSE., FWHM( 1 ),
     :                   STATUS )
         NOFWHM = 1
      ELSE
         CALL PAR_DEF1R( 'FWHM', 1, 2.0, STATUS )
         CALL PAR_GDRVR( 'FWHM', NDIM, 0.1, 10000.0, FWHM, NOFWHM,
     :                   STATUS )

*  Find whether two different values have been given, and hence whether
*  an elliptical or circular PSF is required.
         IF ( NOFWHM .GT. 1 ) THEN
            CIRCUL = FWHM( 1 ) .EQ. FWHM( 2 )
            IF ( CIRCUL ) NOFWHM = 1
         ELSE
            CIRCUL = .TRUE.
         END IF
      END IF

*  Allow for an abort.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Convert the value(s) obtained to a value for the Gaussian standard
*  deviation SIGMA.
      DO I = 1, NOFWHM
         SIGMA( I ) = FWHM( I ) / SQRT( 8.0 * LOG( 2.0 ) )
      END DO

*  Obtain the other appropriate parameter values.
*  ==============================================
      IF ( CIRCUL ) THEN

*  Obtain a smoothing box size (this defines the absolute limits of the
*  PSF).  The suggested default is a box just large enough to
*  accommodate the PSF out to 3 standard deviations.  The box size must
*  be a positive odd number, so derive IBOX so that BOX = 2*IBOX+1 is
*  rounded up if necessary.  Ignore any second value.
         BOXDEF( 1 ) = 2 * NINT( 3.0 * SIGMA( 1 ) ) + 1
         CALL PAR_DEF1I( 'BOX', 1, BOXDEF, STATUS )

         IF ( STATUS .NE. SAI__OK ) GO TO 999
         CALL PAR_GDRVI( 'BOX', NDIM, 1, VAL__MAXI, BOX, NVAL, STATUS )

         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            BOX( 1 ) = BOXDEF( 1 )
            NVAL = 1

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999

         END IF

         IBOX = MAX( BOX( 1 ), 1 ) / 2
      ELSE

*  Obtain the orientation of the ellipse.
         CALL PAR_GDR0R( 'ORIENT', VAL__BADR, 0.0, 180.0, .FALSE.,
     :                   ORIENT, STATUS )

*  Allow for an abort.
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Convert it to radians.
         RORI = ORIENT * 1.7454329E-02

*  Find the maximum x and y co-ordinates of the ellipse oriented at this
*  angle.  First define some constants to parameterise the ellipse
         K( 1 ) = 3.0 * SIGMA( 1 ) * COS( RORI )
         K( 3 ) = 3.0 * SIGMA( 1 ) * SIN( RORI )
         K( 2 ) = 3.0 * SIGMA( 2 ) * SIN( RORI )
         K( 4 ) = 3.0 * SIGMA( 2 ) * COS( RORI )

*  Find the orientation in polar co-ordinates of the points in the
*  locus that correspond to the maximum x and y co-ordinates.  This is
*  done by differentiating the x-y co-ordinates with respect to polar
*  orientation and setting these to zero.
         RXMAX = ATAN2( K( 2 ), -K( 1 ) )
         RYMAX = ATAN2( K( 4 ), K( 3 ) )

*  Hence evaluate the maximum x-y co-ordinates at three sigma.
         XMAX = K( 1 ) * COS( RXMAX ) - K( 2 ) * SIN( RXMAX )
         YMAX = K( 3 ) * COS( RYMAX ) + K( 4 ) * SIN( RYMAX )

*  Obtain a smoothing box dimensions (this defines the absolute limits
*  of the PSF).  The suggested defaults defines a box just large enough
*  to accommodate the PSF out to 3 standard deviations.  The box
*  dimensions must be positive odd numbers, and hence derive the half
*  widths.
         BOXDEF( 1 ) = 2 * NINT( ABS( XMAX ) ) + 1
         BOXDEF( 2 ) = 2 * NINT( ABS( YMAX ) ) + 1
         CALL PAR_DEF1I( 'BOX', NDIM, BOXDEF, STATUS )

         IF( STATUS .NE. SAI__OK ) GO TO 999
         CALL PAR_GDRVI( 'BOX', NDIM, 1, VAL__MAXI, BOX, NVAL, STATUS )

         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            BOX( 1 ) = BOXDEF( 1 )
            BOX( 2 ) = BOXDEF( 2 )
            NVAL = 2
         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

         IBOX = MAX( BOX( 1 ), 1 ) / 2

*  If only one value was given, a square is used.
         IF ( NVAL .EQ. 1 ) THEN
            JBOX = MAX( BOX( 1 ), 1 ) / 2
         ELSE
            JBOX = MAX( BOX( 2 ), 1 ) / 2
         END IF
      END IF

*  Obtain the minimum weighted fraction of good pixels which should be
*  used to calculate an output pixel value (the maximum is unity in the
*  absence of bad pixels within the PSF area).  Test if a null value is
*  specified and set SAMBAD appropriately, anulling the error.
      CALL ERR_MARK
      SAMBAD = .FALSE.
      CALL PAR_GDR0D( 'WLIM', 0.5D0, 1.0D-6, 1.0D0, .FALSE., WLIM,
     :                STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         SAMBAD = .TRUE.
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Determine which arrays to process and the precision.
*  ====================================================

*  Determine if a variance component is present and derive a list of
*  the components to be processed.
      CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )
      IF ( VAR ) THEN
         COMP = 'Data,Variance'
      ELSE
         COMP = 'Data'
      END IF

*  Determine the numeric type to be used for processing the input
*  arrays.  This application supports single- and double-precision
*  floating point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFI, NDFI, COMP, ITYPE, DTYPE,
     :                STATUS )

*  Create an output NDF based on the input one.  Set an appropriate
*  numeric type for the output arrays.
      CALL LPG_PROP( NDFI, 'WCS,Axis,Quality,Units', 'OUT', NDFO,
     :               STATUS )
      CALL NDF_STYPE( DTYPE, NDFO, COMP, STATUS )

*  See if it is necessary to check for bad pixels in the input arrays.
      CALL NDF_MBAD( .TRUE., NDFI, NDFI, COMP, .FALSE., BAD, STATUS )

*  Do the steps that can be performed outside the loop.
*  ====================================================
      IF ( CIRCUL ) THEN

*  Obtain workspace arrays for the smoothing algorithm and map them.
         WDIM = 2 * IBOX + 1
         CALL PSX_CALLOC( WDIM, ITYPE, WPNTR1, STATUS )
         CALL PSX_CALLOC( DIM( 1 ), ITYPE, WPNTR2, STATUS )
         CALL PSX_CALLOC( DIM( 1 ), ITYPE, WPNTR3, STATUS )

      ELSE
         WDIM = ( 2 * IBOX + 1 ) * ( 2 * JBOX + 1 )
         CALL PSX_CALLOC( WDIM, '_REAL', WPNTR1, STATUS )

*  Get some more workspace for a double-precision version of the
*  weights.
         IF ( ITYPE .EQ. '_DOUBLE' )
     :     CALL PSX_CALLOC( WDIM, ITYPE, WPNTR2, STATUS )

*  Find the axis ratio of the ellipical PSF.
         AXISR = MAX( FWHM( 1 ), FWHM( 2 ) ) /
     :           MIN( FWHM( 1 ), FWHM( 2 ) )

*  Evaluate the point-spread function within the box.  Gamma is 2.0 for
*  a Gaussian.
         CALL KPS1_PSEVL( 1.0, AXISR, RORI, MIN( FWHM( 1 ), FWHM( 2 ) ),
     :                    2.0, 1, 2 * IBOX + 1, 1, 2 * JBOX + 1,
     :                    REAL( IBOX ) + 0.5, REAL( JBOX ) + 0.5,
     :                    %VAL( CNF_PVAL( WPNTR1 ) ), STATUS )

         IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Make a double-precision copy of the array of weights.
            CALL VEC_RTOD( .FALSE., WDIM, %VAL( CNF_PVAL( WPNTR1 ) ),
     :                     %VAL( CNF_PVAL( WPNTR2 ) ),
     :                     IERR, NERR, STATUS )
         END IF

      END IF

*  Only proceed around the loop if everything is satisfactory.
      IF ( STATUS .NE. SAI__OK ) GOTO 990

*  Loop around planes.
*  ===================

*  Initialise bad-data and bad-variance flags.
      BADDAT = .FALSE.
      BADVAR = .FALSE.

*  Loop round every slice to be smoothed.
      PAXLO = LBND( PERPAX )
      PAXHI = UBND( PERPAX )
      DO PAXVAL = PAXLO, PAXHI

*  Get identifiers for the required slices of the input and output NDF.
         LBND( PERPAX ) = PAXVAL
         UBND( PERPAX ) = PAXVAL
         CALL NDF_SECT( NDFI, NDF__MXDIM, LBND, UBND, NDFIB, STATUS )
         CALL NDF_SECT( NDFO, NDF__MXDIM, LBND, UBND, NDFOB, STATUS )

*  Map these input and output arrays.
         CALL KPG1_MAP( NDFIB, COMP, ITYPE, 'READ', PNTR1, EL, STATUS )
         CALL KPG1_MAP( NDFOB, COMP, ITYPE, 'WRITE', PNTR2, EL, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 990

*  Perform smoothing using a circular Gaussian PSF.
*  ================================================
         IF ( CIRCUL ) THEN

*  Apply smoothing to the mapped data array, using the appropriate
*  numeric type of processing.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_GAUSR( SIGMA, IBOX, SAMBAD, SNGL( WLIM ),
     :                          DIM( 1 ), DIM( 2 ), BAD, .FALSE.,
     :                          %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                          BADOUT, %VAL( CNF_PVAL( WPNTR1 ) ),
     :                          %VAL( CNF_PVAL( WPNTR2 ) ),
     :                          %VAL( CNF_PVAL( WPNTR3 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_GAUSD( SIGMA, IBOX, SAMBAD, WLIM, DIM( 1 ),
     :                          DIM( 2 ), BAD, .FALSE.,
     :                          %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTR2( 1 ) ) ), BADOUT,
     :                          %VAL( CNF_PVAL( WPNTR1 ) ),
     :                          %VAL( CNF_PVAL( WPNTR2 ) ),
     :                          %VAL( CNF_PVAL( WPNTR3 ) ), STATUS )
            END IF

*  Update the bad-data flag.
            IF ( BADOUT ) BADDAT = .TRUE.

*  If a variance array is present, then also apply smoothing to it.
            IF ( VAR ) THEN
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_GAUSR( SIGMA, IBOX, SAMBAD, SNGL( WLIM ),
     :                             DIM( 1 ), DIM( 2 ), BAD, .TRUE.,
     :                             %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                             %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                             BADOUT, %VAL( CNF_PVAL( WPNTR1 ) ),
     :                             %VAL( CNF_PVAL( WPNTR2 ) ),
     :                             %VAL( CNF_PVAL( WPNTR3 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_GAUSD( SIGMA, IBOX, SAMBAD, WLIM, DIM( 1 ),
     :                             DIM( 2 ), BAD, .TRUE.,
     :                             %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                             %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                             BADOUT, %VAL( CNF_PVAL( WPNTR1 ) ),
     :                             %VAL( CNF_PVAL( WPNTR2 ) ),
     :                             %VAL( CNF_PVAL( WPNTR3 ) ), STATUS )
               END IF

*  Update the bad-variance flag.
               IF ( BADOUT ) BADVAR = .TRUE.
            END IF


*  Perform smoothing using an elliptical Gaussian PSF.
*  ===================================================
         ELSE

*  Apply smoothing to the mapped data array, using the appropriate
*  numeric type of processing.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_AKERR( IBOX, JBOX, %VAL( CNF_PVAL( WPNTR1 ) ),
     :                          SAMBAD,
     :                          SNGL( WLIM ), DIM( 1 ), DIM( 2 ), BAD,
     :                          .FALSE., %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                          BADOUT, STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_AKERD( IBOX, JBOX, %VAL( CNF_PVAL( WPNTR2 ) ),
     :                          SAMBAD,
     :                          WLIM, DIM( 1 ), DIM( 2 ), BAD, .FALSE.,
     :                          %VAL( CNF_PVAL( PNTR1( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTR2( 1 ) ) ),
     :                          BADOUT, STATUS )
            END IF

*  Update the bad-data flag.
            IF ( BADOUT ) BADDAT = .TRUE.

*  If a variance array is present, then also apply smoothing to it.
            IF ( VAR ) THEN
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_AKERR( IBOX, JBOX,
     :                             %VAL( CNF_PVAL( WPNTR1 ) ), SAMBAD,
     :                             SNGL( WLIM ), DIM( 1 ), DIM( 2 ),
     :                             BAD, .TRUE.,
     :                             %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                             %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                             BADOUT, STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_AKERD( IBOX, JBOX,
     :                             %VAL( CNF_PVAL( WPNTR2 ) ), SAMBAD,
     :                             WLIM, DIM( 1 ), DIM( 2 ), BAD,
     :                             .TRUE.,
     :                             %VAL( CNF_PVAL( PNTR1( 2 ) ) ),
     :                             %VAL( CNF_PVAL( PNTR2( 2 ) ) ),
     :                             BADOUT, STATUS )
               END IF

*  Update the bad-variance flag.
               IF ( BADOUT ) BADVAR = .TRUE.
            END IF

         END IF

*  Free the section identifiers.
         CALL NDF_ANNUL( NDFIB, STATUS )
         CALL NDF_ANNUL( NDFOB, STATUS )

      END DO

*  Set the output bad-pixel flag of the data array.
      CALL NDF_SBAD( BADDAT, NDFO, 'Data', STATUS )

*  Set the output bad-pixel flag of the variance array.
      IF ( VAR ) CALL NDF_SBAD( BADVAR, NDFO, 'Variance', STATUS )

*  Obtain a new title for the output NDF.  The input NDF's title was
*  already propagated by the LPG_PROP call and so a null value will
*  leave it unaltered.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  Tidy up.
*  ========
 990  CONTINUE

*  Release the temporary workspace arrays.
      CALL PSX_FREE( WPNTR1, STATUS )
      IF ( CIRCUL ) THEN
         CALL PSX_FREE( WPNTR2, STATUS )
         CALL PSX_FREE( WPNTR3, STATUS )
      ELSE
         IF ( ITYPE .EQ. '_DOUBLE' ) CALL PSX_FREE( WPNTR2, STATUS )
      END IF

*  End the NDF context.
  999 CONTINUE
      CALL NDF_END( STATUS )

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GAUSMOOTH_ERR',
     :     'GAUSMOOTH: Error smoothing an NDF using a one- or '/
     :     /'two-dimensional Gaussian filter.', STATUS )
      END IF

      END
