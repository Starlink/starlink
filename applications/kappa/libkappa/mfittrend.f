      SUBROUTINE MFITTREND( STATUS )
*+
*  Name:
*     MFITTREND

*  Purpose:
*     Fits independent trends to data lines that are parallel to an
*     axis.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MFITTREND( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine fits trends to all lines of data in an NDF that lie
*     parallel to a chosen axis.  The trends are characterised by
*     polynomials of order up to 15, or by cubic splines.  The fits can
*     be restricted to use data that only lies within a series of
*     co-ordinate ranges along the selected axis.
*
*     The ranges may be determined automatically.  There is a choice
*     of tunable approaches to mask regions to be excluded from the
*     fitting to cater for a variety of data sets.  The actual ranges
*     used are reported in the current co-ordinate Frame and pixels,
*     provided they apply to all lines being fitted.
*
*     Once the trends have been determined they can either be stored
*     directly or subtracted from the input data.  If stored directly
*     they can be subtracted later.  The advantage of that approach is
*     the subtraction can be undone, but at some cost in efficiency.
*
*     Fits may be rejected if their root-mean squared (rms) residuals
*     are more than a specified number of standard deviations from the
*     the mean rms residuals of the fits.  Rejected fits appear as
*     bad pixels in the output data.

*     Fitting independent trends can be useful when you need to remove
*     the continuum from a spectral cube, where each spectrum is
*     independent of the others (that is you need an independent
*     continuum determination for each position on the sky).  It can
*     also be used to de-trend individual spectra and perform functions
*     like debiassing a CCD which has bias strips.

*  Usage:
*     mfittrend in axis ranges out { order
*                                  { knots=?
*                                  fittype

*  ADAM Parameters:
*     ARANGES() = _INTEGER (Write)
*        This parameter is only written when AUTO=TRUE, recording the
*        trend-axis fitting regions determined automatically.  They
*        comprise pairs of pixel co-ordinates.
*     AUTO = _LOGICAL (Read)
*        If TRUE, the ranges that define the trends are determined
*        automatically, and parameter RANGES is ignored.  [FALSE]
*     AXIS = LITERAL (Read)
*        The axis of the current co-ordinate system that defines the
*        direction of the trends.  This is specified using one of the
*        following options.
*
*        - An integer index of an axis within the current Frame of the
*        input NDF (in the range 1 to the number of axes in the current
*        Frame).
*
*        - An axis symbol string such as "RA" or "VRAD".
*
*        - A generic option where "SPEC" requests the spectral axis,
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*        sky longitude and latitude axes respectively.  Only those axis
*        domains present are available as options.
*
*        A list of acceptable values is displayed if an illegal value
*        is supplied.  If the axes of the current Frame are not parallel
*        to the NDF pixel axes, then the pixel axis which is most nearly
*        parallel to the specified current Frame axis will be used.
*        AXIS defaults to the last axis.  [!]
*     CLIP() = _REAL (Read)
*        Array of standard-deviation limits for progressive clipping
*        of outlying binned (see NUMBIN) pixel values while determining
*        the fitting ranges automatically.  It is therefore only
*        applicable when AUTO=TRUE.  Its purpose is to exclude features
*        that are not part of the trends.
*
*        Pixels are rejected at the ith clipping cycle if they lie
*        beyond plus or minus CLIP(i) times the dispersion about the
*        median of the remaining good pixels.  Thus lower values of CLIP
*        will reject more pixels.  The normal approach is to start low
*        and progressivley increase the clipping factors, as the
*        dispersion decreases after the exclusion of features.  The
*        source of the dispersion depends on the value the METHOD
*        parameter.  Between one and five values may be supplied.
*        Supplying the null value (!), results in 2, 2.5, and 3
*        clipping factors.  [2,2,2.5,3]
*     FITTYPE = LITERAL (Read)
*        The type of fit.  It must be either "Polynomial" for a
*        polynomial or "Spline" for a bi-cubic B-spline.  ["Polynomial"]
*     FOREST = _LOGICAL (Read)
*        Set this TRUE if the data may contain spectral data with many
*        lines---a line forest---when using the automatic range mode
*        (AUTO=TRUE).  A different approach using the histogram
*        determines the baseline mode and noise better in the presence
*        of multiple lines.  This leads to improved masking of the
*        spectral lines and affords a better determination of the
*        baseline.  In a lineforest the ratio of baseline to line
*        regions is much reduced and hence normal sigma clipping,
*        when FOREST=FALSE, is biased.  [FALSE]
*     KNOTS = _INTEGER (Read)
*        The number of interior knots used for the cubic-spline fit
*        along the trend axis.  Increasing this parameter value
*        increases the flexibility of the surface.  KNOTS is only
*        accessed when FITTYPE="Spline".  See INTERPOL for how the knots
*        are arranged.  The default is the current value.
*
*        For INTERPOL=TRUE, the value must be in the range 1 to 11, and
*        4 is a reasonable value for flatish trends.  The initial
*        default is 4.
*
*        For INTERPOL=FALSE the allowed range is 1 to 60 with an initial
*        default of 8.  In this mode, KNOTS is the maximum number of
*        interior knots.
*
*        The upper limit of acceptable values for a trend axis is no
*        more than half of the axis dimension.   []
*     IN = NDF (Read & Write)
*        The input NDF.  On successful completion this may have the
*        trends subtracted, but only if SUBTRACT and MODIFYIN are both
*        set TRUE.
*     INTERPOL = _LOGICAL (Read)
*        The type of spline fit to use when FITTYPE="Spline".
*
*        If set TRUE, an interpolating spline is fitted by least squares
*        that ensures the fit is exact at the knots.  Therefore the knot
*        locations may be set by the POSKNOT parameter.
*
*        If set FALSE, a smoothing spline is fitted.  A smoothing factor
*        controls the degree of smoothing.  The factor is determined
*        iteratively between limits, hence it is the slower option of
*        the two, but generally gives better fits, especially for curvy
*        trends.  The location of of the knots is decided automatically
*        by Dierckx's algorithm, governed where they are most needed.
*        Knots are added when the weighted sum of the squared residuals
*        exceeds the smoothing factor.  A final fit is made with the
*        chosen smoothing, but finding the knots afresh.
*
*        The few iterations commence from the upper limit and progress
*        more slowly at each iteration towards the lower limit.  The
*        iterations continue until the residuals stabilise or the
*        maximum number of interior knots is reached or the lower limit
*        is reached.  The upper limit is the weighted sum of the squares
*        of the residuals of the least-squares cubic polynomial fit.
*        The lower limit is the estimation of the overall noise obtained
*        from a clipped mean the standard deviation in short segments
*        that diminish bias arising from the shape of the trend.  The
*        lower limit prevents too many knots being created and fitting
*        to the noise or fine features.
*
*        The iteration to a smooth fit makes a smoothing spline somewhat
*        slower.   [FALSE]
*     MASK = NDF (Write)
*        The name of the NDF to contain the feature mask.  It is only
*        accessed for automatic mode and METHOD="Single" or "Global".
*        It has the same bounds as the input NDF and the data array is
*        type _BYTE.  No mask NDF is created if null (!) is supplied.
*        [!]
*     METHOD = LITERAL (Given)
*        The method used to define the masked regions in automatic
*        mode.  Allowed values are as follows.
*
*        - "Region" -- This averages trend lines from a selected
*        representative region given by parameter SECTION and bins
*        neighbouring elements within this average line.  Then it
*        performs a linear fit upon the binned line, and rejects the
*        outliers, iteratively with standard-deviation clipping
*        (parameter CLIP).  The standard deviation is that of the
*        average line within the region.  The ranges are the
*        intervals between the rejected points, rescaled to the
*        original pixels.  They are returned in parameter ARANGES.
*
*        This is best suited to a low dispersion along the trend axis
*        and a single concentrated region containing the bulk of the
*        signal to be excluded from the trend fitting.
*
*        - "Single" -- This is like "Region" except there is neither
*        averaging of lines nor a single set of ranges.  Each line is
*        masked independently.  The dispersion for the clipping of
*        outliers within a line is the standard deviation within that
*        line.
*
*        This is more appropriate when the features being masked
*        vary widely across the image, and significantly between
*        adjacent lines.  Some prior smoothing or background tracing
*        (CUPID:FINDBACK) will usually prove beneficial.
*
*        - "Global" -- This is a variant of "Single".  The only
*        difference is that the dispersion used to reject features
*        using the standard deviation of the whole data array.  This is
*        more robust than "Single", however it does not perform
*        rejections well for lines with anomalous noise.
*
*        ["Single"]
*     MODIFYIN = _LOGICAL (Read)
*        Whether or not to modify the input NDF.  It is only used when
*        SUBTRACT is TRUE.  If MODIFYIN is FALSE, then an NDF name must
*        be supplied by the OUT parameter.  [FALSE]
*     NUMBIN = _INTEGER (Read)
*        The number of bins in which to compress the trend line for the
*        automatic range-determination mode.  A single line or even the
*        average over a region will often be noisy; this compression
*        creates a better signal-to-noise ratio from which to detect
*        features to be excluded from the trend fitting.  If NUMBIN is
*        made too large, weaker features will be lost or stronger
*        features will be enlarged and background elements excluded from
*        the fitting.  The minimum value is 16, and the maximum is such
*        that there will be a factor of two compression.  NUMBIN is
*        ignored when there are fewer than 32 elements in each line to
*        be de-trended.  [32]
*     ORDER = _INTEGER (Read)
*        The order of the polynomials to be used when trend fitting.
*        A polynomial of order 0 is a constant and 1 a line, 2 a
*        quadratic etc.  The maximum value is 15.  ORDER is only
*        accessed when FITTYPE="Polynomial".  [3]
*     OUT = NDF (Read)
*        The output NDF containing either the difference between the
*        input NDF and the various trends, or the values of the trends
*        themselves.  This will not be used if SUBTRACT and MODIFYIN
*        are TRUE (in that case the input NDF will be modified).
*     POSKNOT( ) = LITERAL (Read)
*        The co-ordinates of the interior knots for all trends.  KNOTS
*        values should be supplied, or just the null (!) value to
*        request equally spaced knots.  The units of these co-ordinates
*        is determined by the axis of the current world co-ordinate
*        system of the input NDF that corresponds to the trend axis.
*        Supplying a colon ":" will display details of the current
*        co-ordinate Frame.  [!]
*     PROPBAD = _LOGICAL (Read)
*        Only used if SUBTRACT is FALSE.  If PROPBAD is TRUE, the
*        returned fitted values are set bad if the corresponding input
*        value is bad.  If PROPBAD is FALSE, the fitted values are
*        retained.  [TRUE]
*     RANGES() = LITERAL (Read)
*        These are the pairs of co-ordinates that define ranges
*        along the trend axis.  When given these ranges are used to
*        select the values that are used in the fits.  The null value
*        (!) causes all the values along each data line to be used.  The
*        units of these ranges is determined by the axis of the current
*        world co-ordinate system that corresponds to the trend axis.
*        Supplying a colon ":" will display details of the current
*        co-ordinate Frame.  Up to ten pairs of values are allowed.
*        This parameter is not accessed when AUTO=TRUE.  [!]
*     RMSCLIP = _REAL (Read)
*        The number of standard deviations exceeding the mean of the
*        root-mean-squared residuals of the fits at which a fit is
*        rejected.  A null value (!) means perform no rejections.
*        Allowed values are between 2 and 15.  [!]
*     SECTION = LITERAL (Read)
*        The region from which representative lines are averaged
*        in automatic mode to determine the regions to fit trends.  It
*        is therefore only accessed when AUTO=TRUE, METHOD="Region", and
*        the dimensionality of the input NDF is more than 1.  The value
*        is defined as an NDF section, so that ranges can be defined
*        along any axis, and be given as pixel indices or axis (data)
*        co-ordinates.  The pixel axis corresponding to parameter AXIS
*        is ignored.   So for example, if the pixel axis were 3 in a
*        cube, the value "3:5,4," would average all the lines
*        in elements in columns 3 to 5 and row 4.  See "NDF sections"
*        in SUN/95, or the online documentation for details.
*
*        A null value (!) requests that a representative region around
*        the centre be used.  [!]
*     SUBTRACT = _LOGICAL (Read)
*        Whether not to subtract the trends from the input NDF or not.
*        If not, then the trends will be evaluated and written to a new
*        NDF (see also Parameter PROPBAD). [FALSE]
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN to be used
*        instead.  [!]
*     VARIANCE = _LOGICAL (Read)
*        If TRUE and the input NDF contains variances, then the
*        polynomial or spline fits will be weighted by the variances.

*  Examples:
*     mfittrend in=cube axis=3 ranges="1000,2000,3000,4000" order=4
*               out=detrend
*        This example fits cubic polynomials to the spectral axis of
*        a data cube. The fits only use the data lying within the
*        ranges 1000 to 2000 and 3000 to 4000 Angstroms (assuming
*        the spectral axis is calibrated in Angstroms and that is the
*        current co-ordinate system).  The fit is evaluated and
*        written to the data cube called detrend.
*     mfittrend in=cube axis=3 auto clip=[2,3] order=4 out=detrend
*        As above except the fitting ranges are determined automatically
*        with 2- then 3-sigma clipping using a representative central
*        region.
*     mfittrend in=cube axis=3 auto clip=[2,3] fittype=spline out=detrend
*               interpol
*        As the previous example except that interpolation cubic-spline
*        fits with four equally spaced interior knots are used to
*        characterise the trends.
*     mfittrend m51 3 out=m51_bsl mask=m51_msk auto fittype=spl
*        This example fits to trends along the third axis of NDF m51
*        and writes the evaluated fits to NDF m51_bsl.  The fits use a
*        smoothing cubic spline with the placement and number of
*        interior knots determined automatically.  Features are
*        determined automatically, and a mask of excluded features is
*        written to NDF m51_msk.
*     mfittrend cube axis=3 auto method=single order=1 subtract
*               out=cube_dt mask=cube_mask
*        This fits linear trends to the spectral axis of a data cube
*        called cube, masking spectral features along each line
*        independently.  The mask pixels are recorded in NDF cube_mask.
*        The fitted trend are subtracted and stored in NDF cube_dt.

*  Notes:
*     -  This application attempts to solve the problem of fitting
*     numerous polynomials in a least-squares sense and that do not
*     follow the natural ordering of the NDF data, in the most
*     CPU-time-efficient way possible.
*
*     To do this requires the use of additional memory (of order one
*     less than the dimensionality of the NDF itself, times the
*     polynomial order squared).  To minimise the use of memory and get
*     the fastest possible determinations you should not use weighting
*     and assert that the input data do not have any BAD values (use the
*     application SETBAD to set the appropriate flag).
*     -  If you choose to use the automatic range determination.  You
*     may need to determine empirically what are the best
*     clipping limits, binning factor, and for METHOD="Region" the
*     region to average.
*     -  You are advised to inspect the fits, especially the spline
*     fits or high-order polynomials.  A given set of trends may require
*     more than one pass through this task, if they exhibit varied
*     morphologies.  Use masking or NDF sections to select different
*     regions that are fit with different parameters.  The various trend
*     maps are then integrated with PASTE to form the final composite
*     set of trends that you can subtract.

*  Related Applications:
*     FIGARO: FITCONT, FITPOLY; CCDPACK: DEBIAS; KAPPA: SETBAD.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, UNITS, TITLE, HISTORY, WCS and VARIANCE components of an
*     NDF data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Handles data of up to 7 dimensions.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council.
*     Copyright (C) 2007-2008, 2012, 2016 Science and Technology
*     Facilities Council.
*     All Rights reserved.

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
*     PWD: Peter W. Draper (JAC, Durham University)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     14-SEP-2005 (PWD):
*        Original version, some parts from COLLAPSE.
*     2006 April 12 (MJC):
*        Remove unused variables.
*     2006 May 31 (MJC):
*        Added option for automatic estimations of the ranges.
*     2007 January 11 (MJC):
*        Added clipping of outlier fits via the RMSCLIP parameter.
*     2007 January 18 (MJC):
*        Constrain the automatic ranges to be within the NDF's bounds.
*        Record automatically determined fitting regions to output
*        parameter ARANGES.
*     5-MAR-2007 (DSB):
*        Add code to identify the required pixel axis in cases where the
*        WCS->pixel transformation is not defined.
*     12-MAY-2007 (DSB):
*        Add code to identify the required pixel ranges in cases where
*        the WCS->pixel transformation is not defined.
*     2007 July 12 (MJC):
*        Adapted COLLAPSE code to determine the alignment of pixel with
*        WCS axes.  This attempts first to split the current Frame into
*        parallel mappings including one that is solely de-trending
*        axis, before finding the largest projection of the vector
*        joining two test points.
*     2007 July 19 (MJC):
*        Used new KPG1_ASAPA to identify pixel axis corresponding to
*        the de-trended WCS axis, rather than inline code.
*     2007 August 10 (MJC):
*        Added METHOD and NUMBIN parameters.
*     2007 September 6 (MJC):
*        Create a mask for features detected by the automatic Single
*        method, rather than attempt to store the mask information in
*        the data.
*     2007 September 10 (MJC):
*        Allow the feature mask to be stored in an output NDF given by
*        new parameter MASK.  Output the selected regions in the current
*        co-ordinate Frame.
*     2007 October 3 (MJC):
*        Make the original Single method Global, and introduce a new
*        Single that uses the standard deviation within the line being
*        filtered.  Made Single method the default.
*     2007 December 19 (MJC):
*        Fixed logic bug when using Single mode when no bad pixels are
*        present.
*     2008 May 14 (MJC):
*        Add cubic-spline-fit option through new FITTYPE and KNOTS
*        parameters.  Packaged polynomial fitting by data type in
*        KPS1_LFTA.
*     2008 May 21 (MJC):
*        Added POSKNOT parameter.
*     2008 May 30 (MJC):
*        Added smoothing spline and INTERPOL parameter.
*     2008 December 19 (MJC):
*        Issue automatic-mode caveats at verbose reporting level.
*     2008 December 20 (MJC):
*        Use NDF_CLONE instead of NDF_FIND for null sections for
*        METHOD="Region" in automatic mode.
*     2010 December 30 (MJC):
*        Allow for very large datasets by blocking into manageable
*        sections.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     10-SEP-2013 (DSB):
*        Added parameter PROPBAD.
*     2016 March 29 (MJC):
*        Added Parameter FOREST for automatic-mode masking of many
*        spectral lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST parameters and functions
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'MSG_PAR'          ! Message-system constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'PRM_PAR'          ! Magic-value definitions

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER KPG1_FLOOR         ! Most positive integer .LE. a given
                                 ! real
      INTEGER KPG1_CEIL          ! Most negative integer .GE. a given
                                 ! real

*  Local Constants:
      INTEGER MAXBIB             ! Maximum number of bytes in a block
      PARAMETER ( MAXBIB = 67108864 ) ! Guestimate a size: 64MB

      INTEGER MAXRNG             ! Maximum number of range limits
      PARAMETER( MAXRNG = 20 )

      INTEGER MXCLIP             ! Maximum number of clips of the data
      PARAMETER ( MXCLIP = 5 )

      INTEGER OPTBIN             ! Nominal number of bins
      PARAMETER ( OPTBIN = 32 )

*  TOKNOT must be at least UPKNOT.  FKNOT dimension uses TOKNOT because
*  we want to pass NKNOT for a smoothing spline, and it is also the
*  dimension of the FKNOT array in the fitting subroutine.
      INTEGER TOKNOT             ! Maximum number of interior knots
      PARAMETER ( TOKNOT = 60 )  ! smoothing spline

      INTEGER UPKNOT             ! Upper limit to number of interior
      PARAMETER ( UPKNOT = 11 )  ! knots, interpolating spline

*  Local Variables:
      INTEGER AREA               ! Area of axes orthogonal to fit axis
      CHARACTER*9 ATTR           ! Name of an AST attribute
      LOGICAL AUTO               ! Determine regions automatically?
      INTEGER BLDIMS( NDF__MXDIM ) ! NDF dimensions in current block
      INTEGER BPV                ! Number of bytes per data value
      INTEGER CFRM               ! Current frame
      REAL CLIP( MXCLIP )        ! Clipping sigmas during binning
      LOGICAL CLIPRE             ! Clip the outlier residuals?
      REAL CLPRMS                ! Clipping sigma for outlier rejection
      CHARACTER*15 COMP          ! List of array components to process
      DOUBLE PRECISION CPOS( 2, NDF__MXDIM ) ! Two current Frame
                                 ! positions
      INTEGER D                  ! A dimension size
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of NDF
      DOUBLE PRECISION DRANGE( MAXRNG ) ! Fit ranges world co-ordinates
      DOUBLE PRECISION DKNOTS( UPKNOT ) ! Knots' world co-ordinates
      INTEGER DSIZE              ! Number of elements in data array
      INTEGER EL                 ! Number of mapped elements
      CHARACTER*16 FITYPE        ! Type of fit ('POLYNOMIAL'|'SPLINE')
      REAL FKNOT( TOKNOT )       ! Grid co-ordinates of fixed knots
      LOGICAL FILMSK             ! Fill a mask array?
      CHARACTER*14 FMT           ! Format string
      LOGICAL FOREST             ! Data may contain line forests?
      LOGICAL GLOBAL             ! Use the automatic global method?
      LOGICAL HASBAD             ! Input NDF may have BAD pixels?
      LOGICAL HAVVAR             ! Have a variance component?
      INTEGER I                  ! Loop variable
      INTEGER IAXIS              ! Index of axis within current Frame
      INTEGER IBL                ! Identifier for input-NDF block
      INTEGER IBLOCK             ! Loop counter for the NDF blocks
      INTEGER IERR               ! Position of first error (dummy)
      INTEGER INNDF              ! NDF identifier of input NDF
      LOGICAL INTERP             ! Interpolation spline?
      INTEGER IPAS               ! Pointer to workspace
      INTEGER IPBS               ! Pointer to coefficients
      INTEGER IPCO               ! Pointer to co-ordinates
      INTEGER IPCOEF             ! Pointer to spline coefficients
      INTEGER IPCOL              ! Pointer to collapsed rms residuals
      INTEGER IPDAT( 2 )         ! Pointer to NDF data & variance comp's
      INTEGER IPGOOD             ! Pointer to numbers of good values
      INTEGER IPIN               ! Pointer to input NDF data
      INTEGER IPIX               ! Index of PIXEL Frame within FrameSet
      INTEGER IPKNOT             ! Pointer to knots
      INTEGER IPMASK             ! Pointer to feature mask
      INTEGER IPMN               ! Pointer to mask NDF's data array
      INTEGER IPNC               ! Pointer to number of coefficients
      INTEGER IPRES              ! Pointer to array of residuals
      INTEGER IPSCAL             ! Pointer to scale factors
      INTEGER IPTMP( 1 )         ! Pointer to temporary NDF component
      INTEGER IPVAL              ! Pointer to values
      INTEGER IPVAR( 1 )         ! Pointer to NDF variance component
      INTEGER IPWRK1             ! Pointer to workspace
      INTEGER IPWRK2             ! Pointer to workspace
      INTEGER IPWRK3             ! Pointer to workspace
      INTEGER IPWT               ! Pointer to weights
      CHARACTER*( NDF__SZTYP ) ITYPE ! Numeric type for processing
      INTEGER IWCS               ! AST FrameSet identifier
      INTEGER JAXIS              ! Index of axis within pixel Frame
      INTEGER JHI                ! High pixel index for axis
      INTEGER JLO                ! Low pixel index for axis
      INTEGER LATTR              ! Used length of ATTR
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      CHARACTER*( DAT__SZLOC ) LOC ! Locator for the input NDF
      LOGICAL LOOP               ! Continue to loop through dimensions?
      INTEGER MAP                ! PIXEL Frame to Current Frame Mapping
                                 ! pointer
      INTEGER MAXBIN             ! Maximum number of bins for auto mode
      INTEGER MAXSIZ             ! Maximum size of block (in pixels)
      INTEGER MBDIMS( NDF__MXDIM ) ! Maximum NDF dimensions in a block
      INTEGER MBL                ! Identifier for mask-NDF block
      CHARACTER*9 METHOD         ! Method for determining the mode
      LOGICAL MODIN              ! Modify input NDF by subtracting fits?
      INTEGER MSKNDF             ! Identifier of the mask NDF
      INTEGER MXKNOT             ! Maximum number of knots
      INTEGER NAXC               ! Number of axes in current frame
      INTEGER NBLOCK             ! Number of NDF blocks
      INTEGER NCF                ! Used length of FMT
      INTEGER NCLIP              ! Number of clips of averaged data
      INTEGER NCSECT             ! Number of characters in section
      INTEGER NDFS               ! NDF identifier of section
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NERR               ! Number of errors
      INTEGER NFIT               ! Number of successful fits
      INTEGER NGOOD              ! Number of good knot positions
      INTEGER NKNOT              ! Number of interior knots
      INTEGER NRANGE             ! Number of range values (not pairs)
      INTEGER NUMBIN             ! Number of bins in automatic mode
      INTEGER NWS                ! Size of spline-fitting routine's
                                 ! workspace
      INTEGER OBL                ! Identifier for output-NDF block
      INTEGER ORDER              ! The order of the polynomial to fit
      INTEGER OTOMAP             ! One-to-one mapping
      INTEGER OUTNDF             ! NDF identifier of output NDF
      INTEGER PLACE              ! Placeholder for temporary NDF
      DOUBLE PRECISION PKNOTS( UPKNOT ) ! Knots' pixel co-ordinates
      DOUBLE PRECISION PPOS( 2, NDF__MXDIM ) ! Two pixel Frame positions
      DOUBLE PRECISION PRANGE( MAXRNG ) ! Fit ranges pixel co-ordinates
      DOUBLE PRECISION PXHIGH    ! High pixel bound of fitted axis
      DOUBLE PRECISION PXLOW     ! Low pixel bound of fitted axis
      LOGICAL PRPBAD             ! Propagate bad input values to fit?
      LOGICAL QUICK              ! Use quicker code?
      INTEGER RANGES( MAXRNG )   ! The fit ranges pixels
      LOGICAL REGION             ! Representative region auto method?
      CHARACTER*80 SECT          ! Section specifier
      LOGICAL SINGLE             ! Single-line automatic method?
      LOGICAL SUBTRA             ! Subtract fit from data?
      INTEGER SWPTR              ! Pointer to spline-fitting workspace
      INTEGER TBL                ! Identifier for temporary-NDF block
      INTEGER TMPNDF             ! Identifier of temporary NDF
      CHARACTER*( 10*UPKNOT + 3 ) TOKEN ! List of knot grid positions
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      LOGICAL USEALL             ! Use the entire axis?
      LOGICAL USEVAR             ! Use variance as weights in fits?

*.

*  Future development notes: should look at storing the coefficients and
*  write a model evaluating application MAKETREND?  This would follow
*  the KAPPA model more closely and allow the fit to be undone, even
*  when subtracting directly.  Maybe a need for a statistics-generating
*  version too, but the quality of the fits is a potentially large
*  amount of information.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the main parameters.
*  ===========================

*  Obtain the type of the fit ('POLYNOMIAL' or 'SPLINE').
      CALL PAR_CHOIC( 'FITTYPE', 'Polynomial', 'Polynomial,Spline',
     :                .TRUE., FITYPE, STATUS )

*  Constrain the number of interior knots given the number of pixels.
*  This is not perfect, as we really need to know the number of pixels.
      IF ( FITYPE( 1:3 ) .EQ. 'SPL' ) THEN

*  Obtain the type of spline fitting.
         CALL PAR_GET0L( 'INTERPOL', INTERP, STATUS )

*  Obtain the number of interior knots.  Defer the number of knots
*  for the smoothing spline until the number of data values are known,
*  but set a temporary value to prevent problems.  Allow more knots
*  for the smoothing spline as the knot positions do not ned to be
*  supplied.
         IF ( INTERP ) THEN
            CALL PAR_GDR0I( 'KNOTS', 4, 1, UPKNOT, .FALSE., NKNOT,
     :                      STATUS )
         ELSE
            CALL PAR_GDR0I( 'KNOTS', 8, 1, 60, .FALSE., NKNOT, STATUS )
         END IF
      ELSE

*  Get the order of the polynomial.
         CALL PAR_GDR0I( 'ORDER', 3, 0, 15, .FALSE., ORDER, STATUS )
      END IF

*  See if we should subtract fit from data. Need to do this early as we
*  may be modifying the input NDF.
      CALL PAR_GET0L( 'SUBTRACT', SUBTRA, STATUS )

*  See if the input NDF should have the fits subtracted, only matters if
*  we're subtracting the fit.
      MODIN = .FALSE.
      IF ( SUBTRA ) THEN
         CALL PAR_GET0L( 'MODIFYIN', MODIN, STATUS )

*  See if bad pixels should be propagated to the output fit. Only matters
*  if we're not subtracting the fit.
      ELSE
         CALL PAR_GET0L( 'PROPBAD', PRPBAD, STATUS )
      END IF

*  Obtain the rms clipping threshold.
      CLIPRE = .FALSE.
      CALL PAR_GDR0R( 'RMSCLIP', 4.0, 2.0, 15.0, .FALSE., CLPRMS,
     :                STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         CLIPRE = .TRUE.
      END IF

*  Ask if there might be a line forest.
      CALL PAR_GET0L( 'FOREST', FOREST, STATUS )

*  Access the input NDF.
*  =====================
*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain identifier for the input NDF.
      IF ( MODIN ) THEN
         CALL LPG_ASSOC( 'IN', 'UPDATE', INNDF, STATUS )
      ELSE
         CALL LPG_ASSOC( 'IN', 'READ', INNDF, STATUS )
      END IF

*  Get the bounds and dimensionality.
      CALL NDF_BOUND( INNDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Extra dimensions have nominal size 1.
      DO I = NDIM + 1, NDF__MXDIM
         LBND( I ) = 1
         UBND( I ) = 1
      END DO

*  Get dimensions of NDF and fill maximum blocking size for unused
*  dimensions.
      EL = 1
      DO I = 1, NDF__MXDIM
         DIMS( I ) = UBND( I ) - LBND( I ) + 1
         EL = EL * DIMS( I )
         MBDIMS( I ) = DIMS( I )
      END DO

*  Do we have any variances to use for weights and should they be used?
      CALL NDF_STATE( INNDF, 'Variance', HAVVAR, STATUS )
      IF ( HAVVAR ) THEN
         CALL PAR_GET0L( 'VARIANCE', USEVAR, STATUS )
         COMP = 'Data,Variance'
      ELSE
         USEVAR = .FALSE.
         COMP = 'Data'
      END IF

*  Obtain WCS information.
*  =======================

*  Get the WCS FrameSet from the NDF.
      CALL KPG1_GTWCS( INNDF, IWCS, STATUS )

*  Extract the current Frame, this is used for picking the axis and the
*  units of the ranges.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      NAXC = AST_GETI( CFRM, 'NAXES', STATUS )

*  Get axis to fit the polynomials to.  Default is last axis in the WCS.
      IF ( NDIM .NE. 1 ) THEN
         IAXIS = NAXC
         CALL KPG1_GTAXI( 'AXIS', CFRM, 1, IAXIS, STATUS )
      ELSE
         IAXIS = 1
      END IF

*  Find the index of the PIXEL Frame.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Extract the Mapping from PIXEL Frame to Current Frame.
      MAP = AST_GETMAPPING( IWCS, IPIX, AST__CURRENT, STATUS )

      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the fit ranges.
*  ===================
      USEALL = .TRUE.

*  Manual or automatic estimation?
      CALL PAR_GET0L( 'AUTO', AUTO, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the ranges here so that it applies to both automatic
*  mode and when user specifies the whole range.
      DO I = 1, MAXRNG
         DRANGE( I ) = AST__BAD
      END DO

      IF ( AUTO ) THEN
         CALL MSG_BLANKIF( MSG__VERB, STATUS )
         CALL MSG_OUTIF( MSG__VERB, 'AUTOWARN1',
     :     'WARNING: The automatic mode has undergone only moderate '//
     :     'testing.  Check that the regions used for fitting '//
     :     'reported below are sensible, i.e. avoid features like '//
     :     'spectral lines.', STATUS )
         CALL MSG_BLANKIF( MSG__VERB, STATUS )
         CALL MSG_OUTIF( MSG__VERB, 'AUTOWARN2',
     :     'Feedback is welcome on the tuning of the CLIP '//
     :     'parameter''s default, the size of the default averaging '//
     :     'region, the binning resolution, and whether or not the '//
     :     'simple linear fit is adequate for detecting features '//
     :     'and not rejecting curvature in the trend.', STATUS )
         CALL MSG_BLANK( STATUS )

*  Inquire the method used to automate the rejection of features and
*  outliers.
         CALL PAR_CHOIC( 'METHOD', 'Single', 'Global,Region,Single',
     :                   .TRUE., METHOD, STATUS )
         REGION = METHOD .EQ. 'REGION'
         GLOBAL = METHOD .EQ. 'GLOBAL'
         SINGLE = METHOD .EQ. 'SINGLE' .OR. GLOBAL

      ELSE
         SINGLE = .FALSE.
         GLOBAL = .FALSE.

*  Get the ranges to use. These values are transformed from current
*  co-ordinates along the fit axis to pixel co-ordinates on some
*  NDF axis (we've yet to determine).
         CALL KPG1_GTAXV( 'RANGES', MAXRNG, .FALSE., CFRM, IAXIS,
     :                    DRANGE, NRANGE, STATUS )

*  If a null value was supplied then we should use the full extent.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE

*  Ranges must come in pairs.
            IF ( 2 * ( NRANGE / 2 ) .NE. NRANGE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'MFITTREND_ERR1',
     :                       'Range values must be supplied in pairs',
     :                       STATUS )
               GO TO 999
            END IF

*  We are not use all the range.
            USEALL = .FALSE.
         END IF
      END IF

*  Determine which pixel axis is most nearly aligned with the selected
*  WCS axis.
      CALL KPG1_ASAPA( INNDF, CFRM, MAP, IAXIS, DRANGE( 1 ),
     :                 DRANGE( 2 ), JAXIS, PXLOW, PXHIGH, OTOMAP,
     :                 STATUS )

*  Automatic mode to define ranges.
*  --------------------------------
      IF ( AUTO ) THEN

*  Obtain the binning factor.  The maximum compression should still
*  retain at least 16 or all the elements.
         MAXBIN = DIMS( IAXIS ) / 2
         IF ( DIMS( IAXIS ) .GE. OPTBIN ) THEN
            CALL PAR_GDR0I( 'NUMBIN', OPTBIN, OPTBIN / 2 , MAXBIN,
     :                      .FALSE., NUMBIN, STATUS )
         ELSE
            NUMBIN = DIMS( IAXIS )
         END IF

*  Obtain the clipping thresholds.
         NCLIP = 0
         CALL PAR_GDRVR( 'CLIP', MXCLIP, 1.0, VAL__MAXR,
     :                   CLIP, NCLIP, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            NCLIP = 3
            CLIP( 1 ) = 2.0
            CLIP( 2 ) = 2.5
            CLIP( 3 ) = 3.0

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            GOTO 999

         END IF

*  Region method
*  -------------

*  Obtain the NDF section of the test region.
         IF ( REGION ) THEN
            CALL KPS1_MFGNS( 'SECTION', JAXIS, NDIM, DIMS, NCSECT,
     :                       SECT, STATUS )

*  Obtain a locator to the input NDF.
            CALL NDF_LOC( INNDF, 'Read', LOC, STATUS )

*  Create the section in the input array.  Allow for a null string, or
*  for a one-dimensional NDF by continuing to use the input NDF, but
*  cloned so that the NDF section can be released independent of how it
*  was created.
            IF ( NCSECT .GT. 0 ) THEN
               CALL NDF_FIND( LOC, '(' // SECT( :NCSECT ) // ')', NDFS,
     :                        STATUS )
            ELSE
               CALL NDF_CLONE( INNDF, NDFS, STATUS )
            END IF

*  Form ranges by averaging the lines in the section, and then
*  performing a fit, and rejecting outliers.
            CALL KPS1_MFAUR( NDFS, JAXIS, NCLIP, CLIP, NUMBIN, MAXRNG,
     :                       NRANGE, RANGES, STATUS )
            CALL NDF_ANNUL( NDFS, STATUS )

*  Ensure that we have valid ranges before attempting to use them.
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Convert the GRID co-ordinates of the RANGES to pixel co-ordinates.
*  Also ensure that the selected regions are within the NDF bounds.
            DO I = 1, NRANGE
               RANGES( I ) = MAX( MIN( DIMS( JAXIS ), RANGES( I ) ), 1 )
     :                       + LBND( JAXIS ) - 1
            END DO

*  Record the ranges to a parameter.
            CALL PAR_PUT1I( 'ARANGES', NRANGE, RANGES, STATUS )

*  Single methods
*  --------------
         ELSE IF ( SINGLE ) THEN

*  Use NDF axis JAXIS.  Pick full extent.  Features will be flagged
*  as bad in a mask array.
            RANGES( 1 ) = LBND( JAXIS )
            RANGES( 2 ) = UBND( JAXIS )
            NRANGE = 2
         END IF

*  OK, use NDF axis JAXIS.  Pick full extent if no values were given.
      ELSE IF ( USEALL ) THEN
         RANGES( 1 ) = LBND( JAXIS )
         RANGES( 2 ) = UBND( JAXIS )
         NRANGE = 2

*  User-defined ranges
*  -------------------
*  Project the given ranges into pixel co-ordinates.
      ELSE
         DO I = 1, NRANGE, 2

*  If MAP has an inverse, we can use MAP directly.
            IF ( OTOMAP .EQ. AST__NULL ) THEN
               CPOS( 1, IAXIS ) = DRANGE( I + 1 )
               CPOS( 2, IAXIS ) = DRANGE( I )

               CALL AST_TRANN( MAP, 2, NAXC, 2, CPOS, .TRUE., NDIM, 2,
     :                         PPOS, STATUS )

*  Find the projection of the two test points onto the axis.
               JLO = KPG1_FLOOR( REAL( MIN( PPOS( 1, JAXIS ),
     :                                      PPOS( 2, JAXIS ) ) ) )
               JHI = KPG1_CEIL( REAL( MAX( PPOS( 1, JAXIS ),
     :                                     PPOS( 2, JAXIS ) ) ) )

*  Otherwise, we use the single-input single-output Mapping that
*  generates the requested WCS axis.
            ELSE
               CALL AST_TRAN1( OTOMAP, 2, DRANGE( I ), .TRUE., PRANGE,
     :                         STATUS )

               JLO = KPG1_FLOOR( REAL( MIN( PRANGE( 1 ),
     :                                      PRANGE( 2 ) ) ) )
               JHI = KPG1_FLOOR( REAL( MAX( PRANGE( 1 ),
     :                                      PRANGE( 2 ) ) ) )
            END IF

*  Ensure these are within the bounds of the pixel axis.
            JLO = MAX( LBND( JAXIS ), JLO )
            JHI = MIN( UBND( JAXIS ), JHI )

*  Report an error if there is no intersection.
            IF ( JLO .GT. JHI .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'LO', JLO )
               CALL MSG_SETI( 'HI', JHI )
               CALL ERR_REP( 'MFITTREND_ERR6', 'An axis range '//
     :                       'covers zero pixels (are the '//
     :                       'RANGE values equal or outside the '//
     :                       'bounds of the NDF?)(^LO:^HI)',
     :                       STATUS )
               GO TO 999
            END IF

*  Store pixel co-ordinates.
            RANGES( I ) = JLO
            RANGES( I + 1 ) = JHI
         END DO
      END IF

*  Report the ranges.
*  ------------------

*  Tell the user the ranges of pixel being used.  In the Single
*  automatic method there are no ranges kept only flagged in each line
*  to de-trend.
      IF ( .NOT. SINGLE ) THEN

*  Report the ranges in the current co-ordinate frame too, unless it is
*  PIXEL.
         IF ( AST__CURRENT .NE. IPIX ) THEN
            DO I = 1, NRANGE, 2

*  Convert the PIXEL co-ordinates back to the current Frame.
*  If MAP has an inverse, we can use MAP directly.
               IF ( OTOMAP .EQ. AST__NULL ) THEN
                  PPOS( 1, JAXIS ) = RANGES( I + 1 )
                  PPOS( 2, JAXIS ) = RANGES( I )

                  CALL AST_TRANN( MAP, 2, NDIM, 2, PPOS, .FALSE., NAXC,
     :                            2, CPOS, STATUS )
                  DRANGE( I + 1 ) = CPOS( 1, IAXIS )
                  DRANGE( I ) = CPOS( 2, IAXIS )

*  Otherwise, we use the single-input single-output Mapping that
*  generates the requested WCS axis.
               ELSE
                  PRANGE( I ) = DBLE( RANGES( I ) )
                  PRANGE( I + 1 ) = DBLE( RANGES( I + 1 ) )
                  CALL AST_TRAN1( OTOMAP, 2, PRANGE( I ), .FALSE.,
     :                            DRANGE( I ), STATUS )

               END IF
            END DO

            CALL MSG_SETI( 'I', JAXIS )
            ATTR = 'UNIT('
            LATTR = 5
            CALL CHR_PUTI( IAXIS, ATTR, LATTR )
            CALL CHR_APPND( ')', ATTR, LATTR )

            CALL MSG_OUT( ' ',
     :                    '   Fitting NDF Axis ^I, using ranges in '/
     :                    /'current co-ordinate Frame (pixels):',
     :                    STATUS )

            DO I = 1, NRANGE, 2

*  Get the Unit value.
               CALL MSG_SETC( 'UNIT', AST_GETC( CFRM, ATTR( : LATTR ),
     :                        STATUS ) )

               CALL MSG_SETI( 'L', RANGES( I ) )
               CALL MSG_SETI( 'H', RANGES( I + 1 ) )
               CALL MSG_SETC( 'LW', AST_FORMAT( CFRM, IAXIS,
     :                        DRANGE( I ), STATUS ) )
               CALL MSG_SETC( 'HW', AST_FORMAT( CFRM, IAXIS,
     :                        DRANGE( I + 1 ), STATUS ) )

               CALL MSG_OUT( ' ', '      ^LW : ^HW ^UNIT  (^L : ^H) ',
     :                       STATUS )
            END DO

*  There are only pixel co-ordinates.
         ELSE
            CALL MSG_SETI( 'I', JAXIS )
            CALL MSG_OUT( ' ',
     :                    '   Fitting NDF Axis ^I, using pixel ranges:',
     :                    STATUS )
            DO I = 1, NRANGE, 2
               CALL MSG_SETI( 'L', RANGES( I ) )
               CALL MSG_SETI( 'H', RANGES( I + 1 ) )
               CALL MSG_OUT( ' ', '      ^L : ^H ', STATUS )
            END DO
         END IF
         CALL MSG_BLANK( STATUS )
      END IF

*  Convert ranges into indices of the NDF data arrays by correcting for
*  the origin.
      DO I = 1, NRANGE
         RANGES( I ) = RANGES( I ) - LBND( JAXIS ) + 1
      END DO

*  Obtain knot locations.
*  ======================

*  These need to be in grid co-ordinates for the spline-fitting routine.
      IF ( FITYPE( 1:3 ) .EQ. 'SPL' ) THEN

*  Default to undefined knot positions.  Negative is unphysical.
         DO I = 1, NKNOT
            FKNOT( I ) = -1.0
         END DO

         IF ( INTERP ) THEN

*  Get the ranges to use.  These values are transformed from current
*  co-ordinates along the fit axis to pixel co-ordinates on the relevant
*  NDF axis.
            DKNOTS( 1 ) = AST__BAD
            CALL KPG1_GTAXV( 'POSKNOT', NKNOT, .TRUE., CFRM, IAXIS,
     :                       DKNOTS, NGOOD, STATUS )

*  If a null value was supplied then we should use equally spaced
*  knots.  The default remains in force.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )

*  If MAP has an inverse, we can use MAP directly to obtain pixel
*  co-ordinates.
            ELSE IF ( OTOMAP .EQ. AST__NULL ) THEN
               NKNOT = NGOOD

*  Project the given ranges into pixel co-ordinates.  Transform one at
*  a time to reuse the CPOS array.
               DO I = 1, NKNOT
                  CPOS( 1, IAXIS ) = DKNOTS( I )
                  CALL AST_TRANN( MAP, 1, NAXC, 2, CPOS, .TRUE., NDIM,
     :                            2, PPOS, STATUS )

*  Convert to a grid co-ordinate within the allowed range.
                  FKNOT( I ) = REAL( PPOS( 1, JAXIS ) ) -
     :                         REAL( LBND( JAXIS ) ) + 1.5
                  FKNOT( I ) = MAX( 1.0, MIN( REAL( DIMS( JAXIS ) ),
     :                                        FKNOT( I ) ) )
               END DO

*  Otherwise, we use the single-input single-output Mapping that
*  generates the requested WCS axis.
            ELSE
               CALL AST_TRAN1( OTOMAP, NKNOT, DKNOTS, .TRUE., PKNOTS,
     :                         STATUS )

*  Convert to grid co-ordinates within the allowed range.
               DO I = 1, NKNOT
                  FKNOT( I ) = REAL( PKNOTS( I ) ) -
     :                         REAL( LBND( JAXIS ) ) + 1.5
                  FKNOT( I ) = MAX( 1.0, MIN( REAL( DIMS( JAXIS ) ),
     :                                        FKNOT( I ) ) )
               END DO
            END IF
            CALL MSG_OUTIF ( MSG__VERB, 'MFITTREND_KNOTPOS1',
     :                       'Knot positions in grid co-ordinates:',
     :                       STATUS )
            FMT = '('
            NCF = 1
            CALL CHR_PUTI( UPKNOT, FMT, NCF )
            CALL CHR_APPND( '(1X,F9.2))', FMT, NCF )
            WRITE( TOKEN, FMT=FMT ) ( FKNOT( I ), I = 1, NKNOT )
            CALL MSG_OUTIF ( MSG__VERB, 'MFITTREND_KNOTPOS2',
     :                       TOKEN, STATUS )
         END IF
      END IF

*  Create output NDF.
*  ==================

*  If needed create a new output NDF based on the input NDF.
      IF ( SUBTRA ) THEN
         IF ( .NOT. MODIN ) THEN

*  Propagate all components except data and variance to the new NDF.
            CALL LPG_PROP( INNDF, 'Quality,Units,Label,Axis,WCS',
     :                     'OUT', OUTNDF, STATUS )
         END IF
      ELSE

*  Will write evals to a new NDF. Don't propagate quality as this is
*  model data now. Note we will also not propagate the variance.
         CALL LPG_PROP( INNDF, 'Units,Label,Axis,WCS', 'OUT', OUTNDF,
     :                  STATUS )
      END IF

*  Determine if the input NDF has an explicit no bad pixels flag. Could
*  make the check really check if there's no variances as this speeds
*  the calculations, but should let the user control that.
      CALL NDF_BAD( INNDF, 'Data', .FALSE., HASBAD, STATUS )

*  Get the data type.
      CALL NDF_TYPE( INNDF, 'DATA', ITYPE, STATUS )

*  Create the mask for the Global method.
*  ======================================
      FILMSK = .FALSE.
      IF ( SINGLE ) THEN

*  For the global option the whole input NDF is required not blocks, so
*  it must be formed outside the blocking loop.

*  Make workspace to store a byte mask (to reduce storage required).
         IF ( GLOBAL ) THEN
            CALL PSX_CALLOC( EL, '_BYTE', IPMASK, STATUS )

*  Check that we obtained the workspace before attempting to use it.
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Form ranges by averaging the lines in the section, and then
*  performing a fit, and rejecting outliers.
            CALL KPS1_MFSB( INNDF, JAXIS, NCLIP, CLIP, FOREST, NUMBIN,
     :                      GLOBAL, %VAL( CNF_PVAL( IPMASK ) ), STATUS )

*  For the global option, blocks of the mask need to be accessed in the
*  data-blocking loop.  An alternative storage is required in the shape
*  of a temporary NDF that can be blocked too.
            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_NEW( '_BYTE', NDIM, LBND, UBND, PLACE, TMPNDF,
     :                    STATUS )

*  Copy a global-sigma-based mask array to the temporary NDF.
            CALL KPG1_MAP( TMPNDF, 'Data', '_BYTE', 'WRITE', IPMN,
     :                     EL, STATUS )
            CALL KPG1_COPY( '_BYTE', EL, IPMASK, IPMN, STATUS )

*  Free resources.
            CALL NDF_UNMAP( TMPNDF, 'Data', STATUS )
            CALL PSX_FREE( IPMASK, STATUS )
         END IF

*  Create mask NDF.
*  ================

*  Start a new error context.
         CALL ERR_MARK

*  Write the mask to a new NDF.  We do not require quality and variance.
         CALL LPG_PROP( INNDF, 'Units,Label,Axis,WCS', 'MASK',
     :                  MSKNDF, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_RLSE
            GO TO 999

         ELSE

*  The mask file successfully created.
            FILMSK = .TRUE.

*  Set the data type.
            CALL NDF_STYPE( '_BYTE', MSKNDF, 'Data', STATUS )

*  Create a title.  The mask (in the DATA component) is written inside
*  the blocking loop.
            CALL NDF_CPUT( 'KAPPA - Mfittrend - Mask', MSKNDF, 'TITLE',
     :                     STATUS )
         END IF

*  Release the error context.
         CALL ERR_RLSE
      END IF

*  Process in blocks.
*  ==================

*  For large datasets, there may be insufficient memory.  Therefore
*  we form blocks to process, one at a time.  For this by definition
*  we need the trend-fitting-axis pixels to always be present in full
*  for each pixel along the other pixel axes.  If this leaves room for
*  a full span of a dimension that becomes the block size along that
*  axis.  Partial fills take the remaining maximum size and subsequent
*  dimensions' block sizes are unity.
      MBDIMS( JAXIS ) = DIMS( JAXIS )
      CALL KPG_TYPSZ( ITYPE, BPV, STATUS )
      MAXSIZ = MAX( 1, MAXBIB / DIMS( JAXIS ) / BPV )
      LOOP = .TRUE.
      DO I = 1, NDIM
         IF ( I .NE. JAXIS ) THEN
            IF ( LOOP ) THEN
               D = UBND( I ) - LBND( I ) + 1
               IF ( MAXSIZ .GE. D ) THEN
                  MBDIMS( I ) = D
                  MAXSIZ = MAXSIZ / D
               ELSE
                  MBDIMS( I ) = MAXSIZ
                  LOOP = .FALSE.
               END IF
            ELSE
               MBDIMS( I ) = 1
            END IF
         END IF
      END DO

*  Determine the number of blocks.
      CALL NDF_NBLOC( INNDF, NDIM, MBDIMS, NBLOCK, STATUS )

*  Loop through each block.  Start a new NDF context for each block.
      DO IBLOCK = 1, NBLOCK
         CALL NDF_BEGIN
         CALL NDF_BLOCK( INNDF, NDIM, MBDIMS, IBLOCK, IBL, STATUS )
         CALL NDF_BLOCK( OUTNDF, NDIM, MBDIMS, IBLOCK, OBL, STATUS )
         IF ( GLOBAL )
     :     CALL NDF_BLOCK( TMPNDF, NDIM, MBDIMS, IBLOCK, TBL, STATUS )
         IF ( FILMSK )
     :     CALL NDF_BLOCK( MSKNDF, NDIM, MBDIMS, IBLOCK, MBL, STATUS )

*  Find the dimensions of the block, as they may be smaller than the
*  maximum.
         CALL NDF_DIM( IBL, NDF__MXDIM, BLDIMS, NDIM, STATUS )

*  Map the data.
*  =============

*  Note we transfer the data component from the input NDF to the output
*  NDF, as this saves on an unmap by HDS followed by a map by us (if we
*  allowed this to propagate).
         IF ( SUBTRA ) THEN
            IF ( .NOT. MODIN ) THEN
               CALL NDF_MAP( IBL, 'DATA', ITYPE, 'READ', IPTMP, EL,
     :                       STATUS )
               CALL NDF_MAP( OBL, 'DATA', ITYPE, 'WRITE', IPDAT, EL,
     :                       STATUS )

*  Copy data to the output NDF.
               CALL KPG1_COPY( ITYPE, EL, IPTMP( 1 ), IPDAT( 1 ),
     :                         STATUS )
               CALL NDF_UNMAP( IBL, 'DATA', STATUS )

*  Same for variances.
               IF ( USEVAR ) THEN
                  CALL NDF_MAP( IBL, 'VARIANCE', ITYPE, 'READ', IPTMP,
     :                          EL, STATUS )
                  CALL NDF_MAP( OBL, 'VARIANCE', ITYPE, 'WRITE', IPVAR,
     :                          EL, STATUS )
                  CALL KPG1_COPY( ITYPE, EL, IPTMP( 1 ), IPVAR( 1 ),
     :                            STATUS )
                  CALL NDF_UNMAP( IBL, 'VARIANCE', STATUS )
               END IF
            ELSE

*  Subtracting from the input DATA, just map that in update mode.
               CALL NDF_MAP( IBL, 'DATA', ITYPE, 'UPDATE', IPDAT, EL,
     :                       STATUS )
               IF ( USEVAR ) THEN
                  CALL NDF_MAP( IBL, 'VARIANCE', ITYPE, 'UPDATE', IPVAR,
     :                          EL, STATUS )
               END IF
            END IF
         ELSE

*  No need to copy input data, will just populate output NDF data
*  component with model values.
            CALL NDF_MAP( IBL, 'DATA', ITYPE, 'READ', IPDAT, EL,
     :                    STATUS )
            IF ( USEVAR ) THEN
               CALL NDF_MAP( IBL, 'VARIANCE', ITYPE, 'READ', IPVAR, EL,
     :                       STATUS )
            END IF
         END IF
         IF ( .NOT. USEVAR ) IPVAR( 1 ) = IPDAT( 1 )

*  Allocate various workspaces.
*  ============================

*  The requirements for the workspaces depends on the dimensionality.
         AREA = 1
         DO 5 I = 1, NDIM
            IF ( I .NE. JAXIS ) THEN
               AREA = AREA * BLDIMS( I )
            END IF
   5     CONTINUE

*  Polynomial
*  ----------
         IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN

*  We need space for the cumulative coefficient sums and the
*  coefficients themselves (Ax=B).
            IF ( USEVAR .OR. HASBAD .OR. SINGLE ) THEN
               CALL PSX_CALLOC( AREA * ( ORDER + 1 ) * ( ORDER + 1 ),
     :                         '_DOUBLE', IPAS, STATUS )
            ELSE

*  When there are no variances and we also know there are no BAD values
*  useful savings in memory and speed are available as the cumulative
*  sums for matrix inversion are fixed.
               CALL PSX_CALLOC( ( ORDER + 1 ) * ( ORDER + 1 ),
     :                          '_DOUBLE', IPAS, STATUS )
            END IF
            CALL PSX_CALLOC( AREA * ( ORDER + 1 ), '_DOUBLE', IPBS,
     :                       STATUS )
            CALL PSX_CALLOC( ORDER + 1 , '_DOUBLE', IPWRK1, STATUS )
            CALL PSX_CALLOC( ORDER + 1, '_INTEGER', IPWRK2, STATUS )

*  Spline
*  ------
         ELSE

*  Calculate the total number of pixels to fit and the storage
*  requirement for the spline-fitting routine.  We add a couple of
*  clamps.
            DSIZE = AREA * ( DIMS( JAXIS ) + 2 )
            IF ( INTERP ) THEN
               MXKNOT = MIN( NKNOT, DIMS( JAXIS ) / 2 ) + 8
            ELSE

*  This is a guesstimate.  We do not want to fit every wiggle.
               MXKNOT = MIN( 192, MAX( NKNOT, DIMS( JAXIS ) / 2 ) ) + 8
            END IF

*  Map some workspace to hold the co-ordinates, values and weights.  The
*  maximum number of values which may be required is DSIZE, though the
*  presence of bad values may mean that not all this workspace is
*  needed.
            CALL PSX_CALLOC( DSIZE, '_REAL', IPCO, STATUS )
            CALL PSX_CALLOC( DSIZE, '_REAL', IPVAL, STATUS )
            CALL PSX_CALLOC( DSIZE, '_REAL', IPWT, STATUS )

*  Map some workspace for the coefficients, knots, scale factors,
*  number of good values.
            NWS = MXKNOT * AREA
            CALL PSX_CALLOC( AREA, '_INTEGER', IPGOOD, STATUS )
            CALL PSX_CALLOC( AREA, '_INTEGER', IPNC, STATUS )
            CALL PSX_CALLOC( NWS, '_REAL', IPCOEF, STATUS )
            CALL PSX_CALLOC( NWS, '_REAL', IPKNOT, STATUS )
            CALL PSX_CALLOC( AREA, '_REAL', IPSCAL, STATUS )

         END IF

*  Make workspace to store a byte mask (to reduce storage required).
         IF ( SINGLE ) THEN
            CALL PSX_CALLOC( EL, '_BYTE', IPMASK, STATUS )

*  Create a valid pointer for KPS1_LFTx and KPS1_MFRMx calls.
         ELSE
            IPMASK = IPDAT( 1 )
         END IF

*  Check that we obtained the workspace before attempting to use it.
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create the mask for Single and Global methods.
*  ==============================================
         IF ( SINGLE ) THEN

*  Form ranges by averaging the lines in the section, and then
*  performing a fit, and rejecting outliers that define the output
*  mask array.
            IF ( .NOT. GLOBAL ) THEN
               CALL KPS1_MFSB( IBL, JAXIS, NCLIP, CLIP, FOREST, NUMBIN,
     :                         GLOBAL, %VAL( CNF_PVAL( IPMASK ) ),
     :                         STATUS )
            ELSE

*  Copy the mask array block from the previously created temporary NDF
*  of the whole mask.
               CALL KPG1_MAP( TBL, 'Data', '_BYTE', 'READ', IPMN,
     :                        EL, STATUS )
               CALL KPG1_COPY( '_BYTE', EL, IPMN, IPMASK, STATUS )

*  We are done with the temporary-NDF block.  This will unmap too.
               CALL NDF_ANNUL( TBL, STATUS )
            END IF

*  Did the user request to save a mask and was it created successfully?
            IF ( FILMSK ) THEN

*  Copy the mask array to the created NDF.
               CALL KPG1_MAP( MBL, 'Data', '_BYTE', 'WRITE', IPMN,
     :                        EL, STATUS )
               CALL KPG1_COPY( '_BYTE', EL, IPMASK, IPMN, STATUS )

*  We are done with the mask NDF block.  This will unmap too.
               CALL NDF_ANNUL( MBL, STATUS )
            END IF

         END IF

*  Determine the fits.
*  ===================
         QUICK = .NOT. ( USEVAR .OR. HASBAD .OR. SINGLE )
         IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN

*  N.B. could reduce memory use by NDF blocking though planes for
*  higher dimensional data, or just mapping the intersection of ranges,
*  or individual ranges, but that would only be good if working with the
*  last axis (need contiguity).  Use the wrapper routine to make this
*  code more compact.
            CALL KPS1_LFTA( QUICK, ITYPE, ORDER, JAXIS, NRANGE, RANGES,
     :                      USEVAR, IPVAR( 1 ), SINGLE,
     :                      %VAL( CNF_PVAL( IPMASK ) ), BLDIMS,
     :                      IPDAT( 1 ), %VAL( CNF_PVAL( IPAS ) ),
     :                      %VAL( CNF_PVAL( IPBS ) ),
     :                      %VAL( CNF_PVAL( IPWRK1 ) ),
     :                      %VAL( CNF_PVAL( IPWRK2 ) ),
     :                      STATUS )

*  Free up the workspace at the earliest opportunity.
            CALL PSX_FREE( IPAS, STATUS )
            CALL PSX_FREE( IPWRK1, STATUS )
            CALL PSX_FREE( IPWRK2, STATUS )

*  Fit a B-spline to each trend vector.
         ELSE
            CALL KPS1_MFTA( QUICK, ITYPE, INTERP, MXKNOT, NKNOT, FKNOT,
     :                      JAXIS, NRANGE, RANGES, USEVAR, IPVAR( 1 ),
     :                      SINGLE, %VAL( CNF_PVAL( IPMASK ) ), BLDIMS,
     :                      IPDAT( 1 ), %VAL( CNF_PVAL( IPCO ) ),
     :                      %VAL( CNF_PVAL( IPVAL ) ),
     :                      %VAL( CNF_PVAL( IPWT ) ), NFIT,
     :                      %VAL( CNF_PVAL( IPGOOD ) ),
     :                      %VAL( CNF_PVAL( IPKNOT ) ),
     :                      %VAL( CNF_PVAL( IPCOEF ) ),
     :                      %VAL( CNF_PVAL( IPNC ) ),
     :                      %VAL( CNF_PVAL( IPSCAL ) ), STATUS )

*  Free up the workspace at the earliest opportunity.
            CALL PSX_FREE( IPGOOD, STATUS )
            CALL PSX_FREE( IPCO, STATUS )
            CALL PSX_FREE( IPVAL, STATUS )
            CALL PSX_FREE( IPWT, STATUS )

*  Report the success rate.
            CALL MSG_SETI( 'NF', NFIT )
            CALL MSG_SETI( 'AREA' , AREA )
            CALL MSG_OUTIF( MSG__NORM, 'MFITTREND_NFIT', 'Successful '/
     :                      /'spline fits to ^NF of ^AREA trends.',
     :                      STATUS )
         END IF

*  Evaluate and optionally subtract the trends.
*  ============================================

*  Subtract the result from the NDF data or write the evaluated fit.
*  If evaluating, then we need to map in the data component, may as
*  well release the input one to save VM.  Note that if residual reject
*  of points is requested that the IPDAT pointer would be overloaded.
*  So retain the original pointer for the subtraction forming residuals.
*  Use the masked array for the Single method.
         IF ( .NOT. SUBTRA ) THEN
            IF ( CLIPRE ) THEN
               IPIN = IPDAT( 1 )
            ELSE
               CALL NDF_UNMAP( IBL, 'DATA', STATUS )
            END IF

            IF ( USEVAR ) THEN
               CALL NDF_UNMAP( IBL, 'VARIANCE', STATUS )
            END IF
            CALL NDF_MAP( OBL, 'DATA', ITYPE, 'WRITE', IPDAT,
     :                    EL, STATUS )
         END IF

         IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN
            IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPS1_LFTSB( ORDER, JAXIS, SUBTRA,
     :                          BLDIMS, %VAL( CNF_PVAL( IPBS ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               CALL KPS1_LFTSUB( ORDER, JAXIS, SUBTRA,
     :                           BLDIMS, %VAL( CNF_PVAL( IPBS ) ),
     :                           %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                           STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_LFTSD( ORDER, JAXIS, SUBTRA,
     :                          BLDIMS, %VAL( CNF_PVAL( IPBS ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPS1_LFTSI( ORDER, JAXIS, SUBTRA,
     :                          BLDIMS, %VAL( CNF_PVAL( IPBS ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
               CALL KPS1_LFTSK( ORDER, JAXIS, SUBTRA,
     :                          BLDIMS, %VAL( CNF_PVAL( IPBS ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_LFTSR( ORDER, JAXIS, SUBTRA,
     :                          BLDIMS, %VAL( CNF_PVAL( IPBS ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL KPS1_LFTSW( ORDER, JAXIS, SUBTRA,
     :                          BLDIMS, %VAL( CNF_PVAL( IPBS ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               CALL KPS1_LFTSUW( ORDER, JAXIS, SUBTRA,
     :                           BLDIMS, %VAL( CNF_PVAL( IPBS ) ),
     :                           %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                           STATUS )
            END IF

*  Free up the remaining workspace at the earliest opportunity.
            CALL PSX_FREE( IPBS, STATUS )

         ELSE

            IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPS1_MFTSB( JAXIS, SUBTRA, BLDIMS, MXKNOT,
     :                          %VAL( CNF_PVAL( IPNC ) ),
     :                          %VAL( CNF_PVAL( IPKNOT ) ),
     :                          %VAL( CNF_PVAL( IPCOEF ) ),
     :                          %VAL( CNF_PVAL( IPSCAL ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               CALL KPS1_MFTSUB( JAXIS, SUBTRA, BLDIMS, MXKNOT,
     :                           %VAL( CNF_PVAL( IPNC ) ),
     :                           %VAL( CNF_PVAL( IPKNOT ) ),
     :                           %VAL( CNF_PVAL( IPCOEF ) ),
     :                           %VAL( CNF_PVAL( IPSCAL ) ),
     :                           %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                           STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_MFTSD( JAXIS, SUBTRA, BLDIMS, MXKNOT,
     :                          %VAL( CNF_PVAL( IPNC ) ),
     :                          %VAL( CNF_PVAL( IPKNOT ) ),
     :                          %VAL( CNF_PVAL( IPCOEF ) ),
     :                          %VAL( CNF_PVAL( IPSCAL ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPS1_MFTSI( JAXIS, SUBTRA, BLDIMS, MXKNOT,
     :                          %VAL( CNF_PVAL( IPNC ) ),
     :                          %VAL( CNF_PVAL( IPKNOT ) ),
     :                          %VAL( CNF_PVAL( IPCOEF ) ),
     :                          %VAL( CNF_PVAL( IPSCAL ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
               CALL KPS1_MFTSK( JAXIS, SUBTRA, BLDIMS, MXKNOT,
     :                          %VAL( CNF_PVAL( IPNC ) ),
     :                          %VAL( CNF_PVAL( IPKNOT ) ),
     :                          %VAL( CNF_PVAL( IPCOEF ) ),
     :                          %VAL( CNF_PVAL( IPSCAL ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_MFTSR( JAXIS, SUBTRA, BLDIMS, MXKNOT,
     :                          %VAL( CNF_PVAL( IPNC ) ),
     :                          %VAL( CNF_PVAL( IPKNOT ) ),
     :                          %VAL( CNF_PVAL( IPCOEF ) ),
     :                          %VAL( CNF_PVAL( IPSCAL ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL KPS1_MFTSW( JAXIS, SUBTRA, BLDIMS, MXKNOT,
     :                          %VAL( CNF_PVAL( IPNC ) ),
     :                          %VAL( CNF_PVAL( IPKNOT ) ),
     :                          %VAL( CNF_PVAL( IPCOEF ) ),
     :                          %VAL( CNF_PVAL( IPSCAL ) ),
     :                          %VAL( CNF_PVAL( IPDAT( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               CALL KPS1_MFTSUW( JAXIS, SUBTRA, BLDIMS, MXKNOT,
     :                           %VAL( CNF_PVAL( IPNC ) ),
     :                           %VAL( CNF_PVAL( IPKNOT ) ),
     :                           %VAL( CNF_PVAL( IPCOEF ) ),
     :                           %VAL( CNF_PVAL( IPSCAL ) ),
     :                           %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                           STATUS )
            END IF

* Free up the remaining workspace at the earliest opportunity.
            CALL PSX_FREE( IPNC, STATUS )
            CALL PSX_FREE( IPKNOT, STATUS )
            CALL PSX_FREE( IPCOEF, STATUS )
            CALL PSX_FREE( IPSCAL, STATUS )

         END IF

*  Form residuals.
*  ===============

*  If no de-trended data calculated, i.e. the residuals, need to form
*  the residuals to reject their outliers.  Used the masked array for
*  the Single method.
         IF ( CLIPRE ) THEN
            IF ( .NOT. SUBTRA ) THEN
               CALL PSX_CALLOC( EL, ITYPE, IPRES, STATUS )

*  Select the appropriate routine for the data type being processed and
*  subtract the data arrays.
               IF ( ITYPE .EQ. '_BYTE' ) THEN
                  CALL VEC_SUBB( HASBAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                           %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                           %VAL( CNF_PVAL( IPRES ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                  CALL VEC_SUBUB( HASBAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                            %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                            %VAL( CNF_PVAL( IPRES ) ),
     :                            IERR, NERR, STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL VEC_SUBD( HASBAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                           %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                           %VAL( CNF_PVAL( IPRES ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                  CALL VEC_SUBI( HASBAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                           %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                           %VAL( CNF_PVAL( IPRES ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL VEC_SUBR( HASBAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                           %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                           %VAL( CNF_PVAL( IPRES ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                  CALL VEC_SUBW( HASBAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                           %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                           %VAL( CNF_PVAL( IPRES ) ),
     :                           IERR, NERR, STATUS )

               ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                  CALL VEC_SUBUW( HASBAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                            %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                            %VAL( CNF_PVAL( IPRES ) ),
     :                            IERR, NERR, STATUS )
               END IF

*  Use the existing residuals map.
            ELSE
               IPRES = IPDAT( 1 )
            END IF

*  Obtain workspace for the JAXIS collapsed array.
            CALL PSX_CALLOC( AREA, '_DOUBLE', IPCOL, STATUS )
            CALL PSX_CALLOC( AREA, '_INTEGER', IPWRK3, STATUS )

*  Derive the the rms of the residuals in the fitting ranges.
            IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPS1_MFRMB( JAXIS, NRANGE, RANGES, CLPRMS, BLDIMS,
     :                          SINGLE, %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPRES ) ),
     :                          %VAL( CNF_PVAL( IPCOL ) ),
     :                          %VAL( CNF_PVAL( IPWRK3 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               CALL KPS1_MFRMUB( JAXIS, NRANGE, RANGES, CLPRMS, BLDIMS,
     :                           SINGLE, %VAL( CNF_PVAL( IPMASK ) ),
     :                           %VAL( CNF_PVAL( IPRES ) ),
     :                           %VAL( CNF_PVAL( IPCOL ) ),
     :                           %VAL( CNF_PVAL( IPWRK3 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_MFRMD( JAXIS, NRANGE, RANGES, CLPRMS, BLDIMS,
     :                          SINGLE, %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPRES ) ),
     :                          %VAL( CNF_PVAL( IPCOL ) ),
     :                          %VAL( CNF_PVAL( IPWRK3 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPS1_MFRMI( JAXIS, NRANGE, RANGES, CLPRMS, BLDIMS,
     :                          SINGLE, %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPRES ) ),
     :                          %VAL( CNF_PVAL( IPCOL ) ),
     :                          %VAL( CNF_PVAL( IPWRK3 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_MFRMR( JAXIS, NRANGE, RANGES, CLPRMS, BLDIMS,
     :                          SINGLE, %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPRES ) ),
     :                          %VAL( CNF_PVAL( IPCOL ) ),
     :                          %VAL( CNF_PVAL( IPWRK3 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL KPS1_MFRMW( JAXIS, NRANGE, RANGES, CLPRMS, BLDIMS,
     :                          SINGLE, %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPRES ) ),
     :                          %VAL( CNF_PVAL( IPCOL ) ),
     :                          %VAL( CNF_PVAL( IPWRK3 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               CALL KPS1_MFRMUW( JAXIS, NRANGE, RANGES, CLPRMS, BLDIMS,
     :                           SINGLE, %VAL( CNF_PVAL( IPMASK ) ),
     :                           %VAL( CNF_PVAL( IPRES ) ),
     :                           %VAL( CNF_PVAL( IPCOL ) ),
     :                           %VAL( CNF_PVAL( IPWRK3 ) ), STATUS )
            END IF

            CALL PSX_FREE( IPCOL, STATUS )
            CALL PSX_FREE( IPWRK3, STATUS )

*  If required, propagate the bad values to the output.  Recall that if the
*  data were already de-trended then the array supplied to and amended in
*  KPS1_MFRM is the output array.
            IF ( PRPBAD .AND. .NOT. SUBTRA ) THEN

               IF ( ITYPE .EQ. '_BYTE' ) THEN
                  CALL KPG1_CPBDB( EL, %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                             %VAL( CNF_PVAL( IPRES ) ),
     :                             %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                  CALL KPG1_CPBDUB( EL, %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                              %VAL( CNF_PVAL( IPRES ) ),
     :                              %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                              STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_CPBDD( EL, %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                             %VAL( CNF_PVAL( IPRES ) ),
     :                             %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                  CALL KPG1_CPBDI( EL, %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                             %VAL( CNF_PVAL( IPRES ) ),
     :                             %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_CPBDR( EL, %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                             %VAL( CNF_PVAL( IPRES ) ),
     :                             %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                  CALL KPG1_CPBDW( EL, %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                             %VAL( CNF_PVAL( IPRES ) ),
     :                             %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                             STATUS )

               ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                  CALL KPG1_CPBDUW( EL, %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                              %VAL( CNF_PVAL( IPRES ) ),
     :                              %VAL( CNF_PVAL( IPDAT( 1 ) ) ),
     :                              STATUS )
               END IF

*  Tidy.
               CALL PSX_FREE( IPRES, STATUS )
            END IF
         END IF

*  Tidy the mask block.
         IF ( SINGLE ) CALL PSX_FREE( IPMASK, STATUS )

*   Close NDF context at the end of the blocking loop.
         CALL NDF_END( STATUS )
      END DO

*  Free mask NDFs.
      IF ( FILMSK ) CALL NDF_ANNUL( MSKNDF, STATUS )
      IF ( GLOBAL ) CALL NDF_ANNUL( TMPNDF, STATUS )

*  Obtain the output title and insert it into the result NDF.
      IF ( MODIN ) THEN
         CALL NDF_CINP( 'TITLE', INNDF, 'TITLE', STATUS )
      ELSE
         CALL NDF_CINP( 'TITLE', OUTNDF, 'TITLE', STATUS )
      END IF

*  Exit in error label, tidyup after this point.
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MFITTREND_ERR',
     :                 'MFITTREND: Error determining trends',
     :                 STATUS )
      END IF

      END
