      SUBROUTINE COLLAPSE( STATUS )
*+
*  Name:
*     COLLAPSE

*  Purpose:
*     Reduce the number of axes in an N-dimensional NDF by compressing
*     it along a nominated axis.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COLLAPSE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application collapses a nominated pixel axis of an
*     N-dimensional NDF, producing an output NDF with one fewer pixel
*     axes than the input NDF.  A specified range of axis values can
*     be used instead of the whole axis (see Parameters LOW and HIGH).
*
*     For each output pixel, all corresponding input pixel values
*     between the specified bounds of the nominated axis to be
*     collapsed are combined together using one of a selection of
*     estimators, including a mean, mode, or median, to produce the
*     output pixel value.
*
*     Possible uses include such things as collapsing a range of
*     wavelength planes in a three-dimensional RA/DEC/Wavelength cube to
*     produce a single two-dimensional RA/DEC image, or collapsing a
*     range of slit positions in a two-dimensional slit
*     position/wavelength image to produce a one-dimensional wavelength
*     array.

*  Usage:
*     collapse in out axis [low] [high] [estimator] [wlim]

*  ADAM Parameters:
*     AXIS = LITERAL (Read)
*        The axis along which to collapse the NDF.  This can be
*        specified using one of the following options.
*
*        - Its integer index within the current Frame of the input
*        NDF (in the range 1 to the number of axes in the current
*        Frame).
*        - Its symbol string such as "RA" or "VRAD".
*        - A generic option where "SPEC" requests the spectral axis,
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*        sky longitude and latitude axes respectively.  Only those axis
*        domains present are available as options.
*
*        A list of acceptable values is displayed if an illegal value is
*        supplied.  If the axes of the current Frame are not parallel to
*        the NDF pixel axes, then the pixel axis which is most nearly
*        parallel to the specified current Frame axis will be used.
*     CLIP = _REAL (Read)
*        The number of standard deviations about the mean at which to
*        clip outliers for the "Mode", "Cmean" and "Csigma" statistics
*        (see Parameter ESTIMATOR).  The application first computes
*        statistics using all the available pixels.  It then rejects
*        all those pixels whose values lie beyond CLIP standard
*        deviations from the mean and will then re-evaluate the
*        statistics.   For "Cmean" and "Csigma" there is currently only
*        one iteration , but up to seven for "Mode".

*        The value must be positive.  [3.0]
*     COMP = LITERAL (Read)
*        The name of the NDF array component for which statistics are
*        required: "Data", "Error", "Quality" or "Variance" (where
*        "Error" is the alternative to "Variance" and causes the square
*        root of the variance values to be taken before computing the
*        statistics).  If "Quality" is specified, then the quality
*        values are treated as numerical values (in the range 0 to
*        255).  ["Data"]
*     ESTIMATOR = LITERAL (Read)
*        The method to use for estimating the output pixel values.  It
*        can be one of the following options.  The first five are
*        more for general collapsing, and the remainder are for cube
*        analysis.
*          "Mean"   -- Mean value
*          "WMean"  -- Weighted mean in which each data value is
*                      weighted by the reciprocal of the associated
*                      variance (not available for COMP="Variance" or
*                      "Error").
*          "Mode"   -- Modal value
*          "Median" -- Median value.  Note that this is extremely memory
*                      and CPU intensive for large datasets; use with
*                      care!  If strange things happen, use "Mean" or
*                      try "FastMed".
*          "FastMed"-- Faster median using Wirth's algorithm for selecting
*                      the kth value, rather than a full sort.
*                      Weighting is not supported, thus this option is
*                      unavailable if both Parameter VARIANCE is TRUE and
*                      the input NDF contains a VARIANCE component.
*
*          "Absdev" -- Mean absolute deviation from the unweighted mean.
*          "Cmean"  -- Sigma-clipped mean.
*          "Csigma" -- Sigma-clipped standard deviation.
*          "Comax"  -- Co-ordinate of the maximum value.
*          "Comin"  -- Co-ordinate of the minimum value.
*          "FBad"   -- Fraction of bad pixel values.
*          "FGood"  -- Fraction of good pixel values.
*          "Integ"  -- Integrated value, being the sum of the products
*                      of the value and pixel width in world
*                      co-ordinates.  Note that for sky co-ordinates the
*                      pixel width is measured in radians.
*          "Iwc"    -- Intensity-weighted co-ordinate, being the sum of
*                      each value times its co-ordinate, all divided by
*                      the integrated value (see the "Integ" option).
*          "Iwd"    -- Intensity-weighted dispersion of the
*                      co-ordinate, normalised like "Iwc" by the
*                      integrated value.
*          "Max"    -- Maximum value.
*          "Min"    -- Minimum value.
*          "NBad"   -- Number of bad pixel values.
*          "NGood"  -- Number of good pixel values.
*          "Rms"    -- Root-mean-square value.
*          "Sigma"  -- Standard deviation about the unweighted mean.
*          "Sum"    -- The total value.
*        ["Mean"]
*     HIGH = LITERAL (Read)
*        A formatted value for the axis specified by Parameter AXIS.
*        For example, if AXIS is 3 and the current Frame of the input
*        NDF has axes RA/DEC/Wavelength, then a wavelength value should
*        be supplied.  If, on the other hand, the current Frame in the
*        NDF was the PIXEL Frame, then a pixel co-ordinate value would
*        be required for the third axis (note, the pixel with index I
*        covers a range of pixel co-ordinates from (I-1) to I).
*        Together with Parameter LOW, this parameter gives the range of
*        axis values to be compressed.  Note, HIGH and LOW should not be
*        equal.  If a null value (!) is supplied for either HIGH or LOW,
*        the entire range of the axis is collapsed.  [!]
*     IN  = NDF (Read)
*        The input NDF.
*     LOW = LITERAL (Read)
*        A formatted value for the axis specified by Parameter AXIS.
*        For example, if AXIS is 3 and the current Frame of the input
*        NDF has axes RA/DEC/Wavelength, then a wavelength value should
*        be supplied.  If, on the other hand, the current Frame in the
*        NDF was the PIXEL Frame, then a pixel co-ordinate value would
*        be required for the third axis (note, the pixel with index I
*        covers a range of pixel co-ordinates from (I-1) to I).
*        Together with Parameter HIGH, this parameter gives the range of
*        axis values to be compressed.  Note, LOW and HIGH should not be
*        equal.  If a null value (!) is supplied for either LOW or HIGH,
*        the entire range of the axis is collapsed.  [!]
*     OUT = NDF (Write)
*        The output NDF.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF.  [!]
*     TRIM = _LOGICAL (Read)
*        This parameter controls whether the collapsed axis should be
*        removed from the co-ordinate syatems describing the output NDF.
*        If a TRUE value is supplied, the collapsed WCS axis will be
*        removed from the WCS FrameSet of the output NDF, and the
*        collapsed pixel axis will also be removed from the NDF,
*        resulting in the output NDF having one fewer pixel axes than
*        the input NDF.  If a FALSE value is supplied, the collapsed WCS
*        and pixel axes are retained in the output NDF, resulting in the
*        input and output NDFs having the same number of pixel axes.  In
*        this case, the pixel-index bounds of the collapse axis will be
*        set to (1:1) in the output NDF (that is, the output NDF will
*        span only a single pixel on the collapse axis).  Thus, setting
*        TRIM to FALSE allows information to be retained about the range
*        of values over which the collapse occurred.  [TRUE]
*     VARIANCE = _LOGICAL (Read)
*        A flag indicating whether a variance array present in the
*        NDF is used to weight data values while forming the estimator's
*        statistic, and to derive output variance.  If VARIANCE is TRUE
*        and the NDF contains a variance array, this array will be
*        used to define the weights, otherwise all the weights will be
*        set equal.  By definition this parameter is set to FALSE when
*        COMP is "Variance" or "Error".
*
*        The VARIANCE parameter is ignored and set to FALSE when there
*        are more than 300 pixels along the collapse axis and
*        ESTIMATOR is "Median", "Mode", "Cmean", or "Csigma".  This
*        prevents the covariance matrix from being huge.  For "Median"
*        estimates of variance come from mean variance instead.  The
*        other affected estimators switch to use equal weighting.
*        [TRUE]
*     WCSATTS = GROUP (Read)
*        A group of attribute settings which will be used to make
*        temporary changes to the properties of the current co-ordinate
*        Frame in the WCS  FrameSet before it is used.  Supplying a list
*        of attribute values for this parameter is equivalent to
*        invoking WCSATTRIB on the input NDF prior to running this
*        command, except that no permanent change is made to the NDF
*        (however the changes are propagated through to the output NDF).
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be read
*        and interpreted in the same manner.  Attribute settings are
*        applied in the order in which they occur within the list, with
*        later settings overriding any earlier settings given for the
*        same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a Frame attribute, and <value>
*        is the value to assign to the attribute.  Any unspecified
*        attributes will retain the value they have in the supplied NDF.
*        No attribute values will be changed if a null value (!) is
*        supplied.  Any unrecognised attributes are ignored (no error is
*        reported).  [!]
*     WLIM = _REAL (Read)
*        If the input NDF contains bad pixels, then this parameter
*        may be used to determine the number of good pixels which must
*        be present within the range of collapsed input pixels before a
*        valid output pixel is generated.  It can be used, for example,
*        to prevent output pixels from being generated in regions where
*        there are relatively few good pixels to contribute to the
*        collapsed result.
*
*        WLIM specifies the minimum fraction of good pixels which must
*        be present in order to generate a good output pixel.  If this
*        specified minimum fraction of good input pixels is not present,
*        then a bad output pixel will result, otherwise a good output
*        value will be calculated.  The value of this parameter should
*        lie between 0.0 and 1.0 (the actual number used will be rounded
*        up if necessary to correspond to at least one pixel). [0.3]

*  Examples:
*     collapse m31 profile axis=RA low="0:36:01" high="0:48:00"
*        Collapses the two-dimensional NDF called m31 along the
*        right-ascension axis, from "0:36:01" to "0:48:00", and puts the
*        result in an output NDF called profile.
*     collapse cube slab lambda 4500 4550
*        The current Frame in the input three-dimensional NDF called
*        cube has axes with labels "RA", "DEC" and "Lambda", with the
*        lambda axis being parallel to the third pixel axis.  The above
*        command extracts a slab of the input cube between wavelengths
*        4500 and 4550 Angstroms, and collapses this slab into a single
*        two-dimensional output NDF called slab with RA and DEC axes.
*        Each pixel in the output NDF is the mean of the corresponding
*        input pixels with wavelengths between 4500 and 4550 Angstroms.
*     collapse cube slab 3 4500 4550
*        The same as the previous example except the axis to collapse
*        along is specified by index (3) rather than label (lambda).
*     collapse cube slab 3 101.0 134.0
*        This is the same as the second example, except that the current
*        Frame in the input NDF has been set to the PIXEL Frame (using
*        WCSFRAME), and so the high and low axis values are specified in
*        pixel co-ordinates instead of Angstroms.  Note the difference
*        between floating-point pixel co-ordinates, and integer pixel
*        indices (for instance the pixel with index 10 extends from
*        pixel co-ordinate 9.0 to pixel co-ordinate 10.0).
*     collapse cube slab 3 low=99.0 high=100.0
*        This is the same as the second example, except that a single
*        pixel plane in the cube (pixel 100) is used to create the
*        output NDF.  Following the usual definition of pixel
*        co-ordinates, pixel 100 extends from pixel co-ordinate 99.0 to
*        pixel co-ordinate 100.0.  So the given HIGH and LOW values
*        encompass the single pixel plane at pixel 100.

*  Notes:
*     -  The collapse is always performed along one of the pixel axes,
*     even if the current Frame in the input NDF is not the PIXEL Frame.
*     Special care should be taken if the current-Frame axes are not
*     parallel to the pixel axes.  The algorithm used to choose the
*     pixel axis and the range of values to collapse along this pixel
*     axis proceeds as follows.
*
*     The current-Frame co-ordinates of the central pixel in the input
*     NDF are determined (or some other point if the co-ordinates of the
*     central pixel are undefined).  Two current-Frame positions are
*     then generated by substituting in turn into this central position
*     each of the HIGH and LOW values for the current-Frame axis
*     specified by Parameter AXIS.  These two current-Frame positions
*     are transformed into pixel co-ordinates, and the projections of
*     the vector joining these two pixel positions on to the pixel axes
*     are found.  The pixel axis with the largest projection is selected
*     as the collapse axis, and the two end points of the projection
*     define the range of axis values to collapse.
*     -  A warning is issued (at the normal reporting level) whenever
*     any output values are set bad because there are too few
*     contributing data values.  This reports the fraction of flagged
*     output data generated by the WLIM parameter's threshold.
*
*     No warning is given when Parameter WLIM=0.  Input data containing
*     only bad values are not counted in the flagged fraction, since no
*     potential good output value has been lost.

*  Related Applications:
*     KAPPA: WCSFRAME, COMPAVE, COMPICK, COMPADD.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     LABEL, TITLE, UNITS, WCS, and HISTORY components of the input NDF
*     and propagates all extensions.  QUALITY is not propagated.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.
*     -  Huge NDF are supported.

*  Copyright:
*     Copyright (C) 2000-2001, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2005-2006 Particle Physics & Astronomy
*     Research Council.  Copyright (C) 2007-2009, 2013, 2018 Science and
*     Technology Facilities Council.  All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-AUG-2000 (DSB):
*        Original version.
*     27-OCT-2000 (DSB):
*        Modified to avoid allocating unnecessary workspace if the last
*        axis is being collapsed.
*     14-DEC-2001 (DSB):
*        Renamed wieghted mean estimator as "Wmean" and added new
*        unweighted mean estimator "Mean".
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     2-DEC-2005 (DSB):
*        Move the code for creating the output WCS FrameSet into a KPS
*        routine.
*     2005 December 22 (MJC):
*        Add MAX and MIN estimators.
*     2005 December 24 (MJC):
*        Add ABSDEV estimator.
*     2005 December 27 (MJC):
*        Add SIGMA estimator.
*     2005 December 28 (MJC):
*        Add RMS estimator.
*     2005 December 29 (MJC):
*        Add SUM estimator.
*     2005 December 30 (MJC):
*        Add INTEG, COMAX, COMIN, IWC, IWD estimators.
*     2006 January 2 (MJC):
*        Obtain co-ordinates and pass co-ordinate array for some
*        estimators.
*     2006 January 5 (MJC):
*        Added section headings in comments.  Copied FrameSet for
*        estimators using co-ordinates to fix bug.
*     2006 January 6 (MJC):
*        Obtain axis widths for Integ, Iwc, and Iwd options.
*     2006 January 8 (MJC):
*        Initialise UNITS for NDF_CGET call for INTEG estimator.
*     2006 January 20 (MJC):
*        Remove STATUS from a CHR_APPND, and tidy the widths correctly.
*     2006 January 27 (MJC):
*        For Integ estimator obtain co-ordinates and create workspace
*        for widths instead of using AXIS-component widths.  Initialise
*        variance pointers.
*     9-FEB-2006 (DSB):
*        Report an error if the selected WCS axis has a constant value.
*     2006 February 10 (MJC):
*        Update the units for Iwc and Iwd estimators, as the incorrect
*        published formulae for these were dimensionless.
*     2006 February 13 (MJC):
*        Allow for very large datasets by blocking into manageable
*        sections.
*     2006 February 14 (MJC):
*        Correct calculation of the blocked bounds for the output NDF.
*     2006 March 3 (MJC):
*        Correct the bounds for blocking.  Use an NDF section to define
*        the number of blocks when the compression is derived from only
*        part of the collapse axis.
*     2006 April 11 (MJC):
*        Obtain bounds for each block passed to KPS1_CLPSx, rather
*        than the full array.  Do not use workspace if higher
*        dimensions are 1.
*     2006 June 23 (MJC):
*        Use a temporary NDF as intermediary to obtain file-size
*        compression of the output NDF.
*     2006 August 4 (MJC):
*        Fix bug propagating AXIS structure introduced from the
*        previous modification.  The revised NDF_PROP enables the
*        file-size compression, and thus the temporary NDF is no longer
*        required.
*     25-SEP-2006 (DSB):
*        Added WCSATTS parameter.
*     2006 December 13 (MJC):
*        Added VARIANCE parameter.
*     21-DEC-2006 (DSB):
*        Manage without an inverse WCS transformation, when possible.
*     2-MAR-2007 (DSB):
*        Pad out the current Frame in the output WCS FrameSet by
*        duplicating PIXEL axes so that the current Frame has at least
*        as many axes as the base (GRID) Frame.
*     5-MAR-2007 (DSB):
*        Added Parameter TRIM. Moved calculation of GRDPOS so that it
*        is available for both splitable Mappings and unsplitable
*        Mappings.
*     2007 April 3 (MJC):
*        Fix bug when the collapsed axis is retained in the WCS
*        FrameSet: it now uses the GRID co-ordinates of the limits of
*        the collapsed axis to give the correct bounds.
*     10-MAY-2007 (DSB):
*        Correct problems with use of KPG1_WCFAX.
*     2007 July 19 (MJC):
*        Used new KPG1_ASAPA to identify pixel axis corresponding to
*        the collapsed WCS axis, rather than inline code.
*     2007 November 17 (MJC):
*        Revised KPS1_CLPSx API.
*     2007 December 10 (MJC):
*        Add warning for non-zero WLIM when its threshold introduces bad
*        values into the output and the corresponding input data
*        contain some good values.
*     4-APR-2008 (DSB):
*        Change prologue to emphasise the fact that HIGH and LOW are
*        formatted axis values.
*     2008 June 17 (MJC):
*        Trim trailing blanks from output NDF character components.
*     2008 September 11 (MJC):
*        Add COMP parameter.
*     2008 September 24 (MJC):
*        Add Csigma estimator.
*     2009 July 2 (MJC):
*        Add Cmean estimator.
*     2009 July 5 (MJC):
*        Added CLIP parameter.  Improve a WLIM-related WARNING message
*        and fix bug in calculations used to decide whether or not to
*        issue either warning and the reported fractions of bad values.
*     2009 July 15 (MJC):
*        Fix bug calculating the fraction of bad pixels created for
*        large datasets processed in blocks.
*     2009 November 20 (MJC):
*        Adjust the precision of the fractions and WLIM in the bad-pixel
*        warning.
*     17-JUL-2012 (DSB):
*        Added "NGood", "NBad", "FGood" and "FBad" estimators.
*     2013 May 10 (MJC):
*        Do not pass COMP=Error to NDF_MTYPE.
*     2018 January 11 (MJC):
*        Add "FastMed" estimator.
*     15-JAN-2020 (DSB):
*        Add support for huge NDFs.
*     12-FEB-2020 (DSB):
*        Increase max block size by a factor of ten compared to its 2006
*        value. This speeds up the multi-threaded combination methods by
*        about 25%.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'         ! Global SSE definitions
      INCLUDE  'PAR_ERR'         ! Parameter-system errors
      INCLUDE  'NDF_PAR'         ! NDF_ public constants
      INCLUDE  'DAT_PAR'         ! HDS public constants
      INCLUDE  'AST_PAR'         ! AST constants and functions
      INCLUDE  'CNF_PAR'         ! For CNF_PVAL function
      INCLUDE  'MSG_PAR'         ! Message-system constants
      INCLUDE  'PRM_PAR'         ! PRIMDAT constants

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      INTEGER KPG1_FLOOR8        ! Most positive integer*8 .LE. a given
                                 ! real
      INTEGER KPG1_CEIL8         ! Most negative integer*8 .GE. a given
                                 ! real

*  Local Constants:
      REAL CLPDEF                ! Default no. of standard deviations to
      PARAMETER( CLPDEF = 3.0 )  ! clip for mode, clipped mean & std dev

      INTEGER*8 MAXPIX           ! Maximum number of pixels in a block
      PARAMETER ( MAXPIX = 83886080 ) ! Guestimate a size: 80 mega

*  Local Variables:
      CHARACTER AUNITS*( 30 )    ! Units of co-ordinates
      CHARACTER ATTRIB*( 10 )    ! AST attribute name
      CHARACTER COMP * ( 13 )    ! List of components to process
      CHARACTER COMPO * ( 13 )   ! List of output components to process
      CHARACTER DTYPE*( NDF__SZFTP ) ! Numeric type for output arrays
      CHARACTER ESTIM*( 7 )      ! Method to use to estimate collapsed
                                 ! values
      CHARACTER FORMAT*6         ! Format for warning
      CHARACTER ITYPE*( NDF__SZTYP ) ! Numeric type for input arrays
      CHARACTER LOC1*(DAT__SZLOC)! Locator to the output NDF
      CHARACTER LOC2*(DAT__SZLOC)! Locator to NDF AXIS array
      CHARACTER LOC3*(DAT__SZLOC)! Locator to copy of the original AXIS
                                 ! array
      CHARACTER LOC4*(DAT__SZLOC)! Locator to cell of the new AXIS array
      CHARACTER LOC5*(DAT__SZLOC)! Locator to cell of the old AXIS array
      CHARACTER LOC6*(DAT__SZLOC)! Locator to component of the old cell
      CHARACTER * ( 8 ) MCOMP    ! Component name for mapping arrays
      CHARACTER NAME*(DAT__SZNAM)! The component name
      CHARACTER OTYPE*( NDF__SZTYP ) ! Numeric type for output arrays
      CHARACTER TTLC*( 255 )     ! Title of original current Frame
      CHARACTER UNITS*( 60 )     ! Units of data
      DOUBLE PRECISION AXHIGH    ! High bound of collapse axis in
                                 ! current Frame
      DOUBLE PRECISION AXLOW     ! Low bound of collapse axis in current
                                 ! Frame
      DOUBLE PRECISION CURPOS( NDF__MXDIM ) ! Valid current Frame
                               ! position
      DOUBLE PRECISION DLBND( NDF__MXDIM ) ! Lower bounds in pixel
                               ! co-ords
      DOUBLE PRECISION DUBND( NDF__MXDIM ) ! Upper bounds in pixel
                               ! co-ords
      DOUBLE PRECISION GRDPOS( NDF__MXDIM ) ! Valid grid Frame position
      DOUBLE PRECISION PIXPOS( NDF__MXDIM ) ! Valid pixel Frame position
      DOUBLE PRECISION PXHIGH    ! High pixel bound of collapse axis
      DOUBLE PRECISION PXLOW     ! Low pixel bound of collapse axis
      INTEGER*8 AEL              ! Number of collapse axis elements
      INTEGER AXES( NDF__MXDIM ) ! A list of axis indices
      INTEGER CFRM               ! Original Current Frame pointer
      INTEGER*8 D                ! A dimension size
      INTEGER*8 EL1              ! Number of elements in an input mapped
                                 ! array
      INTEGER*8 EL2              ! Number of elements in an output
                                 ! mapped array
      INTEGER*8 ELO              ! Number of elements in output NDF
      INTEGER*8 GHI              ! High GRID index for collapse axis
      INTEGER*8 GLO              ! Low GRID index for collapse axis
      LOGICAL HIGHER             ! Significant dimensions above collapse
                                 ! axis?
      INTEGER I                  ! Loop count
      INTEGER IAXIS              ! Index of collapse axis within current
                                 ! Frame
      INTEGER IBL                ! Identifier for input-NDF block
      INTEGER IBLOCK             ! Loop counter for the NDF blocks
      INTEGER*8 IBLSIZ( NDF__MXDIM ) ! Input-NDF sizes for processing
                                 ! large datasets in blocks
      INTEGER*8 IERR             ! Position of first numerical error
      INTEGER INDFI              ! Input NDF identifier
      INTEGER INDFO              ! Output NDF identifier
      INTEGER INDFS              ! Input NDF-section identifier
      INTEGER IPAXCO             ! Pointers to mapped d.p. axis array
      INTEGER IPAXWO             ! Pointers to mapped d.p. axis array
      INTEGER IPCO               ! Pointers to mapped co-ordinate array
      INTEGER IPIN( 2 )          ! Pointers to mapped input arrays
      INTEGER IPIX               ! Index of PIXEL Frame within WCS
                                 ! FrameSet
      INTEGER IPOUT( 2 )         ! Pointers to mapped output arrays
      INTEGER IPW1               ! Pointer to first work array
      INTEGER IPW2               ! Pointer to second work array
      INTEGER IPW3               ! Pointer to third work array
      INTEGER IPWID              ! Pointers to mapped width work array
      INTEGER IWCS               ! WCS FrameSet pointer
      INTEGER J                  ! Loop count
      INTEGER JAXIS              ! Index of collapse axis within PIXEL
                                 ! Frame
      INTEGER*8 JHI              ! High pixel index for collapse axis
      INTEGER*8 JLO              ! Low pixel index for collapse axis
      INTEGER*8 LBND( NDF__MXDIM ) ! Lower pixel index bounds of the input
                                   ! NDF
      INTEGER*8 LBNDO( NDF__MXDIM )! Lower pixel index bounds of the
                                   ! output NDF
      INTEGER*8 LBNDS( NDF__MXDIM )! Lower pixel index bounds of the
                                   ! section of the input NDF
      INTEGER*8 MAXSIZ           ! Maximum size of block along current
                                 ! dimension
      INTEGER MAP                ! PIXEL Frame to Current Frame Mapping
                                 ! pointer
      INTEGER NAXC               ! Original number of current Frame axes
      INTEGER NBLOCK             ! Number of NDF blocks
      INTEGER NC                 ! Used length of string
      INTEGER NCOMP              ! No. of components within cell of AXIS
                                 ! array
      INTEGER NDEC               ! Number of decimal places in warning
      INTEGER NDIM               ! Number of pixel axes in input NDF
      INTEGER NDIMO              ! Number of pixel axes in output NDF
      INTEGER*8 NERR             ! Number of numerical errors
      INTEGER*8 NFLAG            ! Number of WLIM-flagged o/p values
      INTEGER NVAL               ! Number of values obtained (1)
      INTEGER OBL                ! Identifier for output-NDF block
      INTEGER*8 OBLSIZ( NDF__MXDIM ) ! Output-NDF sizes for processing
                                 ! large datasets in blocks
      INTEGER OTOMAP             ! One-to-one mapping
      INTEGER*8 UBND( NDF__MXDIM ) ! Upper pixel index bounds of the input
                                   ! NDF
      INTEGER*8 UBNDO( NDF__MXDIM )! Upper pixel index bounds of the
                                   ! output NDF
      INTEGER*8 UBNDS( NDF__MXDIM )! Upper pixel index bounds of the
                                   ! section of the input NDF
      LOGICAL GOTAX              ! Does the NDF have an AXIS component?
      LOGICAL LOOP               ! Continue to loop through dimensions?
      LOGICAL NDFVAR             ! NDF contains a variance array?
      LOGICAL PROVAR             ! Collapse VARIANCE?
      LOGICAL TRIM               ! Remove collapsed axes from o/p?
      LOGICAL USEALL             ! Use the entire collapse pixel axis?
      LOGICAL USEVAR             ! Allow weights to be derived from the
                                 ! NDF's variance array (if present)
      LOGICAL VAR                ! Process variances?
      REAL CLIP                  ! Value of CLIP parameter
      REAL WLIM                  ! Value of WLIM parameter

*.

*  Check the global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain input NDF and some of its AST Frames.
*  ============================================

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', INDFI, STATUS )

*  Get the bounds of the NDF.
      CALL NDF_BOUND8( INDFI, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Get the component we require.
      CALL PAR_CHOIC( 'COMP', 'DATA', 'DATA,VARIANCE,QUALITY,ERROR',
     :                .FALSE., COMP, STATUS )
      PROVAR = COMP .EQ. 'VARIANCE' .OR. COMP .EQ. 'ERROR'
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Get the WCS FrameSet from the NDF.
      CALL KPG1_GTWCS( INDFI, IWCS, STATUS )

*  Allow the user to modify the current Frame attributes.
      CALL KPG1_ASSET( 'COLLAPSE', 'WCSATTS', IWCS, STATUS )

*  Extract the current and base Frames, and get the number of axes in
*  the current Frame, and its title.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      NAXC = AST_GETI( CFRM, 'NAXES', STATUS )
      TTLC = AST_GETC( CFRM, 'TITLE', STATUS )

*  Find the index of the PIXEL Frame.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Extract the Mapping from PIXEL Frame to Current Frame.
      MAP = AST_GETMAPPING( IWCS, IPIX, AST__CURRENT, STATUS )

*  Select the collapse axis and limits thereon.
*  ============================================

*  Get the index of the current Frame axis defining the collapse
*  direction.  Use the last axis as the dynamic default.
      IAXIS = NAXC
      CALL KPG1_GTAXI( 'AXIS', CFRM, 1, IAXIS, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the bounding values for the specified current Frame axis defining
*  the height of the slab to be collapsed.
      AXLOW = AST__BAD
      CALL KPG1_GTAXV( 'LOW', 1, .TRUE., CFRM, IAXIS, AXLOW, NVAL,
     :                 STATUS )

      AXHIGH = AST__BAD
      CALL KPG1_GTAXV( 'HIGH', 1, .TRUE., CFRM, IAXIS, AXHIGH, NVAL,
     :                 STATUS )

*  If a null value was supplied for either of these parameters, annul
*  the error and set a flag indicating that the whole axis should be
*  used.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         USEALL = .TRUE.
      ELSE
         USEALL = .FALSE.
      END IF

*  Determine which pixel axis is most nearly aligned with the selected
*  WCS axis.
      CALL KPG1_ASAPA( INDFI, CFRM, MAP, IAXIS, AXLOW, AXHIGH,
     :                 JAXIS, PXLOW, PXHIGH, OTOMAP, STATUS )

*  Find an arbitrary position within the NDF which has valid current
*  Frame co-ordinates.  Both pixel and current Frame co-ordinates for
*  this position are returned.
      DO I = 1, NDIM
         DLBND( I ) = DBLE( LBND( I ) - 1 )
         DUBND( I ) = DBLE( UBND( I ) )
      END DO

      CALL KPG1_ASGDP( MAP, NDIM, NAXC, DLBND, DUBND, PIXPOS, CURPOS,
     :                 STATUS )

*  Convert the pixel position into a grid position.
      DO I = 1, NDIM
         GRDPOS( I ) = PIXPOS( I ) - LBND( I ) + 1.5
      END DO

*  Derive the pixel-index bounds along the collapse axis.
*  ======================================================

*  Choose the pixel index bounds of the slab to be collapsed on the
*  collapse pixel axis.  If no axis limits supplied, use the upper and
*  lower bounds.
      IF ( USEALL ) THEN
         JLO = LBND( JAXIS )
         JHI = UBND( JAXIS )

*  If limits were supplied...
      ELSE

*  Find the projection of the two test points on to the collapse axis.
         JLO = KPG1_FLOOR8( REAL( MIN( PXHIGH, PXLOW ) ) ) + 1
         JHI = KPG1_CEIL8( REAL( MAX( PXHIGH, PXLOW ) ) )

*  Ensure these are within the bounds of the pixel axis.
         JLO = MAX( LBND( JAXIS ), JLO )
         JHI = MIN( UBND( JAXIS ), JHI )

*  Report an error if there is no intersection.
         IF ( JLO .GT. JHI .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COLLAPSE_ERR4', 'The axis range to '//
     :                    'collapse covers zero pixels (are the '//
     :                    'HIGH and LOW parameter values equal '//
     :                    'or outside the bounds of the NDF?)',
     :                    STATUS )
            GO TO 999
         END IF

      END IF

*  Tell the user the range of pixels being collapsed.
      CALL MSG_SETI( 'I', JAXIS )
      CALL MSG_SETK( 'L', JLO )
      CALL MSG_SETK( 'H', JHI )
      CALL MSG_OUT( 'COLLAPSE_MSG1', '   Collapsing pixel axis ^I '/
     :              /'from pixel ^L to pixel ^H inclusive...', STATUS )
      CALL MSG_BLANK( STATUS )
      AEL = JHI - JLO + 1

*  Define the output NDF's bounds.
*  ===============================

*  See if the collapsed axis is to be trimmed from the output NDF.
      CALL PAR_GET0L( 'TRIM', TRIM, STATUS )

*  If we are removing the collapsed pixel axis... */
      IF ( TRIM ) THEN

*  The output NDF will have one fewer axes than the input NDF.
         NDIMO = NDIM - 1

*  For each pixel axis I in the final output NDF, find the
*  corresponding axis in the input NDF.
         DO I = 1, NDIMO
            IF ( I .LT. JAXIS ) THEN
               AXES( I ) = I
            ELSE
               AXES( I ) = I + 1
            END IF
         END DO

*  Find the pixel bounds of the NDF after axis permutation.
         DO I = 1, NDIMO
            LBNDO( I ) = LBND( AXES( I ) )
            UBNDO( I ) = UBND( AXES( I ) )
         END DO

*  If we are retaining the collapsed pixel axis, just set its upper and
*  lower bounds to 1.
      ELSE
         NDIMO = NDIM

         DO I = 1, NDIMO
            AXES( I ) = I
            LBNDO( I ) = LBND( I )
            UBNDO( I ) = UBND( I )
         END DO

         LBNDO( JAXIS ) = 1
         UBNDO( JAXIS ) = 1
      END IF

*  Determine whether or not there are significant dimensions above
*  the collapse axis.
      HIGHER = JAXIS .NE. NDIM
      IF ( HIGHER ) THEN
         HIGHER = .FALSE.
         DO I = JAXIS + 1, NDIM
            HIGHER = HIGHER .OR. ( UBND( I ) - LBND( I ) ) .NE. 0
         END DO
      END IF

*  Propagate the input to the output NDF.
*  ======================================

*  Create the output NDF by propagation from the input NDF.  This
*  results in history, etc., being passed on.  The shape and
*  dimensionality will be wrong but they will be corrected later.
      CALL LPG_PROP( INDFI, 'Axis,Units', 'OUT', INDFO, STATUS )

*  Set the title of the output NDF.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', INDFI, INDFO, STATUS )

*  Obtain the component and processing data type.
*  ==============================================

*  See if the input NDF has a VARIANCE component.
      CALL NDF_STATE( INDFI, 'VARIANCE', NDFVAR, STATUS )

*  Find out whether variances are to be used to define the weights, if
*  the NDF contains any.
      USEVAR = .FALSE.
      IF ( NDFVAR ) THEN
         IF ( .NOT. PROVAR )
     :     CALL PAR_GET0L( 'VARIANCE', USEVAR, STATUS )

      ELSE IF ( PROVAR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COLLAPSE_ERR3', 'COLLAPSE: There is no '//
     :                 'VARIANCE component to collapse.', STATUS )
         GO TO 999
      END IF

*  Weights will be derived from variances only if allowed by USEVAR and
*  if the NDF contains a variance array.
      VAR = ( USEVAR .AND. NDFVAR )

*  Store a list of components to be accessed.  The output component list
*  must start with the DATA_ARRAY.
      COMPO = 'DATA'
      IF ( VAR ) THEN
         MCOMP = MCOMP( : CHR_LEN( MCOMP ) ) // ',VARIANCE'
         COMP = COMP( : CHR_LEN( COMP ) ) // ',VARIANCE'
         COMPO = COMPO( : 4 ) // ',VARIANCE'
      END IF

*  Determine the numeric type to be used for processing the input
*  data and variance (if any) arrays.  Since the subroutines that
*  perform the collapse need the data and variance arrays in the same
*  data type, the component list is used.  This application supports
*  single- and double-precision floating-point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', INDFI, INDFO, COMP, ITYPE, DTYPE,
     :                STATUS )

*  Adjust output NDF to its new shape.
*  ===================================

*  Easy if we are retaining the collapsed axes....
      IF ( .NOT. TRIM ) THEN
         CALL NDF_SBND8( NDIMO, LBNDO, UBNDO, INDFO, STATUS )

*  Otherwise...
      ELSE

*  The shape and size of the output NDF created above will be wrong,
*  so we need to correct it by removing the collapse axis.  This is
*  easy if it is the final axis (we would just use NDF_SBND8 specifying
*  NDIM-1 axes), but is not so easy if the collapse axis is not the
*  final axis.  In this case, we do the following.
*    1) - Save copies of an AXIS structure in the output NDF (because
*         the following step will change their lengths to match the new
*         bounds).
*    2) - Change the bounds and dimensionality of the NDF to the
*         appropriate values.
*    3) - Restore the saved AXIS structures, permuting them so that they
*         apply to the correct axis.
*    4) - Adjust the WCS FrameSet to pick the required axis from the
*         original Base Frame.

*  First see if the AXIS component is defined.
         CALL NDF_STATE( INDFO, 'AXIS', GOTAX, STATUS )

*  If so, we need to save copies of the AXIS structures.
         IF ( GOTAX ) THEN

*  Get an HDS locator to the output NDF structure.
            CALL NDF_LOC( INDFO, 'UPDATE', LOC1, STATUS )

*  Get a locator to the AXIS component.
            CALL DAT_FIND( LOC1, 'AXIS', LOC2, STATUS )

*  Take a copy of the AXIS component and call it OLDAXIS.
            CALL DAT_COPY( LOC2, LOC1, 'OLDAXIS', STATUS )

*  Get a locator for OLDAXIS.
            CALL DAT_FIND( LOC1, 'OLDAXIS', LOC3, STATUS )
         END IF

*  Set the output NDF bounds to the required values.  This will change
*  the lengths of the current AXIS arrays (but we have a copy of the
*  originals in OLDAXIS), and reduce the dimensionality by one.
         CALL NDF_SBND8( NDIMO, LBNDO, UBNDO, INDFO, STATUS )

*  We now re-instate any AXIS structures, in their new order.
         IF ( GOTAX ) THEN

*  Promote the NDF locator to a primary locator so that the HDS
*  container file is not closed when the NDF identifier is annulled.
            CALL DAT_PRMRY( .TRUE., LOC1, .TRUE., STATUS )

*  The DATA array of the output NDF will not yet be in a defined state.
*  This would result in NDF_ANNUL reporting an error, so we temporarily
*  map the DATA array (which puts it into a defined state) to prevent
*  this.
            CALL NDF_MAP8( INDFO, 'DATA', ITYPE, 'WRITE', IPOUT( 1 ),
     :                     EL2, STATUS )

*  Annul the supplied NDF identifier so that we can change the contents
*  of the NDF using HDS, without getting out of step with the NDFs
*  libraries description of the NDF.
            CALL NDF_ANNUL( INDFO, STATUS )

*  Loop round each cell in the returned AXIS structure.
            DO I = 1, NDIMO

*  Get a locator to this cell in the NDFs AXIS array.
               CALL DAT_CELL( LOC2, 1, I, LOC4, STATUS )

*  Empty it of any components
               CALL DAT_NCOMP( LOC4, NCOMP, STATUS )
               DO J = NCOMP, 1, -1
                  CALL DAT_INDEX( LOC4, J, LOC5, STATUS )
                  CALL DAT_NAME( LOC5, NAME, STATUS )
                  CALL DAT_ANNUL( LOC5, STATUS )
                  CALL DAT_ERASE( LOC4, NAME, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GO TO 999
               END DO

*  Get a locator to the corresponding cell in the OLDAXIS array.
               CALL DAT_CELL( LOC3, 1, AXES( I ), LOC5, STATUS )

*  We now copy all the components of the OLDAXIS cell into the AXIS
*  cell.  Find the number of components, and loop round them.
               CALL DAT_NCOMP( LOC5, NCOMP, STATUS )
               DO J = NCOMP, 1, -1

*  Get a locator to this component in the original OLDAXIS cell.
                  CALL DAT_INDEX( LOC5, J, LOC6, STATUS )

*  Get its name.
                  CALL DAT_NAME( LOC6, NAME, STATUS )

*  Copy it into the new AXIS structure.
                  CALL DAT_COPY( LOC6, LOC4, NAME, STATUS )

*  Annul the locators.
                  CALL DAT_ANNUL( LOC6, STATUS )

*  Abort if an error has occurred.
                  IF ( STATUS .NE. SAI__OK ) GO TO 999

               END DO

*  Annul the locators.
               CALL DAT_ANNUL( LOC4, STATUS )
               CALL DAT_ANNUL( LOC5, STATUS )

            END DO

*  Annul the locator to the OLDAXIS structure and then erase the object.
            CALL DAT_ANNUL( LOC3, STATUS )
            CALL DAT_ERASE( LOC1, 'OLDAXIS', STATUS )

*  Annul the AXIS array locator.
            CALL DAT_ANNUL( LOC2, STATUS )

*  Import the modified NDF back into the NDF system.
            CALL NDF_FIND( LOC1, ' ', INDFO, STATUS )

*  Annul the NDF locator.
            CALL DAT_ANNUL( LOC1, STATUS )

         END IF
      END IF

*  Obtain the remaining parameters.
*  ================================

*  Get the ESTIMATOR and WLIM parameters.  Cannot weight with the
*  variance if it is the variance that's being collapsed.  The fast
*  median does not support weights.
      IF ( PROVAR ) THEN
         CALL PAR_CHOIC( 'ESTIMATOR', 'Mean','Mean,Mode,Median,Max,'/
     :                   /'Min,Comax,Comin,Absdev,RMS,Sigma,Sum,Iwc,'/
     :                   /'Iwd,Integ,Cmean,Csigma,NGood,NBad,FGood,'/
     :                   /'FBad,FastMed', .FALSE., ESTIM, STATUS )
      ELSE IF ( VAR ) THEN
         CALL PAR_CHOIC( 'ESTIMATOR', 'Mean','Mean,WMean,Mode,Median,'/
     :                   /'Max,Min,Comax,Comin,Absdev,RMS,Sigma,Sum,'/
     :                   /'Iwc,Iwd,Integ,Cmean,Csigma,NGood,NBad,'/
     :                   /'FGood,FBad', .FALSE., ESTIM, STATUS )
      ELSE
         CALL PAR_CHOIC( 'ESTIMATOR', 'Mean','Mean,WMean,Mode,Median,'/
     :                   /'Max,Min,Comax,Comin,Absdev,RMS,Sigma,Sum,'/
     :                   /'Iwc,Iwd,Integ,Cmean,Csigma,NGood,NBad,'/
     :                   /'FGood,FBad,FastMed', .FALSE., ESTIM, STATUS )
      END IF

      CALL PAR_GDR0R( 'WLIM', 0.3, 0.0, 1.0, .FALSE., WLIM, STATUS )

*  For now obtain just a single number of standard deviations at which
*  to clip.
      IF ( ESTIM .EQ. 'MODE' .OR. ESTIM .EQ. 'CMEAN' .OR.
     :     ESTIM .EQ. 'CSIGMA' ) THEN
         CALL PAR_GDR0R( 'CLIP', CLPDEF, VAL__SMLR, VAL__MAXR, .FALSE.,
     :                   CLIP, STATUS )
      ELSE
         CLIP = CLPDEF
      END IF

*  Adjust output data type if required.
*  ====================================

*  The NBad and NGood estimators always produce _INTEGER output NDFs.
      IF ( ESTIM .EQ. 'NGOOD' .OR. ESTIM .EQ. 'NBAD' ) THEN
         CALL NDF_RESET( INDFO, COMPO, STATUS )
         CALL NDF_STYPE( '_INTEGER', INDFO, COMPO, STATUS )
         OTYPE = '_INTEGER'
      ELSE
         OTYPE = ITYPE
      END IF

*  Redefine the data units.
*  ========================
      IF ( ESTIM .EQ. 'COMAX' .OR. ESTIM .EQ. 'COMIN' .OR.
     :     ESTIM .EQ. 'IWC' .OR. ESTIM .EQ. 'IWD' ) THEN

*  Obtain the collapsed-axis units of the input NDF; these now become
*  the data units in output NDF.  Note that NDF_CPUT does not truncate
*  trailing blanks.
         ATTRIB = 'UNIT('
         NC = 5
         CALL CHR_PUTI( IAXIS, ATTRIB, NC )
         CALL CHR_PUTC( ')', ATTRIB, NC )
         UNITS = AST_GETC( IWCS, ATTRIB( :NC ), STATUS )

         NC = CHR_LEN( UNITS )
         CALL NDF_CPUT( UNITS( :NC ), INDFO, 'Units', STATUS )

*  New unit is the existing unit times the co-ordinate's unit.  So
*  obtain each unit and concatenate the two inserting a blank between
*  them.
      ELSE IF ( ESTIM .EQ. 'INTEG' ) THEN
         ATTRIB = 'UNIT('
         NC = 5
         CALL CHR_PUTI( IAXIS, ATTRIB, NC )
         CALL CHR_PUTC( ')', ATTRIB, NC )
         AUNITS = AST_GETC( IWCS, ATTRIB( :NC ), STATUS )

         UNITS = ' '
         CALL NDF_CGET( INDFI, 'Unit', UNITS, STATUS )
         CALL NDF_CLEN( INDFI, 'Unit', NC, STATUS )
         NC = NC + 1
         UNITS( NC:NC ) = ' '
         CALL CHR_APPND( AUNITS, UNITS, NC )

         CALL NDF_CPUT( UNITS( :NC ), INDFO, 'Units', STATUS )

*  New unit is "pixel".
      ELSE IF ( ESTIM .EQ. 'NGOOD' .OR. ESTIM .EQ. 'NBAD' ) THEN
         CALL NDF_CPUT( 'Pixel', INDFO, 'Units', STATUS )

*  Dimensionless...
      ELSE IF ( ESTIM .EQ. 'FGOOD' .OR. ESTIM .EQ. 'FBAD' ) THEN
         CALL NDF_CPUT( ' ', INDFO, 'Units', STATUS )

      END IF

*  Process in blocks.
*  ==================

*  For large datasets, there may be insufficient memory.  Therefore
*  we form blocks to process, one at a time.  For this by definition
*  we need the collapse-axis pixels to always be present in full for
*  each pixel along the other pixel axes.  If this leaves room for a
*  full span of a dimension that becomes the block size along that
*  axis.  Partial fills take the remaining maximum size and subsequent
*  dimensions' block sizes are unity.
      IBLSIZ( JAXIS ) = AEL
      MAXSIZ = MAX( 1, MAXPIX / AEL )
      LOOP = .TRUE.
      J = 0
      DO I = 1, NDIM
         IF ( I .NE. JAXIS ) THEN
            IF ( LOOP ) THEN
               D = UBND( I ) - LBND( I ) + 1
               IF ( MAXSIZ .GE. D ) THEN
                  IBLSIZ( I ) = D
                  MAXSIZ = MAXSIZ / D
               ELSE
                  IBLSIZ( I ) = MAXSIZ
                  LOOP = .FALSE.
               END IF
            ELSE
               IBLSIZ( I ) = 1
            END IF

*  Copy the output NDF block sizes in sequence omitting the
*  collapse axis.
            J = J + 1
            OBLSIZ( J ) = IBLSIZ( I )

*  If we are retaining the degenerate collapsed pixel axis in the output
*  NDF, then the output block size for that axis can be 1.
         ELSE IF ( .NOT. TRIM ) THEN
            J = J + 1
            OBLSIZ( J ) = 1

         END IF
      END DO

*  If the LOW and HIGH limits have reduced the collapsed section from
*  the whole collapsed dimension, then we cannot use the original input
*  NDF to derive the number of blocks.  Instead we create a subsection
*  spanning the actual collapse limits, as if the user had supplied
*  this section with the input NDF.
      DO I = 1, NDIM
         LBNDS( I ) = LBND( I )
         UBNDS( I ) = UBND( I )
      END DO
      LBNDS( JAXIS ) = JLO
      UBNDS( JAXIS ) = JHI
      IF ( USEALL ) THEN
         INDFS = INDFI
      ELSE
         CALL NDF_SECT8( INDFI, NDIM, LBNDS, UBNDS, INDFS, STATUS )
      END IF

*  Determine the number of blocks.
      CALL NDF_NBLOC8( INDFS, NDIM, IBLSIZ, NBLOCK, STATUS )

*  The total number of elements in the output array is needed for the
*  calculation of the fraction of bad pixels generated by the
*  statistical calculations in conjunction with the WLIM parameter.
*  The count of the bad pixels generated is summed within KPS1_CLPSx.
      NFLAG = 0
      ELO = 0

*  Loop through each block.  Start a new NDF context.

      DO IBLOCK = 1, NBLOCK
         CALL NDF_BEGIN
         CALL NDF_BLOCK8( INDFS, NDIM, IBLSIZ, IBLOCK, IBL, STATUS )
         CALL NDF_BLOCK8( INDFO, NDIMO, OBLSIZ, IBLOCK, OBL, STATUS )

*  Map the NDF arrays and workspace required.
*  ==========================================

*  Map the full input, and output data and (if needed) variance arrays.
         CALL NDF_MAP8( IBL, MCOMP, ITYPE, 'READ', IPIN, EL1, STATUS )
         CALL NDF_MAP8( OBL, COMPO, OTYPE, 'WRITE', IPOUT, EL2, STATUS )

         IF ( .NOT. VAR ) THEN
            IPIN( 2 ) = IPIN( 1 )
            IPOUT( 2 ) = IPOUT( 1 )
         END IF

*  Obtain the bounds of the blocks.
         CALL NDF_BOUND8( IBL, NDF__MXDIM, LBNDS, UBNDS, NDIM, STATUS )
         CALL NDF_BOUND8( OBL, NDF__MXDIM, LBNDO, UBNDO, NDIMO, STATUS )

*  Allocate work space, unless the last axis is being collapsed (in
*  which case no work space is needed).
         IF ( HIGHER ) THEN
            CALL PSX_CALLOC8( EL2 * AEL, ITYPE, IPW1, STATUS )
            IF ( VAR ) THEN
               CALL PSX_CALLOC8( EL2 * AEL, ITYPE, IPW2, STATUS )
            ELSE
               IPW2 = IPW1
            END IF

*  Store safe pointer values if no work space is needed.
         ELSE
            IPW1 = IPIN( 1 )
            IPW2 = IPIN( 1 )
         END IF

*  Associate co-ordinate information.
*  ==================================

*  Obtain co-ordinates along the collapse axis for the following
*  methods.
         IF ( ESTIM .EQ. 'COMAX' .OR. ESTIM .EQ. 'COMIN' .OR.
     :        ESTIM .EQ. 'IWC' .OR. ESTIM .EQ. 'IWD' .OR.
     :        ESTIM .EQ. 'INTEG' ) THEN

*  Create workspace for the co-ordinates along a single WCS axis
*  in the correct data type.
            CALL PSX_CALLOC8( EL1, '_DOUBLE', IPAXCO, STATUS )
            CALL PSX_CALLOC8( EL1, ITYPE, IPCO, STATUS )
            CALL PSX_CALLOC8( UBNDS( JAXIS ) - LBNDS( JAXIS ) + 1,
     :                       '_DOUBLE', IPAXWO, STATUS )

*  Allocate work space, unless the last pixel axis is being collapsed
*  (in which case no work space is needed).
            IF ( HIGHER ) THEN
               CALL PSX_CALLOC8( EL2 * AEL, ITYPE, IPW3, STATUS )
            ELSE
               IPW3 = IPIN( 1 )
            END IF

*  Obtain the double-precision co-ordinate centres along the collapse
*  axis in the current Frame.
            CALL KPG1_WCFAX8( LBNDS, UBNDS, MAP, JAXIS, IAXIS,
     :                       %VAL( CNF_PVAL( IPAXCO ) ),
     :                       %VAL( CNF_PVAL( IPAXWO ) ), STATUS )

*  Copy the centres to the required precision.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL VEC8_DTOR( .TRUE., EL1, %VAL( CNF_PVAL( IPAXCO ) ),
     :                         %VAL( CNF_PVAL( IPCO ) ), IERR, NERR,
     :                         STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL VEC8_DTOD( .TRUE., EL1, %VAL( CNF_PVAL( IPAXCO ) ),
     :                         %VAL( CNF_PVAL( IPCO ) ), IERR, NERR,
     :                         STATUS )

            END IF
            CALL PSX_FREE( IPAXCO, STATUS )
            CALL PSX_FREE( IPAXWO, STATUS )

*  Store safe pointer value if axis centres are not needed.
         ELSE
            IPCO = IPIN( 1 )
            IPW3 = IPIN( 1 )
         END IF

*  Associate AXIS-width information.
*  =================================

*  Obtain AXIS widths along the collapse axis for the following
*  methods.
         IF ( ESTIM .EQ. 'INTEG' ) THEN

*  Allocate work space for the widths to be derived from the
*  co-ordinates.  This assumes full filling of pixels.
            CALL PSX_CALLOC8( EL2 * AEL, ITYPE, IPWID, STATUS )

*  Store safe pointer value if widths are not needed.
         ELSE
            IPWID = IPIN( 1 )
         END IF

*  Collapse.
*  =========

*  Now do the work, using a routine appropriate to the numeric type.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_CLPSR( JAXIS, JLO, JHI, VAR, ESTIM, WLIM, CLIP,
     :                       EL2, NDIM, LBNDS, UBNDS,
     :                       %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                       %VAL( CNF_PVAL( IPIN( 2 ) ) ),
     :                       %VAL( CNF_PVAL( IPCO ) ),
     :                       NDIMO, LBNDO, UBNDO, HIGHER, NFLAG,
     :                       %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                       %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                       %VAL( CNF_PVAL( IPWID ) ),
     :                       %VAL( CNF_PVAL( IPW1 ) ),
     :                       %VAL( CNF_PVAL( IPW2 ) ),
     :                       %VAL( CNF_PVAL( IPW3 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_CLPSD( JAXIS, JLO, JHI, VAR, ESTIM, WLIM, CLIP,
     :                       EL2, NDIM, LBNDS, UBNDS,
     :                       %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                       %VAL( CNF_PVAL( IPIN( 2 ) ) ),
     :                       %VAL( CNF_PVAL( IPCO ) ),
     :                       NDIMO, LBNDO, UBNDO, HIGHER, NFLAG,
     :                       %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                       %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                       %VAL( CNF_PVAL( IPWID ) ),
     :                       %VAL( CNF_PVAL( IPW1 ) ),
     :                       %VAL( CNF_PVAL( IPW2 ) ),
     :                       %VAL( CNF_PVAL( IPW3 ) ), STATUS )

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T', ITYPE )
            CALL ERR_REP( 'COLLAPSE_ERR5', 'COLLAPSE: Unsupported '//
     :                    'data type ^T (programming error).', STATUS )
         END IF

*  Free the work space.
         IF ( HIGHER ) THEN
            CALL PSX_FREE( IPW1, STATUS )
            IF ( VAR ) CALL PSX_FREE( IPW2, STATUS )
         END IF

         IF ( ESTIM .EQ. 'COMAX' .OR. ESTIM .EQ. 'COMIN' .OR.
     :        ESTIM .EQ. 'IWC' .OR. ESTIM .EQ. 'IWD' .OR.
     :        ESTIM .EQ. 'INTEG' ) THEN
             CALL PSX_FREE( IPCO, STATUS )
             IF ( HIGHER ) CALL PSX_FREE( IPW3, STATUS )
         END IF

         IF ( ESTIM .EQ. 'INTEG' ) THEN
            CALL PSX_FREE( IPWID, STATUS )
            IF ( HIGHER ) CALL PSX_FREE( IPW3, STATUS )
         END IF

*   Add the current block's number of elements to the total.  This
*   is arguably clearer than finding the total using LBNDO and UBNDO.
         ELO = ELO + EL2

*   Close NDF context.
         CALL NDF_END( STATUS )
      END DO


*  Warn about lost pixels due to insufficient input values.
*  ========================================================

*  For some applications such as spectral-cube analysis where large
*  sections may be flagged, the wrong WLIM may lead to surprising
*  results.  While the WLIM default could be set to zero, this is not
*  the normal default of 0.3 used elsewhere in KAPPA.  At the risk of
*  annoying some users, report the number of output data that were
*  flagged by the WLIM threshold at the normal reporting level.
      IF ( NFLAG .GT. 0 .AND. WLIM .GT. 0.0 ) THEN

*  First set the number of decimal places commensurate with the number
*  of output data values.
         NDEC = MAX( 2, MIN( 10, NINT( LOG10( REAL( ELO ) ) ) ) )
         FORMAT = 'F'
         NC = 1
         CALL CHR_PUTI( NDEC + 2, FORMAT, NC )
         CALL CHR_PUTC( '.', FORMAT, NC )
         CALL CHR_PUTI( NDEC, FORMAT, NC )

         CALL MSG_FMTR( 'WLIM', FORMAT( :NC ), WLIM )
         IF ( NFLAG .EQ. ELO ) THEN
            IF ( WLIM .GT. 0.0 ) THEN
               CALL MSG_OUTIF( MSG__NORM, '',
     :           'WARNING: All of the output pixels are set bad due '/
     :           /'to an excessive number of bad values along the '/
     :           /'collapse axis.  To obtain good values, try '/
     :           /'decreasing the fraction of good values required '/
     :           /'with Parameter WLIM (currently ^WLIM).', STATUS )
            ELSE
               CALL MSG_OUTIF( MSG__NORM, '',
     :           'WARNING: All of the output pixels are set bad due '/
     :           /'to an excessive number of bad values along the '/
     :           /'collapse axis.', STATUS )
            END IF

*  The FRAC token is not directly comparable with WLIM.  Report the
*  fraction of bad pixels.  Note this includes cases where all the input
*  pixels along the collapse axis were bad for a given output pixel.
         ELSE IF ( NFLAG .LT. ELO ) THEN
            CALL MSG_FMTR( 'FRAC', FORMAT( :NC ),
     :                     REAL( NFLAG ) / REAL( ELO ) )

            CALL MSG_SETK( 'NF', NFLAG )
            CALL MSG_SETK( 'EL', ELO )
            IF ( WLIM .GT. 0.0 ) THEN
               CALL MSG_OUTIF( MSG__NORM, '',
     :           'WARNING: ^FRAC of the output pixels (^NF of ^EL) '/
     :           /'are set bad due to an excessive number of bad '/
     :           /'values along the collapse axis.  If this is '/
     :           /'undesired, decrease the fraction of good values '/
     :           /'required with Parameter WLIM (currently ^WLIM).',
     :           STATUS )
            ELSE
               CALL MSG_OUTIF( MSG__NORM, '',
     :           'WARNING: ^FRAC of the output pixels (^NF of ^EL) '/
     :           /'are set bad due to an excessive number of bad '/
     :           /'values along the collapse axis.', STATUS )
            END IF
         END IF
      END IF

*  Alter the WCS FrameSet.
*  =======================

*  We now modify the input NDFs WCS FrameSet by removing the collapsed
*  axis from the base and current Frames.
      GLO = JLO - LBND( JAXIS ) + 1
      GHI = JHI - LBND( JAXIS ) + 1
      CALL KPS1_CLPA0( IWCS, JAXIS, UBND( JAXIS ) - LBND( JAXIS ) + 1,
     :                 GRDPOS, TRIM, GLO, GHI, STATUS )

*  Save this modified WCS FrameSet in the output NDF.
      CALL NDF_PTWCS( IWCS, INDFO, STATUS )

*  KPS1_CLPA0 may have padded out the current WCS Frame with duplicated
*  GRID axes (in order to ensure that the current Frame has at least as
*  many axes as the base Frame).  We now convert these duplicated GRID
*  axes to the corresponding PIXEL axes.
      CALL KPS1_CLPA2( INDFO, STATUS )

*  Come here if something has gone wrong.
  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Report a contextual message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COLLAPSE_ERR6', 'COLLAPSE: Unable to collapse '/
     :                  /'an NDF along one axis.', STATUS )
      END IF

      END
