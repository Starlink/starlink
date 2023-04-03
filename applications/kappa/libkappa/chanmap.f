      SUBROUTINE CHANMAP( STATUS )
*+
*  Name:
*     CHANMAP

*  Purpose:
*     Creates a channel map from a cube NDF by compressing slices along
*     a nominated axis.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHANMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a two-dimensional channel-map image from
*     a three-dimensional NDF.  It collapses along a nominated pixel
*     axis in each of a series of slices.  The collapsed slices are
*     tiled with no margins to form the output image.  This grid of
*     channel maps is filled from left to right, and bottom to top.  A
*     specified range of axis values can be used instead of the whole
*     axis (see parameters LOW and HIGH).  The number of channels and
*     their arrangement into an image is controlled through parameters
*     NCHAN and SHAPE.
*
*     For each output pixel, all corresponding input pixel values
*     between the channel bounds of the nominated axis to be
*     collapsed are combined together using one of a selection of
*     estimators, including a mean, mode, or median, to produce the
*     output pixel value.

*  Usage:
*     chanmap in out axis nchan shape [low] [high] [estimator] [wlim]

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
*        statistics.  For "Cmean" and "Csigma" there is currently only
*        one iteration, but up to seven for "Mode".

*        The value must be positive.  [3.0]
*     ESTIMATOR = LITERAL (Read)
*        The method to use for estimating the output pixel values.  It
*        can be one of the following options.
*          "Mean"   -- Mean value
*          "WMean"  -- Weighted mean in which each data value is
*                      weighted by the reciprocal of the associated
*                      variance.  (2)
*          "Mode"   -- Modal value  (4)
*          "Median" -- Median value.  Note that this is extremely memory
*                      and CPU intensive for large datasets; use with
*                      care!  If strange things happen, use "Mean".  (3)
*          "Absdev" -- Mean absolute deviation from the unweighted mean.
*                      (2)
*          "Cmean"  -- Sigma-clipped mean.  (4)
*          "Csigma" -- Sigma-clipped standard deviation.  (4)
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
*                      integrated value.  (4)
*          "Max"    -- Maximum value.
*          "Min"    -- Minimum value.
*          "NBad"   -- Number of bad pixel values.
*          "NGood"  -- Number of good pixel values.
*          "Rms"    -- Root-mean-square value.  (4)
*          "Sigma"  -- Standard deviation about the unweighted mean. (4)
*          "Sum"    -- The total value.
*
*        The selection is restricted if each channel contains three or
*        fewer pixels.  For instance, measures of dispersion like
*        "Sigma" and "Iwd" are meaningless for single-pixel channels.
*        The minimum number of pixels per channel for each estimator is
*        given in parentheses in the list above.  Where there is no number,
*        there is no restriction.   If you supply an unavailable option,
*        you will be informed, and presented with the available options.
*        ["Mean"]
*     HIGH = LITERAL (Read)
*        Together with Parameter LOW, this parameter defines the range
*        of values for the axis specified by Parameter AXIS to be
*        divided into channels.  For example, if AXIS is 3 and the
*        current Frame of the input NDF has axes RA/DEC/Wavelength, then
*        a wavelength value should be supplied.  If, on the other hand,
*        the current Frame in the NDF was the PIXEL Frame, then a pixel
*        co-ordinate value would be required for the third axis (note,
*        the pixel with index I covers a range of pixel co-ordinates
*        from (I-1) to I).
*
*        Note, HIGH and LOW should not be equal.  If a null value (!) is
*        supplied for either HIGH or LOW, the entire range of the axis
*        fragmented into channels.  [!]
*     IN  = NDF (Read)
*        The input NDF.  This must have three dimensions.
*     LOW = LITERAL (Read)
*        Together with Parameter HIGH this parameter defines the range
*        of values for the axis specified by Parameter AXIS to be
*        divided into channels.  For example, if AXIS is 3 and the
*        current Frame of the input NDF has axes RA/DEC/Frequency, then
*        a frequency value should be supplied.  If, on the other hand,
*        the current Frame in the NDF was the PIXEL Frame, then a pixel
*        co-ordinate value would be required for the third axis (note,
*        the pixel with index I covers a range of pixel co-ordinates
*        from (I-1) to I).
*
*        Note, HIGH and LOW should not be equal.  If a null value (!) is
*        supplied for either HIGH or LOW, the entire range of the axis
*        fragmented into channels.  [!]
*     NCHAN = _INTEGER (Read)
*        The number of channels to appear in the channel map.  It must
*        be a positive integer up to the lesser of 100 or the number of
*        pixels along the collapsed axis.
*     OUT = NDF (Write)
*        The output NDF.
*     SHAPE = _INTEGER (Read)
*        The number of channels along the x axis of the output NDF.  The
*        number along the y axis will be 1+(NCHAN-1)/SHAPE.  A null
*        value (!) asks the application to select a shape.  It will
*        generate one that gives the most square output NDF possible.
*        The value must be positive and no more than the value of
*        Parameter NCHAN.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF.  [!]
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the current co-ordinate Frame of
*        the input NDF has more than three axes.  A group of three
*        strings should be supplied specifying the three axes which are
*        to be retained in a collapsed slab.  Each axis can be specified
*        using one of the following options.
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
*        supplied.  If a null (!) value is supplied, the axes with the
*        same indices as the three used pixel axes within the NDF are
*        used.  [!]
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
*        up if necessary to correspond to at least one pixel).  [0.3]

*  Examples:
*     chanmap cube chan4 lambda 4 2 4500 4550
*        The current Frame in the input three-dimensional NDF called
*        cube has axes with labels "RA", "DEC" and "Lambda", with the
*        lambda axis being parallel to the third pixel axis.  The above
*        command extracts four slabs of the input cube between
*        wavelengths 4500 and 4550 Angstroms, and collapses each slab,
*        into a single two-dimensional array with RA and DEC axes
*        forming a channel image.  Each channel image is pasted into a
*        2x2 grid within the output NDF called chan4.  Each pixel in the
*        output NDF is the mean of the corresponding input pixels with
*        wavelengths in 12.5-Angstrom bins.
*     chanmap in=cube out=chan4 axis=3 low=4500 high=4550 nchan=4
*             shape=2
*        The same as above except the axis to collapse along is
*        specified by index (3) rather than label (lambda), and it uses
*        keywords rather than positional parameters.
*     chanmap cube chan4 3 4 2 9.0 45.0
*        This is the same as the above examples, except that the current
*        Frame in the input NDF has been set to the PIXEL Frame (using
*        WCSFRAME), and so the high and low axis values are specified in
*        pixel co-ordinates instead of Angstroms, and each channel
*        covers nine pixels.  Note the difference between floating-point
*        pixel co-ordinates, and integer pixel indices (for instance the
*        pixel with index 10 extends from pixel co-ordinate 9.0 to pixel
*        co-ordinate 10.0).
*     chanmap in=zcube out=vel7 axis=1 low=-30 high=40 nchan=7 shape=!
*             estimator=max
*        This command assumes that the zcube NDF has a current
*        co-ordinate system where the first axis is radial velocity
*        (perhaps selected using WCSFRAME and WCSATTRIB), and the
*        second and third axes are "RA", and "DEC".  It extracts
*        seven velocity slabs of the input cube between -30 and +40 km/s,
*        and collapses each slab, into a single two-dimensional array
*        with RA and DEC axes forming a channel image.  Each channel
*        image is pasted into a default grid (likely 4x2) within the
*        output NDF called vel7.  Each pixel in the output NDF is the
*        maximum of the corresponding input pixels with velocities in
*        10-km/s bins.

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
*     -  The WCS of the output NDF retains the three-dimensional
*     co-ordinate system of the input cube for every tile, except that
*     each tile has a single representative mean co-ordinate for the
*     collapsed axis.
*     -  The slices may have slightly different pixel depths depending
*     where the boundaries of the channels lie in pixel co-ordinates.
*     Excise care interpreting estimators like "Sum" or ensure equal
*     numbers of pixels in each channel.

*  Related Applications:
*     KAPPA: COLLAPSE, CLINPLOT.

*  Implementation Status:
*     -  This routine correctly processes the DATA, VARIANCE, LABEL,
*     TITLE, UNITS, WCS, and HISTORY components of the input NDF; and
*     propagates all extensions.  AXIS and QUALITY are not propagated.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  The origin of the output NDF is at (1,1).

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     Copyright (C) 2008, 2009, 2012 Science and Technology Faciities
*     Council.
*     All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S Berry (JAC)
*     TIMJ: Tim Jenness (JAC)
*     {enter_new_authors_here}

*  History:
*     2006 April 13 (MJC):
*        Original version adapted from COLLAPSE.
*     2006 April 24 (MJC):
*        Added SwitchMap to modify the output NDF's WCS to be
*        three-dimensional to retain the original spatial co-ordinates
*        with each tile, and to give a representative channel
*        co-ordinate to each tile.
*     2006 April 28 (MJC):
*        Removed call to KPS1_CLPA0 and called NDF_PTWCS after creating
*        the SwitchMap.
*     11-MAY-2006 (DSB):
*        Clear up inconsistencies between use of PIXEL and GRID
*        co-ordinates when creating the output WCS FrameSet.
*     2006 May 15 (MJC):
*        Restrict the choice of estimators by the number of pixels per
*        channel.  Remove MINWID restriction.  Provide inverse Mapping
*        for SelectorMap (merger of May 10 changes).
*     2006 May 18 (MJC):
*        Revise for new SelectorMap constructor.
*     2006 May 26 (DSB):
*        Added ROI<n> domain names for each channel.
*     2006 June 9 (MJC):
*        Use a temporary NDF as intermediary to obtain file-size
*        compression of the output NDF.
*     2006 June 14 (MJC):
*        Allow for no WCS component in the supplied cube NDF; create
*        a renamed copy of the current Frame to assure that there is
*        a three-dimensional Frame in the output NDF.
*     22-JUN-2006 (DSB):
*        Ensure that if the original current Frame is PIXEL, AXIS or
*        GRID, then the output contains a suitable 3D current Frame.
*     2007 May 17 (MJC):
*        Correct problems with use of KPG1_WCFAX.
*     2007 July 11 (MJC):
*         The NDF need not have exactly three significant dimensions.
*     2007 July 19 (MJC):
*        Used new KPG1_ASAPA to identify pixel axis corresponding to
*        the collapsed WCS axis, rather than inline code.
*     2007 December 7 (MJC):
*        Revised KPS1_CLPSx API.
*     2007 December 10 (MJC):
*        Newly revised KPS1_CLPSx API.
*     2008 June 17 (MJC):
*        Trim trailing blanks from output NDF character components.
*     2009 July 5 (MJC):
*        Added Cmean and Csigma estimators and an associated CLIP
*        parameter.
*     4-AUG-2009 (DSB):
*        Add FRACTION Frame.
*     2010-09-24 (TIMJ):
*        Increase size of ESTIMO variable to allow all estimators to
*        be selected. 78 characters was too small to hold the full
*        list.
*     2010-12-03 (TIMJ):
*        Use correct bounds during blocking for estimators that
*        require the co-ordinate value.
*     2012 August 3 (MJC):
*        Added "NGood", "NBad", "FGood", and "FBad" estimators.
*     13-FEB-2020 (DSB):
*        Support huge NDFs.
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
      INCLUDE  'MSG_PAR'         ! Message-system reporting levels
      INCLUDE  'PRM_PAR'         ! PRIMDAT constants

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      INTEGER*8 KPG1_FLOOR8      ! Most positive integer .LE. a given
                                 ! real
      INTEGER*8 KPG1_CEIL8       ! Most negative integer .GE. a given
                                 ! real

*  Local Constants:
      REAL CLPDEF                ! Default no. of standard deviations to
      PARAMETER( CLPDEF = 3.0 )  ! clip for mode, clipped mean & std dev

      INTEGER MAXPIX
      PARAMETER ( MAXPIX = 8388608 ) ! Guestimate a size: 8 mega

      INTEGER MAXCHN             ! Maximum number of channels
      PARAMETER ( MAXCHN = 100 )

      INTEGER NDIM               ! Input dimensionality required
      PARAMETER( NDIM = 3 )

*  Local Variables:
      INTEGER*8 AEL              ! Number of collapse axis elements
      CHARACTER AUNITS*( 30 )    ! Units of co-ordinates
      CHARACTER ATTRIB*( 10 )    ! AST attribute name
      INTEGER AXES( NDF__MXDIM ) ! A list of axis indices
      DOUBLE PRECISION AXHIGH    ! High bound of collapse axis in
                                 ! current Frame
      DOUBLE PRECISION AXLOW     ! Low bound of collapse axis in current
                                 ! Frame
      LOGICAL BAD                ! Bad values may be present?
      INTEGER*8 CAEL             ! Number of collapse axis elements in
                                 ! a channel
      INTEGER CBL                ! Identifier for channel block
      INTEGER*8 CBLSIZ( NDIM )   ! Channel-image sizes for processing
                                 ! large datasets in blocks
      INTEGER*8 CDIMS( NDF__MXDIM ) ! Channel image dimensions
      INTEGER CFRM               ! Original Current Frame pointer
      DOUBLE PRECISION CHAVER    ! Average channel PIXEL co-ordinate
      INTEGER*8 CHDIMS( NDIM - 1 ) ! Dimensions of an unblocked channel
                                 ! image
      INTEGER CHIND( NDIM - 1 )  ! Channel image indices within output
                                 ! array
      REAL CLIP                  ! Value of CLIP parameter
      CHARACTER COMP * ( 13 )    ! List of components to process
      INTEGER*8 D                ! A dimension size
      DOUBLE PRECISION DLBNDI( NDIM ) ! Slab inverse lower bounds in
                                 ! GRID collapsed-axis co-ords
      DOUBLE PRECISION DLBNDS( NDIM - 1 ) ! Slab lower bounds in GRID
                                 ! co-ords along uncollapsed axes
      CHARACTER DOM*( 20 )       ! Original current Frame Domain name
      CHARACTER DTYPE*( NDF__SZFTP ) ! Numeric type for output arrays
      DOUBLE PRECISION DUBNDI( NDIM ) ! Inverse slab upper bounds in
                                 ! GRID collapsed-axis co-ords
      DOUBLE PRECISION DUBNDS( NDIM - 1 ) ! Slab upper bounds in GRID
                                 ! co-ords along uncollapsed axes
      INTEGER*8 ELC              ! Number of elements in a channel
                                 ! mapped array
      INTEGER*8 ELI              ! Number of elements in an input mapped
                                 ! array
      INTEGER*8 ELO              ! Number of elements in an output
                                 ! mapped array
      CHARACTER ESTIM*( 6 )      ! Method to use to estimate collapsed
                                 ! values
      CHARACTER ESTIMO*( 112 )   ! List of available estimators
      INTEGER GFRMO              ! Output GRID Frame pointer
      INTEGER GMAP               ! Pointer to Mapping from GRID Frame
                                 ! to Current Frame, input NDF
      LOGICAL HIGHER             ! Significant dimensions above collapse
                                 ! axis?
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Current length of string
      INTEGER IAXIS              ! Index of collapse axis within current
                                 ! Frame
      INTEGER IBL                ! Identifier for input-NDF block
      INTEGER IBLOCK             ! Loop counter for the NDF blocks
      INTEGER*8 IBLSIZ( NDIM )   ! Input-NDF sizes for processing
                                 ! large datasets in blocks
      INTEGER ICH                ! Channel counter
      INTEGER ICURR              ! Index of original current Frame
      INTEGER*8 IERR             ! Position of first numerical error
      INTEGER INDFC              ! NDF identifier for single channel map
      INTEGER INDFI              ! Input NDF identifier
      INTEGER INDFO              ! Output NDF identifier
      INTEGER INDFS              ! Input NDF-section identifier
      INTEGER INDFT              ! Temporary-NDF identifier
      INTEGER INSLAB( MAXCHN )   ! Pointers to inverse collapsed-axis
                                 ! slab intervals for each channel
      INTEGER IPAXCO             ! Pointers to mapped d.p. axis array
      INTEGER IPAXWO             ! Pointers to mapped d.p. axis array
      INTEGER IPCH( 2 )          ! Pointers to mapped channel arrays
      INTEGER IPCO               ! Pointers to mapped co-ordinate array
      INTEGER IPERM( NDIM - 1 )  ! Input permutation
      INTEGER IPIN( 2 )          ! Pointers to mapped input arrays
      INTEGER IPIX               ! Index of PIXEL Frame within input
                                 ! WCS FrameSet
      INTEGER IPIXO              ! Index of PIXEL Frame within output
                                 ! WCS FrameSet
      INTEGER IPOUT( 2 )         ! Pointers to mapped output arrays
      INTEGER IPW1               ! Pointer to first work array
      INTEGER IPW2               ! Pointer to second work array
      INTEGER IPW3               ! Pointer to third work array
      INTEGER IPWID              ! Pointers to mapped width work array
      INTEGER ISEMAP             ! Inverse-SelectorMap pointer
      CHARACTER ITYPE*( NDF__SZTYP ) ! Numeric type for input arrays
      INTEGER IWCS               ! WCS FrameSet pointer
      INTEGER IWCSO              ! Output NDF's WCS FrameSet pointer
      INTEGER J                  ! Loop count
      INTEGER JAXIS              ! Index of collapse axis within PIXEL
                                 ! Frame
      INTEGER*8 JHI              ! High pixel index for collapse axis
      INTEGER*8 JLO              ! Low pixel index for collapse axis
      INTEGER*8 LBND( NDIM )     ! Lower pixel index bounds of the input
                                 ! NDF
      INTEGER*8 LBNDBI( NDIM )   ! Lower pixel index bounds of the
                                 ! cube's block
      INTEGER*8 LBNDBO( NDIM - 1 ) ! Lower pixel index bounds of the
                                 ! channel-map block
      INTEGER*8 LBNDC( NDIM - 1 )! Lower pixel index bounds of the
                                 ! channel section of the input NDF
      INTEGER*8 LBNDO( NDIM - 1 )! Lower pixel index bounds of the
                                 ! output NDF
      INTEGER*8 LBNDS( NDIM )    ! Lower pixel index bounds of the
                                 ! slab of the input NDF
      LOGICAL LOOP               ! Continue to loop through dimensions?
      INTEGER*8 MAXSIZ           ! Maximum size of block along current
                                 ! dimension
      INTEGER MAP                ! Pointer to Mapping from PIXEL Frame
                                 ! to Current Frame, input NDF
      INTEGER MAPC               ! Pointer to Compound Mapping PIXEL
                                 ! 2-D Frame to 3-D Current Frame
      INTEGER MXCHN              ! Maximum number of channels
      INTEGER NAXC               ! Original number of current Frame axes
      INTEGER NBLOCK             ! Number of NDF blocks
      INTEGER NC                 ! Used length of string
      INTEGER ND                 ! Number of dimensions (dummy)
      INTEGER NDIMO              ! Number of pixel axes in output NDF
      INTEGER*8 NERR             ! Number of numerical errors
      CHARACTER NEWDOM*( 20 )    ! Domain name for revised current Frame
      INTEGER NEWFRM             ! Pointer to revised current Frame
      INTEGER*8 NFLAG            ! Number of WLIM-flagged o/p values
      INTEGER NOCHAN             ! Number of channels
      INTEGER NVAL               ! Number of values obtained (1)
      INTEGER*8 ODIMS( NDF__MXDIM ) ! Output NDF dimensions
      INTEGER*8 OFFSET( NDF__MXDIM ) ! Channel-image pixel offsets within
                                 ! output array
      INTEGER OPERM( NDIM )      ! Output permutation
      INTEGER OTOMAP             ! One-to-one mapping
      CHARACTER OTYPE*( NDF__SZTYP ) ! Numeric type for output arrays
      INTEGER PERMAP             ! PermMap pointer
      INTEGER PFRMI              ! Input PIXEL Frame pointer
      REAL PIXPCH                ! Collapse-axis pixels per channel
      INTEGER PLACE              ! NDF placeholder
      DOUBLE PRECISION PXHIGH    ! High pixel bound of collapse axis
      DOUBLE PRECISION PXLOW     ! Low pixel bound of collapse axis
      CHARACTER ROIDOM*20        ! Domain name for an ROI Region
      INTEGER ROUMAP( MAXCHN )   ! Route maps for each channel
      INTEGER SDIM( NDIM )       ! Significant dimensions
      INTEGER SELMAP             ! SelectorMap pointer
      INTEGER SHAPE( 2 )         ! Number of channel maps per axis
      DOUBLE PRECISION SHIFTS( NDIM - 1 ) ! Shifts from output origin
                                 ! to current tile's origin
      INTEGER SHIMAP             ! SwitchMap pointer
      INTEGER SLABIN( MAXCHN )   ! Pointers to spatial-pixel slab
                                 ! Intervals for each channel
      INTEGER SWIMAP             ! SwitchMap pointer
      CHARACTER TTL*( 255 )      ! Tile title
      CHARACTER TTLC*( 255 )     ! Title of original current Frame
      INTEGER*8 UBND( NDIM )     ! Upper pixel index bounds of the input
                                 ! NDF
      INTEGER*8 UBNDBI( NDIM )   ! Upper pixel index bounds of the
                                 ! cube's block
      INTEGER*8 UBNDBO( NDIM - 1 ) ! Upper pixel index bounds of the
                                 ! channel-map block
      INTEGER*8 UBNDC( NDIM - 1 )! Upper pixel index bounds of the
                                 ! channel section of the input NDF
      INTEGER*8 UBNDO( NDIM - 1 )! Upper pixel index bounds of the
                                 ! output NDF
      INTEGER*8 UBNDS( NDIM )    ! Upper pixel index bounds of the
                                 ! slab of the input NDF
      INTEGER UMAP               ! A two-dimensional UnitMap
      CHARACTER UNITS*( 60 )     ! Units of data
      LOGICAL USEALL             ! Use the entire collapse pixel axis?
      LOGICAL VAR                ! Process variances?
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

*  Get an AST pointer to a FrameSet describing the co-ordinate Frames
*  present in the NDF's WCS component.  Modify it to ensure that the
*  Base, PIXEL and Current frames all have three dimensions.  The NDF
*  need not have exactly three significant dimensions (i.e. axes
*  spanning more than one pixel).
      CALL KPG1_ASGET8( INDFI, NDIM, .FALSE., .TRUE., .TRUE., SDIM,
     :                  LBND, UBND, IWCS, STATUS )

*  Extract the current and base Frames, and get the number of axes in
*  the current Frame, and its title.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      NAXC = AST_GETI( CFRM, 'NAXES', STATUS )
      TTLC = AST_GETC( CFRM, 'TITLE', STATUS )

*  While the NDF system makes GRID, FRACTION, AXIS, and PIXEL Frames
*  accessible, even if there is no WCS component in the suppled NDF,
*  NDF_PTWCS (called later) strips out any Frames with Domain names GRID,
*  FRACTION, AXIS, or PIXEL, regardless of how many axes those Frames have.
*  We must therefore add a new Frame with a different Domain name to preserve
*  the three-dimensional information in the two-dimensional NDF.
      DOM = AST_GETC( CFRM, 'Domain', STATUS )
      IF ( DOM .EQ. 'GRID' .OR. DOM .EQ. 'AXIS' .OR.
     :     DOM .EQ. 'PIXEL' .OR. DOM .EQ. 'FRACTION' ) THEN
         NEWDOM = '3D'
         IAT = 2
         CALL CHR_APPND( DOM, NEWDOM, IAT )
         NEWFRM = AST_COPY( CFRM, STATUS )
         CALL AST_SETC( NEWFRM, 'Domain', NEWDOM, STATUS )

         CALL AST_ADDFRAME( IWCS, AST__CURRENT,
     :                      AST_UNITMAP( 3, ' ', STATUS ), NEWFRM,
     :                      STATUS )
      END IF

*  Find the index of the PIXEL Frame.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Extract the Mapping from PIXEL Frame to Current Frame.
      MAP = AST_GETMAPPING( IWCS, IPIX, AST__CURRENT, STATUS )

*  Report an error if the Mapping is not defined in either direction.
      IF ( .NOT. AST_GETL( MAP, 'TRANINVERSE', STATUS ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDFI )
         CALL MSG_SETC( 'T', TTLC )
         CALL ERR_REP( 'CHANMAP_ERR1', 'The transformation from the '//
     :                 'current co-ordinate Frame of ''^NDF'' '//
     :                 '(^T) to pixel co-ordinates is not defined.',
     :                 STATUS )

      ELSE IF ( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) .AND.
     :         STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDFI )
         CALL MSG_SETC( 'T', TTLC )
         CALL ERR_REP( 'CHANMAP_ERR2', 'The transformation from '/
     :                 /'pixel co-ordinates to the current '/
     :                 /'co-ordinate Frame of ''^NDF'' (^T) is not '/
     :                 /'defined.', STATUS )
      END IF

*  Extract the Mapping from GRID Frame to Current Frame.
      GMAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )

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
            CALL ERR_REP( 'CHANMAP_ERR4', 'The axis range to '/
     :                    /'collapse covers zero pixels (are the '/
     :                    /'HIGH and LOW parameter values equal '/
     :                    /'or outside the bounds of the NDF?)',
     :                    STATUS )
            GO TO 999
         END IF

      END IF

*  Tell the user the range of pixels being collapsed.
      CALL MSG_SETI( 'I', JAXIS )
      CALL MSG_SETK( 'L', JLO )
      CALL MSG_SETK( 'H', JHI )
      CALL MSG_OUTIF( MSG__NORM, 'CHANMAP_MSG1',
     :               '   Forming channel map along pixel axis ^I '/
     :               /'between pixel ^L to pixel ^H inclusive.',
     :               STATUS )
      CALL MSG_BLANK( STATUS )
      AEL = JHI - JLO + 1

*  Set the bounds and dimensions of a single tile in the map.
*  ==========================================================

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
         LBNDC( I ) = LBND( AXES( I ) )
         UBNDC( I ) = UBND( AXES( I ) )
         CHDIMS( I ) = UBNDC( I ) - LBNDC( I ) + 1
      END DO

*  Obtain the number of channels and their arrangement.
*  ====================================================

*  The constraints are that the values are positive, and each
*  channel must have at least one pixel. Take care about integer kinds
*  when calculating MXCHN (can't just use MIN).
      IF( AEL .LT. MAXCHN ) THEN
         MXCHN = AEL
      ELSE
         MXCHN = MAXCHN
      END IF
      CALL PAR_GDR0I( 'NCHAN', 6, 1, MXCHN, .FALSE., NOCHAN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      CALL PAR_GDR0I( 'SHAPE', 4, 1, NOCHAN, .FALSE., SHAPE( 1 ),
     :                STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  The aspect ratio is 1.0 and the tiles abut.
         CALL KPS1_CHSHA( NOCHAN, CHDIMS, 1.0, 0.0, SHAPE, STATUS )
      ELSE
         SHAPE( 2 ) = ( NOCHAN - 1 ) / SHAPE( 1 ) + 1
      END IF

*  Define the output NDF's bounds.
*  ===============================

*  Determine whether or not there are significant dimensions above
*  the collapse axis.
      HIGHER = JAXIS .NE. NDIM
      IF ( HIGHER ) THEN
         HIGHER = .FALSE.
         DO I = JAXIS + 1, NDIM
            HIGHER = HIGHER .OR. ( UBND( I ) - LBND( I ) ) .NE. 0
         END DO
      END IF

*  Define the shape of the channel-map image.  The original bounds
*  cannot be retained.
      DO I = 1, NDIMO
         LBNDO( I ) = 1
         UBNDO( I ) = CHDIMS( I ) * SHAPE( I )
         ODIMS( I ) = UBNDO( I )
      END DO

*  Pasting needs aray dimensions and offsets with NDF__MXDIM elements.
      DO I = NDIM, NDF__MXDIM
         OFFSET( I ) = 0
         CDIMS( I ) = 1
         ODIMS( I ) = 1
      END DO

*  Propagate the input to a temporary NDF.
*  =======================================

*  We want to avoid creating an NDF of roughly the same size as the
*  input, when there's been significant compression of the number of
*  pixels.  If we merely propagate then adjust the dimensions, HDS does
*  not free up the space initially allocated.  Therefore we create a
*  temporary NDF, and once its dimensions are correct, we can copy it
*  to the actual output NDF.
      CALL NDF_TEMP( PLACE, STATUS )

*  Create the temporary NDF by propagation from the input NDF.  This
*  results in history, etc., being passed on.  The shape and
*  dimensionality will be wrong but this will be corrected later.
      CALL NDF_SCOPY( INDFI, 'Units', PLACE, INDFT, STATUS )

*  Adjust output NDF to its new shape.
*  ===================================

*  The shape and size of the output NDF created above will be wrong, so
*  we need to correct it by removing the collapse axis.  To avoid
*  sawtooth axis centres that would be unpalatable to many tasks, it's
*  better to remove the AXIS structures, and use the WCS to handle the
*  repeating spatial co-ordinates.  We shall create the basic NDF
*  WCS, and add the SwitchMap Frame at the end.

*  Set the output NDF bounds to the required values.
      CALL NDF_SBND8( NDIMO, LBNDO, UBNDO, INDFT, STATUS )

*  Now copy from the adjusted temporary NDF to the user-specified
*  output NDF, that should have the correct file size.  As the
*  temporary NDF is then no longer required, release its resources.
      CALL LPG_PROP( INDFT, 'Units', 'OUT', INDFO, STATUS )
      CALL NDF_ANNUL( INDFT, STATUS )

*  Set the title of the output NDF.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', INDFI, INDFO, STATUS )

*  See if the input NDF has a Variance component.
      CALL NDF_STATE( INDFI, 'VARIANCE', VAR, STATUS )

*  Store a list of components to be accessed.
      IF ( VAR ) THEN
         COMP = 'DATA,VARIANCE'
      ELSE
         COMP = 'DATA'
      END IF

*  Determine the numeric type to be used for processing the input
*  data and variance (if any) arrays.  Since the subroutines that
*  perform the collapse need the data and variance arrays in the same
*  data type, the component list is used.  This application supports
*  single- and double-precision floating-point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', INDFI, INDFO, COMP, ITYPE, DTYPE,
     :                STATUS )

*  Get the WCS FrameSet from the output NDF.
      CALL NDF_GTWCS( INDFO, IWCSO, STATUS )

*  Find the index of the PIXEL Frame in the output FrameSet.
      CALL KPG1_ASFFR( IWCSO, 'PIXEL', IPIXO, STATUS )

*  Obtain the input PIXEL Frame.
      PFRMI = AST_GETFRAME( IWCS, IPIX, STATUS )

*  Obtain the output GRID Frame.
      GFRMO = AST_GETFRAME( IWCSO, AST__BASE, STATUS )

*  Obtain the remaining parameters.
*  ================================

*  Let's define the number of pixels per channel.  First of all it's
*  needed to restrict the options when its value is small.
      PIXPCH = REAL( AEL ) / REAL( NOCHAN )
      IF ( INT( PIXPCH, 8 ) .GT. 3 ) THEN
         ESTIMO = 'Mean,WMean,Mode,Median,Max,Min,Comax,Comin,Absdev,'/
     :            /'Cmean,Csigma,RMS,Sigma,Sum,Iwc,Iwd,Integ,FBad,'/
     :            /'FGood,NBad,NGood'
      ELSE IF ( INT( PIXPCH, 8 ) .EQ. 1 ) THEN
         ESTIMO = 'Mean,Max,Min,Comax,Comin,Sum,Iwc,Integ'
      ELSE IF ( INT( PIXPCH, 8 ) .EQ. 2 ) THEN
         ESTIMO = 'Mean,WMean,Max,Min,Comax,Comin,Absdev,Sum,Iwc,'/
     :            /'Integ,FBad,FGood,NBad,NGood'
      ELSE IF ( INT( PIXPCH, 8 ) .EQ. 3 ) THEN
         ESTIMO = 'Mean,WMean,Median,Max,Min,Comax,Comin,Absdev,Sum,'/
     :            /'Iwc,Integ,FBad,FGood,NBad,NGood'
      END IF

*  Get the ESTIMATOR and WLIM parameters.
      CALL PAR_CHOIC( 'ESTIMATOR', 'Mean', ESTIMO,
     :                .FALSE., ESTIM, STATUS )

      CALL PAR_GDR0R( 'WLIM', 0.3, 0.0, 1.0, .FALSE., WLIM, STATUS )

*  For now obtain just a single number of standard deviations at which
*  to clip.
      IF ( ESTIMO .EQ. 'MODE' .OR. ESTIMO .EQ. 'CMEAN' .OR.
     :     ESTIMO .EQ. 'CSIGMA' ) THEN
         CALL PAR_GDR0R( 'CLIP', CLPDEF, VAL__SMLR, VAL__MAXR, .FALSE.,
     :                   CLIP, STATUS )
      ELSE
         CLIP = CLPDEF
      END IF

*  Adjust output data type if required.
*  ====================================

*  The NBad and NGood estimators always produce _INTEGER output NDFs.
      IF ( ESTIM .EQ. 'NGOOD' .OR. ESTIM .EQ. 'NBAD' ) THEN
         CALL NDF_RESET( INDFO, COMP, STATUS )
         CALL NDF_STYPE( '_INTEGER', INDFO, COMP, STATUS )
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

*  Prepare for the channel-map loop.
*  =================================

*  Map the channel map.
      CALL KPG1_MAP8( INDFO, COMP, OTYPE, 'WRITE', IPOUT, ELO, STATUS )

*  There may be bad pixels present.
      BAD = .TRUE.

*  Set the pixel bounds of the slab NDF.
      DO I = 1, NDIM
         LBNDS( I ) = LBND( I )
         UBNDS( I ) = UBND( I )
      END DO
      UBNDS( JAXIS ) = JLO - 1

*  Inside the loop we'll need the permutation of the axes to create
*  a two-dimensional to three-dimensional Mapping, with a constant
*  along the third axis, being a representative collapsed-axis
*  PIXEL co-ordinate (indicated by the negative axis).
      J = 0
      DO I = 1, NDIM
         IF ( I .NE. JAXIS ) THEN
            J = J + 1
            IPERM( J ) = J
            OPERM( I ) = J
         END IF
      END DO
      OPERM( JAXIS ) = -1

*  Make a temporary NDF to store a single channel's image.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW8( ITYPE, NDIMO, LBNDC, UBNDC, PLACE, INDFC, STATUS )

*  Iterate through the channels.
      DO ICH = 1, NOCHAN

*  See the bounds of the channel along the collapse axis.  Strictly
*  we should divide the co-ordinates limits of the current WCS Frame
*  equally and convert those channel limits to pixels.
         LBNDS( JAXIS ) = UBNDS( JAXIS ) + 1
         UBNDS( JAXIS ) = JLO - 1 + INT( PIXPCH * REAL( ICH ), 8 )
         CAEL = UBNDS( JAXIS ) - LBNDS( JAXIS ) + 1

*  Obtain the indices of the tile within the large output
*  two-dimensional array.
         CHIND( 1 ) = MOD( ICH - 1, SHAPE( 1 ) ) + 1
         CHIND( 2 ) = ( ICH - 1 ) / SHAPE( 1 ) + 1

*  Process in blocks.
*  ==================

*  For large datasets, there may be insufficient memory.  Therefore
*  we form blocks to process, one at a time.  For this by definition
*  we need the collapse-axis pixels to always be present in full for
*  each pixel along the other pixel axes.  If this leaves room for a
*  full span of a dimension that becomes the block size along that
*  axis.  Partial fills take the remaining maximum size and subsequent
*  dimensions' block sizes are unity.
         IBLSIZ( JAXIS ) = CAEL
         MAXSIZ = MAX( 1, MAXPIX / CAEL )
         LOOP = .TRUE.
         J = 0
         DO I = 1, NDIM
            IF ( I .NE. JAXIS ) THEN
               IF ( LOOP ) THEN
                  D = UBNDS( I ) - LBNDS( I ) + 1
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
               CBLSIZ( J ) = IBLSIZ( I )
            END IF
         END DO

*  The channel limits have reduced the collapsed section from the
*  whole collapsed dimension, then we cannot use the original input
*  NDF to derive the number of blocks.  Instead we create a subsection
*  spanning the actual collapse limits, as if the user had supplied
*  this section with the input NDF.
         CALL NDF_SECT8( INDFI, NDIM, LBNDS, UBNDS, INDFS, STATUS )

*  Determine the number of blocks.
         CALL NDF_NBLOC8( INDFS, NDIM, IBLSIZ, NBLOCK, STATUS )

*  Loop through each block.  Start a new NDF context.
         DO IBLOCK = 1, NBLOCK
            CALL NDF_BEGIN
            CALL NDF_BLOCK8( INDFS, NDIM, IBLSIZ, IBLOCK, IBL, STATUS )
            CALL NDF_BLOCK8( INDFC, NDIMO, CBLSIZ, IBLOCK, CBL, STATUS )

*  Map the NDF arrays and workspace required.
*  ==========================================

*  Map the full input, and output data and (if needed) variance arrays.
            CALL NDF_MAP8( IBL, COMP, ITYPE, 'READ', IPIN, ELI, STATUS )
            CALL NDF_MAP8( CBL, COMP, ITYPE, 'WRITE', IPCH, ELC,
     :                     STATUS )

            IF ( .NOT. VAR ) THEN
               IPIN( 2 ) = IPIN( 1 )
               IPCH( 2 ) = IPCH( 1 )
            END IF

*  Obtain the bounds of the blocks.
            CALL NDF_BOUND8( IBL, NDIM, LBNDBI, UBNDBI, ND, STATUS )
            CALL NDF_BOUND8( CBL, NDIMO, LBNDBO, UBNDBO, ND, STATUS )

*  Allocate work space, unless the last axis is being collapsed (in
*  which case no work space is needed).
            IF ( HIGHER ) THEN
               CALL PSX_CALLOC8( ELC * CAEL, ITYPE, IPW1, STATUS )
               IF ( VAR ) THEN
                  CALL PSX_CALLOC8( ELC * CAEL, ITYPE, IPW2, STATUS )
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
     :           ESTIM .EQ. 'IWC' .OR. ESTIM .EQ. 'IWD' .OR.
     :           ESTIM .EQ. 'INTEG' ) THEN

*  Create workspace for the co-ordinates along a single WCS axis
*  in the correct data type.
               CALL PSX_CALLOC8( ELI, '_DOUBLE', IPAXCO, STATUS )
               CALL PSX_CALLOC8( ELI, ITYPE, IPCO, STATUS )
               CALL PSX_CALLOC8( UBNDBI( JAXIS ) - LBNDBI( JAXIS ) + 1,
     :                          '_DOUBLE', IPAXWO, STATUS )


*  Allocate work space, unless the last pixel axis is being collapsed
*  (in which case no work space is needed).
               IF ( HIGHER ) THEN
                  CALL PSX_CALLOC8( ELC * CAEL, ITYPE, IPW3, STATUS )
               END IF

*  Obtain the double-precision co-ordinate centres along the collapse
*  axis in the current Frame.
               CALL KPG1_WCFAX8( LBNDBI, UBNDBI, MAP, JAXIS, IAXIS,
     :                           %VAL( CNF_PVAL( IPAXCO ) ),
     :                           %VAL( CNF_PVAL( IPAXWO ) ), STATUS )


*  Copy the centres to the required precision.
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL VEC8_DTOR( .TRUE., ELI,
     :                            %VAL( CNF_PVAL( IPAXCO ) ),
     :                            %VAL( CNF_PVAL( IPCO ) ), IERR, NERR,
     :                            STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL VEC8_DTOD( .TRUE., ELI,
     :                            %VAL( CNF_PVAL( IPAXCO ) ),
     :                            %VAL( CNF_PVAL( IPCO ) ), IERR, NERR,
     :                            STATUS )

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

*  Allocate work space for thw widths to be derived from the
*  co-ordinates.  This assumes full filling of pixels.
               CALL PSX_CALLOC8( ELC * CAEL, ITYPE, IPWID, STATUS )

*  Store safe pointer value if widths are not needed.
            ELSE
               IPWID = IPIN( 1 )
            END IF

*  Collapse.
*  =========

*  Now do the work, using a routine appropriate to the numeric type.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_CLPSR( JAXIS, LBNDS( JAXIS ), UBNDS( JAXIS ),
     :                          VAR, ESTIM, WLIM, CLIP,
     :                          ELC, NDIM, LBNDBI, UBNDBI,
     :                          %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                          %VAL( CNF_PVAL( IPIN( 2 ) ) ),
     :                          %VAL( CNF_PVAL( IPCO ) ),
     :                          NDIMO, LBNDBO, UBNDBO, HIGHER, NFLAG,
     :                          %VAL( CNF_PVAL( IPCH( 1 ) ) ),
     :                          %VAL( CNF_PVAL( IPCH( 2 ) ) ),
     :                          %VAL( CNF_PVAL( IPWID ) ),
     :                          %VAL( CNF_PVAL( IPW1 ) ),
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          %VAL( CNF_PVAL( IPW3 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_CLPSD( JAXIS, LBNDS( JAXIS ), UBNDS( JAXIS ),
     :                          VAR, ESTIM, WLIM, CLIP,
     :                          ELC, NDIM, LBNDBI, UBNDBI,
     :                          %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                          %VAL( CNF_PVAL( IPIN( 2 ) ) ),
     :                          %VAL( CNF_PVAL( IPCO ) ),
     :                          NDIMO, LBNDBO, UBNDBO, HIGHER, NFLAG,
     :                          %VAL( CNF_PVAL( IPCH( 1 ) ) ),
     :                          %VAL( CNF_PVAL( IPCH( 2 ) ) ),
     :                          %VAL( CNF_PVAL( IPWID ) ),
     :                          %VAL( CNF_PVAL( IPW1 ) ),
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          %VAL( CNF_PVAL( IPW3 ) ), STATUS )

            ELSE IF ( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'T', ITYPE )
               CALL ERR_REP( 'CHANMAP_ERR5', 'CHANMAP: Unsupported '/
     :                       /'data type ^T (programming error).',
     :                       STATUS )
            END IF

*  Free the work space.
            IF ( HIGHER ) THEN
               CALL PSX_FREE( IPW1, STATUS )
               IF ( VAR ) CALL PSX_FREE( IPW2, STATUS )
            END IF

            IF ( ESTIM .EQ. 'COMAX' .OR. ESTIM .EQ. 'COMIN' .OR.
     :           ESTIM .EQ. 'IWC' .OR. ESTIM .EQ. 'IWD' ) THEN
                CALL PSX_FREE( IPCO, STATUS )
                IF ( HIGHER ) CALL PSX_FREE( IPW3, STATUS )
            END IF

            IF ( ESTIM .EQ. 'INTEG' ) THEN
               CALL PSX_FREE( IPWID, STATUS )
               IF ( HIGHER ) CALL PSX_FREE( IPW3, STATUS )
            END IF

*  Derive the offsets of the original input NDFs with respect to the
*  origin of the output NDF.  Also extract the dimensions of the
*  current NDF.
            DO J = 1, NDIMO
               OFFSET( J ) = ( CHIND( J ) - 1 ) * CHDIMS( J ) +
     :                       LBNDBO( J ) - LBNDC( J )
               CDIMS( J ) = UBNDBO( J ) - LBNDBO( J ) + 1
            END DO

*  Paste the data array.
*  =====================

*  Call the appropriate routine that performs the pasting of the data
*  array.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_PAST8R( .FALSE., BAD, OFFSET, CDIMS, ELC,
     :                           %VAL( CNF_PVAL( IPCH( 1 ) ) ),
     :                           ODIMS, ELO,
     :                           %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                           STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_PAST8D( .FALSE., BAD, OFFSET, CDIMS, ELC,
     :                           %VAL( CNF_PVAL( IPCH( 1 ) ) ),
     :                           ODIMS, ELO,
     :                           %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                           STATUS )

            END IF

*  Paste the variance array.
*  =========================
            IF ( VAR ) THEN

*  Call the appropriate routine that performs the pasting of the data
*  array.
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_PAST8R( .FALSE., BAD, OFFSET, CDIMS, ELC,
     :                              %VAL( CNF_PVAL( IPCH( 2 ) ) ),
     :                              ODIMS, ELO,
     :                              %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                              STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPG1_PAST8D( .FALSE., BAD, OFFSET, CDIMS, ELC,
     :                              %VAL( CNF_PVAL( IPCH( 2 ) ) ),
     :                              ODIMS, ELO,
     :                              %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                              STATUS )
               END IF
            END IF

*  Close NDF context.
            CALL NDF_END( STATUS )
         END DO

*  Create WCS Frame
*  ================

*  Each tile of the channel map should have three co-ordinates: the
*  spatial co-ordinates, that are the same for each tile; and a
*  constant co-ordinate representative of the channel, currently
*  the average of the channel bounds.  The goal is to be able to
*  inquire the spatial and spectral co-ordinates of a feature
*  (with a task like CURSOR).  The steps involved are as follows.
*
*  For each channel tile (i.e. ignoring any blocking) we proceed as
*  below.
*  a) Create a `route map' transforming two-dimensional GRID
*  co-ordinates in the output array into three-dimensional GRID
*  co-ordinates in the input cube.  This comprises a shift of origin of
*  the spatial GRID co-ordinates from the lower-left of the output
*  array to the lower-left of the current tile, combined with a
*  conversion to three dimensions, using a constant---the average
*  co-ordinate along the collapsed axis---for the third dimension; and
*  b) Create Intervals using the range of GRID co-ordinates along
*  non-collapsed axes.
*  c) Create Intervals using the range of GRID co-ordinates of each
*  channel.

*  Once all the tiles have been pasted into the output array the
*  steps are as follows.
*  d) Form a SelectorMap using the array of spatial Intervals from b).
*  e) Form an inverse SelectorMap using the array of collapsed-axis
*  Intervals from c).
*  f) In turn form a SwitchMap using the forward and inverse
*  SelectorMaps from steps d) and e), and route maps from step a).
*  g) Create a compound Mapping of the SwitchMap and the original
*  three-dimensional GRID-to-current Frame Mapping.  The result maps
*  from two-dimensional GRID co-ordinates to the input current Frame.
*  h) Add a new Frame to the output FrameSet using the Mapping
*  from step g) to connect the FrameSet to the two-dimensional GRID
*  Frame.

*  Create a ShiftMap from two-dimensional GRID co-ordinates in the large
*  output file (i.e. for the ICHth tile) to two-dimensional GRID
*  co-ordinates in the original cube.
         DO J = 1, NDIMO
            SHIFTS( J ) = -DBLE( ( CHIND( J ) - 1 ) * CHDIMS( J ) )
         END DO
         SHIMAP = AST_SHIFTMAP( NDIMO, SHIFTS, ' ', STATUS )

*  Create a PermMap increasing the dimensionality by one and setting
*  the new dimension position to the average GRID position (within the
*  input cube) of the slab.
         CHAVER = DBLE( UBNDS( JAXIS ) + LBNDS( JAXIS ) - 1 ) * 0.5D0
     :            - DBLE( LBND( JAXIS ) ) + 1.5D0
         PERMAP = AST_PERMMAP( NDIMO, IPERM, NDIM, OPERM, CHAVER, ' ',
     :                         STATUS )

*  Combine the ShiftMap and the PermMap to form the route map for the
*  current tile.  The forward transformation of this Mapping goes from
*  two-dimensional GRID co-ordinates in the output image to
*  three-dimensional GRID co-ordinates in the input cube.
         ROUMAP( ICH ) = AST_CMPMAP( SHIMAP, PERMAP, .TRUE., ' ',
     :                               STATUS )

*  Create an Interval describing the region within the output
*  two-dimensional GRID Frame occupied by this tile.  Note that this
*  would need changing if the tiles did not abut.  Also the bounds are
*  double precision.
         DO J = 1, NDIMO
             DLBNDS( J ) = DBLE( ( CHIND( J ) - 1 ) * CHDIMS( J ) ) +
     :                     0.5D0
             DUBNDS( J ) = DBLE( CHIND( J ) * CHDIMS( J ) ) + 0.5D0 -
     :                     1.0D-10
         END DO

         SLABIN( ICH ) = AST_INTERVAL( GFRMO, DLBNDS, DUBNDS, AST__NULL,
     :                                 ' ', STATUS )

*  Create the inverse Interval.   The non-collapsed axes are unbounded.
*  The collapsed axis is bounded by the GRID range of each tile.
         DO J = 1, NDIM
             DLBNDI( J ) = AST__BAD
             DUBNDI( J ) = AST__BAD
         END DO
         DLBNDI( JAXIS ) = DBLE( LBNDS( JAXIS ) - LBND( JAXIS ) ) +
     :                     0.5D0
         DUBNDI( JAXIS ) = DBLE( UBNDS( JAXIS ) - LBND( JAXIS ) ) +
     :                     1.5D0

         INSLAB( ICH ) = AST_INTERVAL( PFRMI, DLBNDI, DUBNDI, AST__NULL,
     :                                 ' ', STATUS )

*  Free up the AST resources we don't need to retain outside of the
*  channel loop.
         CALL AST_ANNUL( PERMAP, STATUS )
         CALL AST_ANNUL( SHIMAP, STATUS )
      END DO

*  Create the SelectorMap and the inverse SelectorMap, and hence the
*  SwitchMap.  For the inverse selector Mapping, indicate that positions
*  that are bad on any axis should be assigned to the first tile.  This
*  can help if (say) bad values are assigned to the spectral axis as a
*  result of displaying only the spatial axes.
      SELMAP = AST_SELECTORMAP( NOCHAN, SLABIN, AST__BAD, ' ', STATUS )
      ISEMAP = AST_SELECTORMAP( NOCHAN, INSLAB, 1.0D0, ' ', STATUS )
      CALL AST_INVERT( ISEMAP, STATUS )

      SWIMAP = AST_SWITCHMAP( SELMAP, ISEMAP, NOCHAN, ROUMAP,
     :                        ' ', STATUS )

*  Combine in sequence the SwitchMap (that goes from two-dimensional
*  GRID to three-dimensional GRID) with the Mapping from
*  three-dimensional GRID co-ordinates to the original
*  three-dimensional current Frame.
      MAPC = AST_CMPMAP( SWIMAP, GMAP, .TRUE., ' ', STATUS )

*  Add the input three-dimensional current Frame into the output
*  FrameSet, using the above Mapping to connect it to the
*  two-dimensional GRID Frame.
      CALL AST_ADDFRAME( IWCSO, AST__BASE, MAPC,
     :                   AST_GETFRAME( IWCS, AST__CURRENT, STATUS ),
     :                   STATUS )

*  Make a two-dimensional UnitMap that will be used in the following
*  loop.
      UMAP = AST_UNITMAP( 2, ' ', STATUS )

*  Note the index of the current Frame.
      ICURR = AST_GETI( IWCSO, 'Current', STATUS )

*  Loop round the Regions defined above that describe the area of each
*  tile in GRID co-ordinates, and add each one into the WCS FrameSet.
      DO ICH = 1, NOCHAN

*  Set the Domain name to "ROI<n>" where <n> is the index of the tile.
*  This enables KPG1_ASTRM to identify the Region as defining a Region
*  Of Interest.
         ROIDOM = 'ROI'
         IAT = 3
         CALL CHR_PUTI( ICH, ROIDOM, IAT )
         CALL AST_SETC( SLABIN( ICH ), 'Domain', ROIDOM( : IAT ),
     :                  STATUS )

*  Set a meaningful title describing the tile.  This would be displayed,
*  for instance, by "NDFTRACE FULLWCS".
         CALL AST_GETREGIONBOUNDS( SLABIN( ICH ), DLBNDS, DUBNDS,
     :                             STATUS )

         TTL = 'Tile '
         IAT = 5
         CALL CHR_PUTI( ICH, TTL, IAT )
         CALL CHR_APPND( ' bounds: (', TTL, IAT )
         CALL CHR_PUTK( NINT( DLBNDS( 1 ), KIND=8 ), TTL, IAT )
         CALL CHR_APPND( ':', TTL, IAT )
         CALL CHR_PUTK( NINT( DUBNDS( 1 ), KIND=8 ), TTL, IAT )
         CALL CHR_APPND( ',', TTL, IAT )
         CALL CHR_PUTK( NINT( DLBNDS( 2 ), KIND=8 ), TTL, IAT )
         CALL CHR_APPND( ':', TTL, IAT )
         CALL CHR_PUTK( NINT( DUBNDS( 2 ), KIND=8 ), TTL, IAT )
         CALL CHR_APPND( ')', TTL, IAT )

         CALL AST_SETC( SLABIN( ICH ), 'Title', TTL( : IAT ),
     :                  STATUS )

*  Add the above Region into the output WCS FrameSet, using a UnitMap to
*  connect it to the base (i.e. GRID) Frame.
         CALL AST_ADDFRAME( IWCSO, AST__BASE, UMAP, SLABIN( ICH ),
     :                      STATUS )
      END DO

*  Reinstate the original current Frame.
      CALL AST_SETI( IWCSO, 'Current', ICURR, STATUS )

*  Save this modified WCS FrameSet in the output NDF.
      CALL NDF_PTWCS( IWCSO, INDFO, STATUS )

*  Come here if something has gone wrong.
  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Report a contextual message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CHANMAP_ERR6', 'CHANMAP: Unable to form '/
     :                 /'a channel-map NDF.', STATUS )
      END IF

      END
