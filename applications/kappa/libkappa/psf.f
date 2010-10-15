      SUBROUTINE PSF( STATUS )
*+
*  Name:
*     PSF

*  Purpose:
*     Determines the parameters of a model star profile by fitting star
*     images in a two-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PSF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application finds a set of parameters to describe a model
*     Gaussian star image.  It can be used for profile-fitting stellar
*     photometry, to evaluate correction terms to aperture photometry,
*     or for filtering.
*
*     The model has a radial profile:
*
*        D =  A exp(-0.5 * (r/sigma) ** gamma )
*
*     where r is calculated from the true radial distance from the star
*     centre allowing for image ellipticity, sigma is the Gaussian
*     precision constant or profile width.  The application combines a
*     number of star images you specify and determines a mean
*     seeing-disc size, radial fall-off parameter (gamma), axis ratio,
*     and orientation of a model star image.
*
*     A table, giving details of the seeing and ellipticity of each
*     star image used can be reported to an output text file.  This
*     table indicates if any star could not be used.  Reasons for
*     rejecting stars are too-many bad pixels present in the image,
*     the star is too close to the edge of the data array, the
*     `star' is a poor fit to model or it could not be located.
*
*     An optional plot of the mean profile and the fitted function may
*     be produced.  The two-dimensional point-spread function may be
*     stored in an NDF for later use, as may the one-dimensional fitted
*     profile.

*  Usage:
*     psf in incat [device] [out] [cut] [range] [isize] [poscols]

*  ADAM Parameters:
*     AMP1 = _REAL (Write)
*        The fitted peak amplitude of the first usable star, in the data
*        units of the input NDF.
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        plot. The width of the margins left for the annotation may be
*        controlled using Parameter MARGIN.  The appearance of the axes
*        (colours, fonts, etc) can be controlled using the Parameter
*        STYLE. [TRUE]
*     AXISR = _REAL (Write)
*        The axis ratio of the star images: the ratio of the major
*        axis length to that of the minor axis.
*     CENTRE = LITERAL (Write)
*        The formatted co-ordinates of the first fitted star position,
*        in the current Frame of the NDF.
*     CLEAR = _LOGICAL (Read)
*        If TRUE the current picture is cleared before the plot is
*        drawn.  If CLEAR is FALSE not only is the existing plot
*        retained, but also an attempt is made to align the new picture
*        with the existing picture.  Thus you can generate a composite
*        plot within a single set of axes, say using different colours
*        or modes to distinguish data from different datasets.  [TRUE]
*     COFILE = FILENAME (Read)
*        Name of a text file containing the co-ordinates of the stars
*        to be used.  It is only accessed if Parameter INCAT is given a
*        null (!) value.  Each line should contain the formatted axis
*        values for a single position, in the current Frame of the NDF.
*        Columns can be separated by spaces, tabs or commas.  The file
*        may contain comment lines with the first character # or !.
*        Other columns may be included in the file, in which case the
*        columns holding the required co-ordinates should be specified
*        using Parameter POSCOLS.
*     CUT = _REAL (Read)
*        This parameter controls the size of the output NDF.  If it is
*        null, !, the dimension of the square NDF will be the size of
*        the region used to calculate the radial profile, which usually
*        is given by RANGE * width in pixels * AXISR, unless truncated.
*        If CUT has a value it is the threshold which must be included
*        in the PSF NDF, and it is given as the fraction of the peak
*        amplitude of the PSF.  For example, if CUT=0.5 the NDF would
*        contain the point-spread function to half maximum.  CUT must
*        be greater than 0 and less than 1.  The suggested default is
*        0.0001.  [!]
*     DEVICE = DEVICE (Read)
*        The graphics workstation on which to produce a plot of the
*        mean radial profile of the stars and the fitted function.  A
*        null (!) name indicates that no plot is required.
*        [current graphics device]
*     FWHM = _REAL (Write)
*        The seeing-disc size: the full width at half maximum across the
*        minor axis of the stars.  It is in units defined by the current
*        Frame of the NDF. For instance, a value in arcseconds will be
*        reported if the current Frame is a SKY Frame, but pixels will
*        be used if it is a PIXEL Frame.
*     GAMMA = _REAL (Write)
*        The radial fall-off parameter of the star images. See the
*        description for more details.  A gamma of two would be a
*        Gaussian.
*     GAUSS = _LOGICAL (Read)
*        If TRUE, the gamma coefficient is fixed to be 2; in other words
*        the best-fitting two-dimensional Gaussian is evaluated.  If
*        FALSE, gamma is a free parameter of the fit, and the derived
*        value is returned in Parameter GAMMA.  [FALSE]
*     IN = NDF (Read)
*        The NDF containing the star images to be fitted.
*     INCAT = FILENAME (Read)
*        A catalogue containing a positions list (such as produced by
*        applications CURSOR, LISTMAKE, etc.) giving the star positions
*        to use.  If a null (!) value is supplied Parameter COFILE will
*        be used to get the star positions from a simple text file.
*     ISIZE = _INTEGER (Read)
*        The side of the square area to be used when forming the
*        marginal profiles for a star image, given as a number of
*        pixels.  It should be sufficiently large to contain the entire
*        star image.  It should be an odd number and must lie in the
*        range from 3 to 101.  [15]
*     LOGFILE = FILENAME (Read)
*        Text file to contain the table of parameters for each star.  A
*        null (!) name indicates that no log file is required.  [!]
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave for axis annotation, given
*        as fractions of the corresponding dimension of the current
*        picture.  Four values may be given, in the order: bottom,
*        right, top, left.  If fewer than four values are given, extra
*        values are used equal to the first supplied value.  If these
*        margins are too narrow, any axis annotation may be clipped.  If
*        a null (!) value is supplied, the value used is 0.15 (for all
*        edges) if either annotated axes or a key are produced, and zero
*        otherwise.  [current value]
*     MARKER = INTEGER (Read)
*        The PGPLOT marker type to use for the data values in the plot.
*        [current value]
*     MINOR = _LOGICAL (Read)
*        If MINOR is TRUE the horizontal axis of the plot is annotated
*        with distance along the minor axis from the centre of the PSF.
*        If MINOR is FALSE, the distance along the major axis is used.
*        [TRUE]
*     NORM = _LOGICAL (Read)
*        If TRUE, the model PSF is normalized so that it has a peak
*        value of unity.  Otherwise, its peak value is equal to the peak
*        value of the fit to the first usable star, in the data units of
*        the input NDF.  [TRUE]
*     ORIENT = _REAL (Write)
*        The orientation of the major axis of the star images, in
*        degrees.  If the current Frame of the NDF is a SKY Frame, this
*        will be a position angle (measured from north through east).
*        Otherwise, it will be measured from the positive direction of
*        the first current Frame axis ("X") towards the second current
*        Frame axis ("Y").
*     OUT = NDF (Write)
*        The NDF containing the fitted point-spread function evaluated
*        at each pixel.  If null, !, is entered no output NDF will be
*        created.  The dimensions of the array are controlled by
*        Parameter CUT.  The pixel origin is chosen to align the model
*        PSF with the fitted star in pixel co-ordinates, thus allowing
*        the NDF holding the model PSF to be compared directly with the
*        input NDF.  A WCS component is stored in the output NDF holding
*        a copy of the input WCS component.  An additional Frame with
*        Domain name OFFSET is added, and is made the current Frame.
*        This Frame measures the distance from the PSF centre in the
*        units in which the FWHM is reported.  [!]
*     POSCOLS = _INTEGER (Read)
*        Column positions of the co-ordinates (x then y) in an input
*        record of the file specified by Parameter COFILE.  The columns
*        must be different amongst themselves.  If there is duplication
*        new values will be requested.  Only accessed if INCAT is given
*        a null (!) value.  If a null (!) value is supplied for POSCOLS,
*        the values [1,2] will be used.  [!]
*     PROFOUT = NDF (Write)
*        The NDF containing the one-dimensional fitted profile as
*        displayed in the plot.  If null, !, is entered no output NDF
*        will be created.  The DATA component of this NDF holds the
*        fitted PSF value at each radial bin.  The VARIANCE component
*        holds the square of the residuals between the fitted values and
*        the binned values derived from the input NDF.  An AXIS
*        component is included in the NDF containing the radial distance
*        as displayed in the plot.  [!]
*     RANGE = _REAL (Read)
*        The number of image profile widths out to which the radial
*        star profile is to be fitted.  (There is an upper limit of 100
*        pixels to the radius at which data are actually used.)  [4.0]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use when drawing the annotated axes, data values, and the model
*        profile.
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
*        where <name> is the name of a plotting attribute, and <value>
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.  To apply changes of style to only the current
*        invocation, begin these attributes with a plus sign.  A mixture
*        of persistent and temporary style changes is achieved by
*        listing all the persistent attributes followed by a plus sign
*        then the list of temporary attributes.
*
*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported).
*
*        The appearance of the model curve is controlled by the
*        attributes Colour(Curves), Width(Curves), etc. (the synonym
*        Line may be used in place of Curves).  The appearance of the
*        markers representing the real data is controlled by
*        Colour(Markers), Width(Markers), etc. (the synonym Symbols may
*        be used in place of Markers).  [current value]
*     TITLE = LITERAL (Read)
*        The title for the NDF to contain the fitted point-spread
*        function.  If null (!) is entered the NDF will not contain a
*        title.  ["KAPPA - PSF"]
*     TOTAL = _REAL (Write)
*        The flux of the fitted function integrated to infinite radius.
*        Its unit is the product of the data unit of the input NDF and
*        the square of the radial unit, such as pixel or arcsec, for
*        the current WCS Frame, when NORM=FALSE.  When NORM=TRUE, TOTAL
*        is just measured in the squared radial unit.  Therefore, for
*        direct comparison of total flux, the same units must be used.
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the current co-ordinate Frame of
*        the NDF has more than two axes.  A group of two strings should
*        be supplied specifying the two axes which are to be used when
*        determining distances, reporting positions, etc.  Each axis can
*        be specified using one of the following options.
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
*        same indices as the two significant NDF pixel axes are used.
*        [!]
*     XCEN  =  LITERAL (Write)
*         The formatted X co-ordinate of the first fitted star position,
*         in the current co-ordinate Frame of the NDF.
*     XLEFT = _REAL (Read)
*        The axis value to place at the left hand end of the horizontal
*        axis of the plot.  If a null (!) value is supplied, a suitable
*        default value will be found and used.  The value supplied may
*        be greater than or less than the value supplied for XRIGHT.
*        [!]
*     XRIGHT = _REAL (Read)
*        The axis value to place at the right hand end of the horizontal
*        axis of the plot.  If a null (!) value is supplied, a suitable
*        default value will be found and used.  The value supplied may
*        be greater than or less than the value supplied for XLEFT.
*        [!]
*     YBOT = _REAL (Read)
*        The axis value to place at the bottom end of the vertical axis
*        of the plot.  If a null (!) value is supplied, a suitable
*        default value will be found and used.  The value supplied may
*        be greater than or less than the value supplied for YTOP.  [!]
*     YCEN  =  LITERAL (Write)
*         The formatted Y co-ordinate of the first fitted star position,
*         in the current co-ordinate Frame of the NDF.
*     YTOP = _REAL (Read)
*        The axis value to place at the top end of the vertical axis of
*        the plot.  If a null (!) value is supplied, a suitable default
*        value will be found and used.  The value supplied may be
*        greater than or less than the value supplied for YBOT.  [!]

*  Examples:
*     psf ngc6405i starlist.FIT \
*        Derives the mean point-spread function for the stars images
*        in the NDF called ngc6405i that are situated near the
*        co-ordinates given in the positions list starlist.FIT. A
*        plot of the profile is drawn on the current graphics device.
*     psf ngc6405i starlist device=!
*        As above but there is no graphical output, and the file type of
*        the input positions list is defaulted.
*     psf ngc6405i cofile=starlist.dat gauss \
*        As the first example, except the psf is fitted to a
*        two-dimensional Gaussian, and the positions are given in a
*        simple text file instead of a positions list.
*     psf incat=starlist.FIT in=ngc6405i logfile=fit.log fwhm=(seeing) \
*        As the first example, but the results, including the fits to
*        each star, are written to the text file fit.log.  The
*        full-width half-maximum is written to the ICL variable SEEING
*        rather than the parameter file.
*     psf ngc6405i starlist isize=31 style="'title=Point spread function'"
*        As the first example, but the area including a star image is
*        31 pixels square, say because the seeing is poor or the pixels
*        are smaller than normal.  The graph is titled "Point spread
*        function".

*  Notes:
*     -  Values for the FWHM seeing are given in arcseconds if the
*     Current co-ordinate Frame of the NDF is a SKY Frame.
*     -  The stars used to determine the mean image parameters should
*     be chosen to represent those whose magnitudes are to be found
*     using a stellar photometry application, and to be sufficiently
*     bright, uncrowded, and noise-free to allow an accurate fit to be
*     made.
*     -  It is assumed that the image scale does not vary significantly
*     across the image.
*     -  The iterative method to calculate the fit is as follows.
*        -  Marginal profiles of each star image are formed in four
*        directions: at 0, 45, 90 and 135 degrees to the x axis.  The
*        profiles are cleaned via an iterative modal filter that
*        removes contamination such as neighbouring stars; moving from
*        the centre of the star, the filter prevents each data point
*        from exceeding the maximum of the two previous data values.
*        -  A Gaussian curve and background is fitted to each profile
*        iteratively refining the parameters until parameters differ
*        by less than 0.1 per cent from the previous iteration.  If
*        convergence is not met after fifteen iterations, each fit
*        parameter is approximately the average of its last pair of
*        values.  The initial background is the lower quartile.
*        Using the resulting four Gaussian centres, a mean centre is
*        found for each star.  Iterations cease when the mean centroid
*        position shifts by less 0.001 from the previous iteration, or
*        after three iterations if the nominal tolerance is not
*        achieved.
*        -  The four Gaussian widths of all the stars are combined
*        modally, using an amplitude-weighted average with rejection of
*        erroneous data (using a maximum-likelihood function for a
*        statistical model in which any of the centres has a constant
*        probability of being corrupt).  From the average widths along
*        the four profiles, the seeing-disc size, axis ratio and axis
*        inclination are calculated.
*        -  The data surrounding each star is then binned into isophotal
*        zones which are elliptical annuli centred on the star---the
*        ellipse parameters being those just calculated.  The data in
*        each zone is processed to remove erroneous points (using the
*        aforementioned maximum-likelihood function) and to find an
*        average value.  A Gaussian profile is fitted to these average
*        values and the derived amplitude is used to normalise the
*        values to an amplitude of unity.  The normalised values are put
*        into bins together with the corresponding data from all other
*        stars and these binned data represent a weighted average radial
*        profile for the set of stars, with the image ellipticity
*        removed.  Finally a radial profile is fitted to these data,
*        giving the radial profile parameter gamma and a final
*        re-estimate of the seeing-disc size.

*  Related Applications:
*     PHOTOM; Starman.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, WCS and TITLE components of an NDF data structure.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  The output
*     point-spread-function NDF has the same type as the input NDF.

*  Copyright:
*     Copyright (C) 1990-1993 Science & Engineering Research Council.
*     Copyright (C) 1998-2001, 2004, 2006 Particle Physics & Astronomy
*     Research Council.
*     Copyright (C) 2007, 2010 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1990 Sep 25 (MJC):
*        Original version with commentary from Rodney Warren-Smith's
*        EDRS documentation.
*     1991 July 6 (MJC):
*        Added Usage, Notes on graphics pictures stored in the database,
*        and more on the Implementation Status in the documentation.
*     1991 July 9 (MJC):
*        An output NDF containing the PSF may now be created.  There are
*        three new parameters: CUT, OUT and TITLE.
*     1991 July 12 (MJC):
*        Support for data co-ordinates added via new Parameter COSYS.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 21 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     1993 August 27 (MJC):
*        Added ABSLAB, ORDLAB, PLTITL, RADUNITS, SCALE, and MINOR
*        parameters for adjustment of the plotting style, for scaling
*        from pixels to physical units, and for minor-axis profiles.
*     1998 May 26 (MJC):
*        Added GAUSS parameter.  A failure to meet the tolerance test
*        is no longer fatal.  Warning messages showing the requested
*        and used tolerances replace the error message.
*     16-JUL-1999 (TDCA):
*        Converted to use AST/PGPLOT for graphics. Blank lines in
*        co-ordinate file are now ignored, rather than causing a
*        fatal error.
*     20-SEP-1999 (DSB):
*        Modified to expect input positions in the current WCS Frame of
*        the NDF. Removed Parameters COSYS, RADUNITS, SCALE. Added
*        INCAT and USEAXIS.
*     26-OCT-1999 (DSB):
*        Made MARGIN a fraction of the current picture, not the DATA
*        picture.
*     2-MAY-2000 (DSB):
*        Added Parameters AMP1 and NORM.
*     17-MAY-2000 (DSB):
*        Added data units to Y axis label of the plot if NORM is FALSE.
*     22-MAY-2000 (DSB):
*        Use an inverted copy of MAP1 when creating MAP3 instead of just
*        temporarily inverting MAP1 itself. This is because the call to
*        AST_SIMPLIFY could otherwise sometimes return a pointer to MAP1
*        (eg if MAP2 was a UnitMap), and so subsequently re-inverting
*        MAP1 would also invert MAP3.
*     10-JUL-2001 (DSB):
*        Added Parameter PROFOUT.
*     13-AUG-2001 (DSB):
*        Corrected size of work arrays IPW3 and IPW4 so that no segvio
*        occurs if the OUT image is not square.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 December 30 (MJC):
*        Expanded the description of the fitting algorithm.
*     15-FEB-2007 (TIMJ):
*        Modify arguments to KPS1_SPAR<X>.  Document XCEN, YCEN and CENTRE
*        parameters.
*     2007 August 8: (MJC)
*        Added TOTAL output parameter.
*     4-MAR-2010 (DSB):
*        Changed behaviour of NORM parameter. Previously, setting NORM=NO
*        resulted in the fit being normalised to the peak value in the
*        intermediate *Gaussian* fit to the first star. Now, the fit
*        is normalised to the peak value in the final, potentially
*        *non-Gaussian*, fit to the first star (i.e. the fit that may
*        have a gamma value different to 2.0).
*     2010 October 14 (MJC):
*        Document temporary style attributes.
*     {enter_further_changes_here}
*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE constants
      INCLUDE 'NDF_PAR'        ! NDF constants
      INCLUDE 'PRM_PAR'        ! VAL__ constants
      INCLUDE 'PAR_ERR'        ! PAR error constants
      INCLUDE 'AST_PAR'        ! AST constants and function declarations
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Status:
      INTEGER STATUS           ! Global status

*  External References:
      DOUBLE PRECISION KPS1_GAMLN ! ln Gamma function

*  Local Constants:
      INTEGER NDIM             ! 2-d data arrays only
      PARAMETER ( NDIM = 2 )

      INTEGER NCHLIN           ! Maximum number of characters in an
      PARAMETER ( NCHLIN = 132 ) ! input record

      REAL PI
      PARAMETER ( PI = 3.141592654 )

*  Local Variables:
      CHARACTER BUFFER*( NCHLIN )! Buffer to store output string
      CHARACTER DTYPE*( NDF__SZFTP ) ! HDS type of the data values
      CHARACTER ITYPE*( NDF__SZTYP ) ! Implemention HDS type
      CHARACTER NAME*255        ! Name of input positions list
      CHARACTER NDFNAM*100      ! Name of input NDF
      CHARACTER TITLE*80        ! Title of input positions list
      CHARACTER UNITS*100       ! Data units for the input NDF
      INTEGER CFRM              ! Pointer to the Current Frame of the NDF
      INTEGER DIMS( NDIM )      ! Dimensions of the NDF
      INTEGER EL                ! Number of elements in the input array
                                ! and output array
      INTEGER FDL               ! File description for log file
      INTEGER I                 ! Loop count
      INTEGER INDF1             ! Identifier for input NDF
      INTEGER INDF2             ! NDF identifier for output PSF
      INTEGER INDF3             ! NDF identifier for input NDF section
      INTEGER IPDIN             ! Pointer to input data array
      INTEGER IPID              ! Pointer to array of position identifiers
      INTEGER IPIN              ! Pointer to array of supplied positions
      INTEGER IPIX              ! Index of PIXEL Frame in IWCS
      INTEGER IPPSF             ! Pointer to output PSF data array
      INTEGER IPW1              ! Pointer work array for pixel positions
      INTEGER IPW2              ! Pointer to work array for width data
      INTEGER IPW3              ! Pointer to work array for WCS creation
      INTEGER IPW4              ! Pointer to work array for WCS creation
      INTEGER ISIZE             ! Pixel size of square about a star used
                                ! to form marginal profiles
      INTEGER IWCS              ! WCS FrameSet from input NDF
      INTEGER IWCSG             ! FrameSet read from input catalogue
      INTEGER LBND( NDIM )      ! Lower bounds of the output image
      INTEGER LBNDS( NDF__MXDIM )! Lower bounds of the input NDF
      INTEGER MAP1              ! Mapping from PIXEL Frame to Current Frame
      INTEGER MAP1B             ! Inverted copy of MAP1
      INTEGER MAP2              ! Mapping from supplied Frame to Current Frame
      INTEGER MAP3              ! Mapping from supplied Frame to PIXEL Frame
      INTEGER MAP4              ! Mapping from GRID Frame to Current Frame
      INTEGER NAXC              ! No. of axes in current NDF Frame
      INTEGER NAXIN             ! No. of axes in supplied Frame
      INTEGER NC                ! Character column counter in output buffer
      INTEGER NCF               ! Character column counter of filenames
      INTEGER NDIMS             ! No. of dimensions in input NDF
      INTEGER NPOS              ! Number of non-comment lines in the x-y file
      INTEGER NW                ! Size of work array
      INTEGER PSFDIM( NDIM )    ! PSF dimensions
      INTEGER PSFSIZ            ! Dimension of region used to calculate mean PSF
      INTEGER SDIM( NDF__MXDIM )! Significant dimensions of the NDF
      INTEGER SLBND( NDIM )     ! Significant lower bounds of the image
      INTEGER SUBND( NDIM )     ! Significant upper bounds of the image
      INTEGER UBND( NDIM )      ! Upper bounds of the output image
      INTEGER UBNDS( NDF__MXDIM )! Upper bounds of the input NDF
      LOGICAL GAUSS             ! Fit to a Gaussian?
      LOGICAL GOTID             ! Does IPID point to an array of identifiers?
      LOGICAL LOGPOS            ! Log file is open for co-ordinates?
      LOGICAL NM                ! Normalize PSF to amplitude of unity?
      REAL AMP                  ! Peak amplitude of the fitted PSF
      REAL AXISR                ! Axis ratio o fthe star images
      REAL CUT                  ! Threshold to which the output PSF must
                                ! extend
      REAL FWHM                 ! FWHM of the star images
      REAL GAMFUN               ! Gamma-function at 2/GAMMA
      REAL GAMMA                ! Radial fall-off parameter
      REAL PX                   ! X pixel co-ord at first fitted star
      REAL PY                   ! Y pixel co-ord at first fitted star
      REAL RANGE                ! Number of image profile widths to
                                ! which radial profile is to be fitted
      REAL THETA                ! Orientation of the star images
      REAL TOTAL                ! Total flux

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Attempt to open a log file to store the results for human readers.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 80, FDL, STATUS )

*  Annul the error if a null value was given, and indicate that a log
*  file is not to be created.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         LOGPOS = .FALSE.

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGPOS = .TRUE.

      END IF

*  Remind the user about the log file, if required.
      IF ( LOGPOS ) CALL MSG_OUT( 'LOG', '  Logging to $LOGFILE',
     :                           STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the identifier of the NDF to be displayed.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Now get the WCS FrameSet from the NDF.
      CALL KPG1_ASGET( INDF1, NDIM, .TRUE., .TRUE., .TRUE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Store the length of each significant dimension.
      DIMS( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
      DIMS( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1

*  Get a pointer to the Current Frame in the NDF.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Save the number of Current Frame axes.
      NAXC = AST_GETI( CFRM, 'NAXES', STATUS )

*  Get the Mapping from GRID to Current Frame in the NDF.
      MAP4 = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE,
     :                                     AST__CURRENT, STATUS ),
     :                     STATUS )

*  Get the Mapping from PIXEL to Current Frame in the NDF. First find
*  the index of the PIXEL Frame, and then get the Mapping.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )
      MAP1 = AST_SIMPLIFY( AST_GETMAPPING( IWCS, IPIX, AST__CURRENT,
     :                                     STATUS ), STATUS )

*  This application can only process non-complex types.  Therefore for
*  the given type of the image find in which type it should be
*  processed.
      CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'/
     :                /'_DOUBLE', INDF1, INDF1, 'Data', ITYPE, DTYPE,
     :                STATUS )

*  Map the image.
      CALL NDF_MAP( INDF1, 'Data', ITYPE, 'READ', IPDIN, EL, STATUS )

*  Store the name of the NDF in the log file.
      IF ( LOGPOS ) THEN

         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_LOAD( ' ', '^NDF', NDFNAM, NCF, STATUS )

         NC = 3
         BUFFER = ' '
         CALL CHR_APPND( 'Input NDF is', BUFFER, NC )
         NC = NC + 1
         CALL CHR_APPND( NDFNAM, BUFFER, NC )
         CALL CHR_APPND( '.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )

      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to open a positions list catalogue and read its contents.  A
*  pointer to a FrameSet is returned, together with pointers to
*  positions and identifiers, and a title.  The positions are returned
*  in the Base Frame of this FrameSet.
      IWCSG = AST__NULL
      CALL KPG1_RDLST( 'INCAT', .FALSE., IWCSG, NPOS, NAXIN, IPIN,
     :                 IPID, TITLE, NAME, STATUS )

*  If successful, get the AST Mapping from the Frame in which the
*  positions are supplied to the Current Frame of the NDF.
      IF ( STATUS .EQ. SAI__OK ) THEN
         GOTID = .TRUE.

*  The positions are supplied in the Base Frame of the FrameSet stored
*  in  the catalogue.  Merge this FrameSet with the FrameSet read from
*  the NDF aligning them in some suitable Frame.
         CALL KPG1_ASMRG( IWCSG, IWCS, ' ', .FALSE., 0, STATUS )

*  Get the Mapping.
         MAP2 = AST_SIMPLIFY( AST_GETMAPPING( IWCSG, AST__BASE,
     :                                        AST__CURRENT, STATUS ),
     :                        STATUS )

*  If a null value was supplied for INCAT, annul the error and try to
*  get a list of star positions using Parameter COFILE.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTID = .FALSE.

*  Obtain the file and read the positions, interpreting them as
*  positions within the Current Frame of the NDF.  A pointer to memory
*  holding the positions is returned.  Store a safe value for the IPID
*  pointer.  Identifiers are generated automatically instead of being
*  read from the file, and so we do not have a pointer to an array of
*  identifiers at this point.
         CALL KPG1_ASFIL( 'COFILE', 'POSCOLS', CFRM, NPOS, IPIN,
     :                    NAME, STATUS )
         IPID = IPIN

*  The positions are supplied in the Current Frame, so use a unit
*  mapping for MAP2.
         MAP2 = AST_UNITMAP( NAXC, ' ', STATUS )

      END IF

*  Store the name of the co-ordinate file or catalogue in the log file.
      IF ( LOGPOS .AND. NAME .NE. ' ' ) THEN
         NC = 3
         BUFFER = ' '
         CALL CHR_APPND( 'Input co-ordinate list is', BUFFER, NC )
         NC = NC + 1
         CALL CHR_APPND( NAME, BUFFER, NC )
         CALL CHR_APPND( '.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )
      END IF

*  Save the number of axes in the Frame in which the positions are
*  supplied.
      NAXIN = AST_GETI( MAP2, 'NIN', STATUS )

*  We need the Mapping from the Frame in which the positions are
*  supplied, to the PIXEL Frame of the NDF.  We get this Mapping by
*  concatenating the Mapping from input Frame to Current Frame, with
*  the Mapping from Current Frame to PIXEL Frame (obtained by
*  inverting the Mapping from PIXEL to Current Frame).
      MAP1B = AST_COPY( MAP1, STATUS )
      CALL AST_INVERT( MAP1B, STATUS )
      MAP3 = AST_SIMPLIFY( AST_CMPMAP( MAP2, MAP1B, .TRUE., ' ',
     :                                 STATUS ), STATUS )

*  Check the Mapping has the required transformations.
      IF ( .NOT. AST_GETL( MAP3, 'TRANFORWARD', STATUS ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PSF_ERR1','The Mapping required '//
     :                 'to map the supplied positions into the '//
     :                 'pixel Frame of the NDF is not defined.',
     :                 STATUS )
         GO TO 999
      END IF

*  Allocate work space to hold the PIXEL co-ordinates of the star
*  positions.
      CALL PSX_CALLOC( NPOS * NDIM, '_DOUBLE', IPW1, STATUS )

*  Transform the supplied positions to the PIXEL Frame of the NDF. Store
*  them in the above work space.
      CALL AST_TRANN( MAP3, NPOS, NAXIN, NPOS, %VAL( CNF_PVAL( IPIN ) ),
     :                .TRUE.,
     :                NDIM, NPOS, %VAL( CNF_PVAL( IPW1 ) ), STATUS )

*  Obtain the search area size and, the range of radii to be used in
*  the profile fit, in units of sigma.  Note the upper limit of the
*  area size should be equal to twice MAXRAD in KPS1_RPRFx.
      CALL PAR_GODD( 'ISIZE', 15, 3, 101, .TRUE., ISIZE, STATUS )
      CALL PAR_GDR0R( 'RANGE', 4.0, 1.0, 10.0, .FALSE., RANGE, STATUS )

*  Determine whether or not gamma is a free parameter.
      CALL PAR_GET0L( 'GAUSS', GAUSS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Determine whether or not the PSF is to be normalized to an amplitude
*  of unity.
      CALL PAR_GET0L( 'NORM', NM, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Annul the error if a null value was supplied.
      IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Find the mean star profile.
*  ===========================
*  Allocate work space
      CALL PSX_CALLOC( NPOS * 5, '_REAL', IPW2, STATUS )

*  Get the input NDF data units.
      UNITS = ' '
      CALL NDF_CGET( INDF1, 'UNITS', UNITS, STATUS )

*  Find the mean star profile parameters calling the routine of the
*  appropriate data type.  Plot the results as required.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPS1_SPARR( CFRM, MAP1, DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( IPDIN ) ),
     :                    SLBND, ISIZE, RANGE, GAUSS,
     :                    NPOS, %VAL( CNF_PVAL( IPW1 ) ), LOGPOS,
     :                    FDL, 'MINOR', 'AXISR', 'ORIENT', 'FWHM',
     :                    'GAMMA', 'AMP1', 'CENTRE', 'XCEN', 'YCEN',
     :                    %VAL( CNF_PVAL( IPID ) ),
     :                    GOTID, NM, UNITS, AXISR, THETA, FWHM,
     :                    GAMMA, PSFSIZ, %VAL( CNF_PVAL( IPW2 ) ),
     :                    PX, PY, AMP, STATUS )

      ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPS1_SPARB( CFRM, MAP1, DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( IPDIN ) ),
     :                    SLBND, ISIZE, RANGE, GAUSS,
     :                    NPOS, %VAL( CNF_PVAL( IPW1 ) ), LOGPOS,
     :                    FDL, 'MINOR', 'AXISR', 'ORIENT', 'FWHM',
     :                    'GAMMA', 'AMP1', 'CENTRE', 'XCEN', 'YCEN',
     :                    %VAL( CNF_PVAL( IPID ) ),
     :                    GOTID, NM, UNITS, AXISR, THETA, FWHM,
     :                    GAMMA, PSFSIZ, %VAL( CNF_PVAL( IPW2 ) ),
     :                    PX, PY, AMP, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_SPARD( CFRM, MAP1, DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( IPDIN ) ),
     :                    SLBND, ISIZE, RANGE, GAUSS,
     :                    NPOS, %VAL( CNF_PVAL( IPW1 ) ), LOGPOS,
     :                    FDL, 'MINOR', 'AXISR', 'ORIENT', 'FWHM',
     :                    'GAMMA', 'AMP1', 'CENTRE', 'XCEN', 'YCEN',
     :                    %VAL( CNF_PVAL( IPID ) ),
     :                    GOTID, NM, UNITS, AXISR, THETA, FWHM,
     :                    GAMMA, PSFSIZ, %VAL( CNF_PVAL( IPW2 ) ),
     :                    PX, PY, AMP, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPS1_SPARI( CFRM, MAP1, DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( IPDIN ) ),
     :                    SLBND, ISIZE, RANGE, GAUSS,
     :                    NPOS, %VAL( CNF_PVAL( IPW1 ) ), LOGPOS,
     :                    FDL, 'MINOR', 'AXISR', 'ORIENT', 'FWHM',
     :                    'GAMMA', 'AMP1', 'CENTRE', 'XCEN', 'YCEN',
     :                    %VAL( CNF_PVAL( IPID ) ),
     :                    GOTID, NM, UNITS, AXISR, THETA, FWHM,
     :                    GAMMA, PSFSIZ, %VAL( CNF_PVAL( IPW2 ) ),
     :                    PX, PY, AMP, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPS1_SPARUW( CFRM, MAP1, DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( IPDIN ) ),
     :                    SLBND, ISIZE, RANGE, GAUSS,
     :                    NPOS, %VAL( CNF_PVAL( IPW1 ) ), LOGPOS,
     :                    FDL, 'MINOR', 'AXISR', 'ORIENT', 'FWHM',
     :                    'GAMMA', 'AMP1', 'CENTRE', 'XCEN', 'YCEN',
     :                    %VAL( CNF_PVAL( IPID ) ),
     :                    GOTID, NM, UNITS, AXISR, THETA, FWHM,
     :                    GAMMA, PSFSIZ, %VAL( CNF_PVAL( IPW2 ) ),
     :                    PX, PY, AMP, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPS1_SPARUB( CFRM, MAP1, DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( IPDIN ) ),
     :                    SLBND, ISIZE, RANGE, GAUSS,
     :                    NPOS, %VAL( CNF_PVAL( IPW1 ) ), LOGPOS,
     :                    FDL, 'MINOR', 'AXISR', 'ORIENT', 'FWHM',
     :                    'GAMMA', 'AMP1', 'CENTRE', 'XCEN', 'YCEN',
     :                    %VAL( CNF_PVAL( IPID ) ),
     :                    GOTID, NM, UNITS, AXISR, THETA, FWHM,
     :                    GAMMA, PSFSIZ, %VAL( CNF_PVAL( IPW2 ) ),
     :                    PX, PY, AMP, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPS1_SPARW( CFRM, MAP1, DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( IPDIN ) ),
     :                    SLBND, ISIZE, RANGE, GAUSS,
     :                    NPOS, %VAL( CNF_PVAL( IPW1 ) ), LOGPOS,
     :                    FDL, 'MINOR', 'AXISR', 'ORIENT', 'FWHM',
     :                    'GAMMA', 'AMP1', 'CENTRE', 'XCEN', 'YCEN',
     :                    %VAL( CNF_PVAL( IPID ) ),
     :                    GOTID, NM, UNITS, AXISR, THETA, FWHM,
     :                    GAMMA, PSFSIZ, %VAL( CNF_PVAL( IPW2 ) ),
     :                    PX, PY, AMP, STATUS )

      END IF

*  Store the total flux
*  ====================

*  Evaluate the total flux.  Use the better logarithmic gamma function
*  (not to be confused with the exponent gamma).
      IF ( STATUS .EQ. SAI__OK ) THEN
         GAMFUN = EXP( SNGL( KPS1_GAMLN( 2.0D0 / DBLE( GAMMA ) ) ) )

*  Use the widely documented formula.  The scalelength squared is
*  the geometric mean of the FWHM values to allow for the elliptical
*  shape.  Also correct to standard deviation, noting that relationship
*  between the dispersion and FWHM is a function of the shape exponent.
         TOTAL = PI * AMP * FWHM * FWHM * AXISR * GAMFUN /
     :            ( GAMMA * 2.0 * ( LOG( 2.0 ) )**( 2.0 / GAMMA ) )

      END IF

      CALL PAR_PUT0R( 'TOTAL', TOTAL, STATUS )

*  Find the dimensions of the NDF to contain the PSF.
*  ==================================================

*  Obtain the level as a fraction of the central amplitude to which the
*  output PSF should extend.  This controls the dimensions of of the
*  PSF array.
      CALL ERR_MARK
      CALL PAR_GDR0R( 'CUT', 0.0001, VAL__SMLR, 1.0 - VAL__SMLR,
     :                .FALSE., CUT, STATUS )

*  A null value means just use the same dimensions as was used to
*  calculate the mean profile.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         PSFDIM( 1 ) = PSFSIZ
         PSFDIM( 2 ) = PSFSIZ
      ELSE
         CALL KPS1_PSDIM( AXISR, THETA, FWHM, GAMMA, CUT, PSFDIM,
     :                    STATUS )
      END IF
      CALL ERR_RLSE

*  Create output NDF containing the PSF.
*  =====================================
      CALL ERR_MARK

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Decide on the bounds of the output NDF so that the centre of the psf
*  is at the position of the first fitted star.
      UBND( 1 ) = INT( PX ) + 1 + PSFDIM( 1 ) / 2
      LBND( 1 ) = INT( PX ) + 1 - PSFDIM( 1 ) / 2
      UBND( 2 ) = INT( PY ) + 1 + PSFDIM( 2 ) / 2
      LBND( 2 ) = INT( PY ) + 1 - PSFDIM( 2 ) / 2

*  Find the complete bounds of this section of the input NDF,
*  remembering to leave bounds of any insignificant axes unchanged.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBNDS, UBNDS, NDIMS, STATUS )
      DO I = 1, NDIM
         UBNDS( SDIM( I ) ) = UBND( I )
         LBNDS( SDIM( I ) ) = LBND( I )
      END DO

*  Now get an NDF identifier for this section of the input NDF.
      CALL NDF_SECT( INDF1, NDIMS, LBNDS, UBNDS, INDF3, STATUS )

*  Create a new NDF, by propagating the shape, size, WCS, etc from this
*  section.
      CALL LPG_PROP( INDF3, 'NOLABEL,WCS,AXIS', 'OUT', INDF2,
     :               STATUS )

*  Map it for write access.
      CALL NDF_MAP( INDF2, 'Data', '_REAL', 'WRITE', IPPSF, EL, STATUS )

*  Get workspace.
      NW = MAX( UBND( 1 ) - LBND( 1 ) + 3, UBND( 2 ) - LBND( 2 ) + 3 )
      CALL PSX_CALLOC( NW, '_DOUBLE', IPW3, STATUS )
      CALL PSX_CALLOC( NW, '_DOUBLE', IPW4, STATUS )

*  Fill the data array with the evaluated point-spread function.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL KPS1_PSEVL( AMP, AXISR, THETA, FWHM, GAMMA, LBND( 1 ),
     :                    UBND( 1 ), LBND( 2 ), UBND( 2 ), PX, PY,
     :                    %VAL( CNF_PVAL( IPPSF ) ), STATUS )

*  Add an OFFSET Frame to the WCS information in the output NDF.
         CALL KPS1_PSWCS( INDF2, LBND, UBND, %VAL( CNF_PVAL( IPW3 ) ),
     :                    %VAL( CNF_PVAL( IPW4 ) ),
     :                    STATUS )

*  Store a title.
         CALL NDF_CINP( 'TITLE', INDF2, 'TITLE', STATUS )

*  Store a label.
         CALL NDF_CPUT( 'Point Spread Function', INDF2, 'Lab', STATUS )

      END IF

*  Free work space.
      CALL PSX_FREE( IPW3, STATUS )
      CALL PSX_FREE( IPW4, STATUS )

*  A null status can be ignored.  This means that no output NDF was
*  required.
      IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

      CALL ERR_RLSE

*  Tidying sequence.
*  =================
  999 CONTINUE

*  Close the log file.
      IF ( LOGPOS ) CALL FIO_ANNUL( FDL, STATUS )

*  Tidy the workspace, etc.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )
      CALL PSX_FREE( IPIN, STATUS )
      IF ( GOTID ) CALL PSX_FREE( IPID, STATUS )

*  Close down the NDF system, unmapping the NDF.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PSF_ERR', 'PSF: Error finding a model star '//
     :                 'profile for a two-dimensional NDF.', STATUS )
      END IF

      END
