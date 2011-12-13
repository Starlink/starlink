      SUBROUTINE DRAWNDF( STATUS )
*+
*  Name:
*     DRAWNDF

*  Purpose:
*     Draws aligned images or outlines on a graphics display.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DRAWNDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine displays on a graphics device the positions of a
*     group of NDFs in their Current attached coordinate system.  This
*     will show their relative positions in their current coordinates,
*     and so can, for instance, be used to check that alignment looks
*     correct prior to resampling and combining into a mosaic.
*     Depending on the CLEAR parameter it will either clear the
*     display device and set the plotting area to the right size to
*     fit in all the images, or leave the display intact and plot those
*     parts of images which fit on the existing area.
*
*     Depending on the LINES and IMAGE parameters, an outline showing
*     the extent of each NDF can be plotted, or the pixels of the NDF
*     plotted resampled into the given coordinate system, or both.
*     Each outline or pixel block shows the extent of the data array of
*     the corresponding NDF, and is therefore basically rectangular
*     in shape, though it may be distorted if the mapping
*     between pixel and Current coordinates is nonlinear.  The origin
*     (minimum X,Y pixel value) of each boundary can be marked and
*     the image labelled with its name and/or index number.
*     Optionally (according to the TRIM parameter), the display may
*     be restricted to the useful extent of the image, enabling
*     overscan regions or bias strips to be ignored.
*
*     If the LINES parameter is true, the position of each NDF's
*     data array will be indicated by a (perhaps distorted) rectangle
*     drawn on the device.  If the IMAGE parameter is true, then
*     the image's pixels will be plotted as well as its position.
*     The colour levels in this case are determined by the PERCENTILES
*     argument applied separately to each plotted frame, and overlapping
*     images will simply be drawn on top of each other - no averaging
*     or scaling is performed.  If the IMAGES parameter is false,
*     the program does not need to examine the data pixels at all,
*     so it can run much faster.
*
*     The results are only likely to be sensible if the Current
*     coordinate system of all the NDFs is one in which they are all
*     (more or less) aligned.  If the Current attached coordinate
*     systems of all do not all have the same Domain (name), a
*     warning will be issued, but plotting will proceed.
*
*     DRAWNDF uses the AGI graphics database in a way which is
*     compatible with KAPPA applications; if the CLEAR parameter is
*     set to false (only possible when IMAGE is also false) then it
*     will attempt to align the plotted outlines with suitably
*     registered graphics which are already on the graphics
*     device; in this case outlines or parts of outlines lying
*     outside the existing graphics window remain unplotted.
*     So, for instance, it is easy to overlay the outlines
*     of a set of NDFs on a mosaic image which has been constructed
*     using those NDFs, or to see how an undisplayed set of NDFs
*     would map onto one already displayed, either by a previous
*     invocation of DRAWNDF or by a KAPPA program such as DISPLAY
*     or CONTOUR.
*
*     This routine is designed for use on two-dimensional NDFs;
*     if the NDFs presented have more than two dimensions, any higher
*     ones will be ignored.

*  Usage:
*     drawndf in [device]

*  ADAM Parameters:
*     AXES = _LOGICAL (Read)
*        True if labelled and annotated axes are to be drawn around the
*        plotting surface, showing the common Current coordinate system
*        of the images.  The appearance of the axes can be controlled
*        using the STYLE parameter.  AXES has a dynamic default; it
*        defaults to the same value as the CLEAR parameter.
*        [dynamic]
*     CLEAR = _LOGICAL (Read)
*        If CLEAR is set to true, the graphics device will be cleared
*        before the plot is made.
*
*        If you want the outlines to be drawn over the top
*        of an existing DATA picture, for instance one displayed with
*        KAPPA's DISPLAY application, then set CLEAR to false.  If
*        possible, alignment will occur within the Current coordinate
*        system of the NDF.  If this is not possible, an attempt is
*        made in SKY, PIXEL or GRID domains.  If the image cannot be
*        aligned in any suitable domain, then DRAWNDF will terminate
*        with an error.  If CLEAR is set to FALSE, then there must
*        already be a picture displayed on the graphics device.
*
*        The CLEAR parameter is ignored (and the device cleared anyway)
*        if IMAGE is true.
*        [TRUE]
*     DEVICE = DEVICE (Read)
*        The name of the device on which to make the plot.
*        [Current display device]
*     EXTENT( 4 ) = _INTEGER (Read)
*        The extent of the useful CCD area.  This should be given in
*        pixel index values (see notes). The extent is restricted to
*        that of the CCD frame, so no padding of the data can occur. If
*        values outside of those permissable are given then they are
*        modified to lie within the CCD frame. The values should be
*        given in the order XMIN,XMAX,YMIN,YMAX.
*
*        If the TRIM parameter is set true, then only the area defined
*        by these values is drawn.  If TRIM is false, this parameter
*        is ignored.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.  If USESET is true
*        then a value specific to the Set Index of each image will
*        be sought.
*     IN = LITERAL (Read)
*        A list of the NDFs to be displayed.
*     IMAGE = _LOGICAL (Read)
*        If true, the pixels of the each image will be plotted.
*        In this case any existing plot on the graphics device is
*        always cleared, regardless of the value of the CLEAR parameter.
*        Note that DRAWNDF does not need to examine the NDF pixels
*        at all unless this option is true, so setting it can make
*        the program run much more slowly.
*        [FALSE]
*     LABNAME = _LOGICAL (Read)
*        If true, each plotted outline is labelled with the name of
*        the NDF.  Label positioning is determined by the LABPOS
*        parameter.
*        [TRUE]
*     LABNUM = _LOGICAL (Read)
*        If true, each plotted outline is labelled with the number of
*        the NDF (i.e. the first on in the IN list is 1, the second
*        is 2, etc).  If both this and the LABNAME parameter are true,
*        the label will contain both the number and the name.
*        Label positioning is determined by the LABPOS parameter.
*        [FALSE]
*     LABOPAQUE = _LOGICAL (Read)
*        If true, the label text indicated by the LABNUM and LABNAME
*        parameters will be written on an opaque rectangle of background
*        colour obscuring the picture below.  If false, the text will
*        be plotted directly on the picture, which may be hard to read.
*        [TRUE]
*     LABPOS = LITERAL (Read)
*        A two-character string identifying the positioning of the text
*        label (only used if at least one of LABNAME or LABNUM is
*        true).  The first letter indicates the side-to-side
*        position and the second indicates the up-and-down position
*        in the pixel coordinates of each NDF.  Each letter must be
*        "N", "C" or "F", for Near to the origin, Central or Far from
*        the origin.  Normally (unless LABUP is true) the text
*        will be written parallel or antiparallel to the X pixel
*        direction for each NDF, with one edge anchored as per the
*        value of LABPOS in such a way that the text sits inside the
*        outline (if it will fit).
*
*        Only the first two characters are significant.
*
*        LABPOS normally defaults to "NN", indicating the label written
*        next to the origin, but if LABUP is set TRUE, then it
*        defaults to "CC".
*        [NN]
*     LABUP = _LOGICAL (Read)
*        Normally this parameter is FALSE, and each text label (as
*        determined by LABNAME and LABNUM) is written parallel or
*        anti-parallel to the pixel X axis of the corresponding NDF.
*        If this parameter is set TRUE however, text will be written
*        upright, that is, horizontal on the graphics device.
*        In this case the positioning algorithm may fail to place it
*        inside the corresponding outline; it is generally not advisable
*        to set LABUP to TRUE unless the label is positioned in the
*        centre of the outline by setting LABPOS="CC".
*        [FALSE]
*     LINES = _LOGICAL (Read)
*        If true, the outline of each NDF is plotted.  If false, no
*        outlines are plotted.
*        [TRUE]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     ORIGIN = _LOGICAL (Read)
*        If true, a marker is placed at the grid coordinate origin
*        of each NDF (the corner of the data region being considered
*        which has the lowest X and Y pixel coordinates).
*        [TRUE]
*     PENROT = _LOGICAL (Read)
*        If TRUE, each outline will be drawn with a different pen
*        (colour).  Otherwise, they will all be drawn in the same pen.
*        [FALSE]
*     PERCENTILES( 2 ) = _DOUBLE (Read)
*        If IMAGE is true, this gives the percentile limits between
*        which each image will be scaled when it is drawn.
*        Any pixels with a value lower than the first element
*        will have the same colour, and any with a value
*        higher than the second will have the same colour.
*        Must be in the range 0 <= PERCENTILES( 1 ) <=
*        PERCENTILES( 2 ) <= 100.
*
*        Note that the percentile levels are calculated separately for
*        each of the NDFs in the IN list, so that the brightest
*        pixel in each NDF will be plotted in the same colour, even
*        though their absolute values may be quite different.
*        [2,98]
*     STYLE = LITERAL (Read)
*        A group of attribute settings describing the plotting style
*        to use for the outlines and annotated axes.  This should be
*        a string consisting of comma-separated `attribute=value'
*        items; as explained in the `Plotting Styles and Attributes'
*        section of SUN/95, except that colours may only be specified
*        by number, and not by name.
*
*        Some attributes which it may be useful to set are the following
*        (default values given in square brackets):
*           - width(curves)   -- the thickness of outlines drawn [1]
*           - colour(curves)  -- colour of the outlines (if PENROT is
*                                true, serves as starting value) [1]
*           - size(strings)   -- font size of text labels [1]
*           - colour(strings) -- colour of text labels [1]
*           - colour(markers) -- colour of origin markers [1]
*           - colour          -- colour of everything plotted
*                                (including axes and axis labels) [1]
*           - grid            -- whether to draw a grid (1=yes, 0=no) [0]
*           - title           -- title to draw above the plot [coords title]
*
*        [""]
*     TRIM = _LOGICAL (Read)
*        If TRIM is true, then an attempt will be made to trim the data
*        to its useful area only; this may be used to exclude non-image
*        areas such as overscan regions.  See the EXTENT parameter for
*        details of how the useful area is determined.
*        [FALSE]
*     USEEXT = _LOGICAL (Read)
*        If USEEXT and TRIM are both TRUE, then the value of the EXTENT
*        parameter will be sought from the CCDPACK extension of each
*        NDF.  This method will only be successful if they have been
*        put there using the IMPORT or PRESENT programs.
*        [TRUE]
*     USESET = _LOGICAL (Read)
*        If the pen colour is being rotated because PENROT is true,
*        USESET determines whether a new colour is used for each
*        individual NDF or each Set.  If TRIM is true, it allows
*        Set-Index-specific values of the EXTENT parameter to be
*        used.  This parameter is ignored if PENROT and TRIM are
*        false, and has no effect if the input NDFs have no Set header
*        information.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [FALSE]

*  Examples:
*     drawndf reg-data* clear
*        This will clear the current graphics device and plot on it
*        labelled outlines of all the `reg-data*' NDFs, as well as
*        axes showing the common coordinate system in which they
*        all reside.  The plotting area will be made just large enough
*        that all the outlines fit in.  Prior to running this, the
*        Current attached coordinate system of all the reg-data* NDFs
*        should be one in which they are all aligned.
*
*     drawndf ccd* noclear
*        This will attempt to plot boundaries of all the `ccd*' NDFs
*        aligned with whatever is already plotted on the graphics
*        device, for instance the result of a KAPPA DISPLAY command
*        or of a previous call of DRAWNDF.  Parts of the NDF outlines
*        which fall outside the existing plot area will not be visible.
*        If this is attempted when there is no existing picture on
*        the graphics device it will fail with an error.
*
*     drawndf in="one,two,three" axes labname labnum penrot
*             style="size(strings)=2,width(curves)=3"
*        This will draw outlines of the NDFs `one', `two' and `three'
*        in the current directory with labelled axes, in triple-thick
*        lines and with double-size text labels which read `1: one',
*        `2: two' and `3: three' respectively.  The colour of each
*        outline and its associated text label will be different from
*        the others.
*
*     drawndf in=a* noclear nopenrot style="colour=2" nolabel nolabnum
*        All the NDFs beginning with `a' will be outlined in colour 2,
*        with no text labels or indication of the origin.
*
*     drawndf in=gc2112 nolines image percentiles=[20,90]
*        The graphics device will be cleared, and the named image
*        resampled into its Current attached coordinate system will
*        be displayed.  The data will be scaled such that the brightest
*        10% of pixels are plotted in the highest available colour and
*        the dimmest 20% in the lowest.
*
*     drawndf "obs-[abc]" image lines labup labopaque=false
*        The files obs-a, obs-b and obs-c will be plotted; both the
*        outlines and the pixel data will be shown, and the name of
*        each will be drawn upright in the middle of each one,
*        without an opaque background.

*  Implementation Status:
*     DRAWNDF's communication with the AGI database is compatible with
*     most of KAPPA's behaviour, but is slightly less capable; in
*     particular it will fail to align with pictures whose alignment
*     has been stored using TRANSFORM structures instead of MORE.AST
*     extensions.  This affects only older applications.

*  Notes:
*     -  Resampling schemes:
*        When the IMAGE parameter is true and image pixels are plotted,
*        the image data has to be resampled into the Current coordinate
*        system prior to being displayed on the graphics device.
*        DRAWNDF currently does this using a nearest-neighbour
*        resampling scheme if the display pixels are of comparable
*        size or larger than the image pixels, and a block averaging
*        scheme if they are much smaller (less than one third the size).
*        Though slower, this latter scheme has the advantage of
*        averaging out noisy data.
*
*     -  Pixel indices:
*        The EXTENT values supplied should be given as pixel index values.
*        These usually start at (1,1) for the pixel at the lower left
*        hand corner of the data-array component (this may
*        not be true if the NDFs have been sectioned, in which case the
*        lower left hand pixel will have pixel indices equal to the data
*        component origin values). Pixel indices are different from
*        pixel coordinates in that they are non-continuous, i.e. can
*        only have integer values, and start at 1,1 not 0,0. To change
*        from pixel coordinates add 0.5 and round to the nearest integer.
*
*     -  Display:
*        The IMAGE display mode is not particularly sophisticated.
*        If you wish to view a single image in its pixel coordinate
*        system, you may find KAPPA's DISPLAY program more versatile.

*  Behaviour of Parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application. The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE, USESET and EXTENT) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line, or in the case
*     of EXTENT, if USEEXT is true.  If USESET is true, a global value
*     for EXTENT corresponding to the Set Index of each image will be
*     sought.  Global values may be set and reset using the CCDSETUP
*     and CCDCLEAR commands.
*
*     The DEVICE parameter also has a global association. This is not
*     controlled by the usual CCDPACK mechanisms, instead it works in
*     co-operation with KAPPA (SUN/95) image display/control routines.
*
*     If the parameter USEEXT is true then the EXTENT parameter will
*     be sought first from the input NDF extensions, and only got
*     from its global or command-line value if it is not present there.

*  Copyright:
*     Copyright (C) 2000-2001 Central Laboratory of the Research
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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-DEC-2000 (MBT):
*        Original version.
*     29-JAN-2001 (MBT):
*        Modified so that it writes the whole frameset from each NDF into
*        the AGI database, not just the GRID frame.
*     15-FEB-2001 (MBT):
*        Upgraded for use with Sets.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'PAR_PAR'          ! Standard PAR constants
      INCLUDE 'PAR_ERR'          ! PAR system error codes
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Local Constants:
      INTEGER MAXLAB             ! Maximum number of labelling options
      PARAMETER ( MAXLAB = 8 )

*  External references:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Used length of string

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BADCOL             ! Image background colour
      INTEGER BGCOL              ! Normal text background colour
      INTEGER BL                 ! Buffer length
      INTEGER DIMS( 2, CCD1__MXNDF ) ! Dimensions of NDFs
      INTEGER EL                 ! Number of pixels in NDF
      INTEGER FRM                ! AST identifier for a frame
      INTEGER FSET               ! AST identifier for global frameset
      INTEGER GID                ! NDG group identifier for input NDFs
      INTEGER GOTSEC( CCD1__MXNDF ) ! Set Indices for which we have param values
      INTEGER HICOL              ! Highest colour index available
      INTEGER I                  ! Loop variable
      INTEGER ID                 ! NDF identifier
      INTEGER IGOT               ! Index of name found in a group
      INTEGER INDFS( CCD1__MXNDF ) ! NDF identifiers
      INTEGER INIPEN             ! Initial pen index
      INTEGER IPDAT              ! Pointer to mapped data of NDF
      INTEGER IPEN               ! Current pen index
      INTEGER IPIM               ! Pointer to image pixel array
      INTEGER ISET( CCD1__MXNDF ) ! Index of Set for each NDF
      INTEGER IWCS               ! AST identifier for WCS frameset
      INTEGER J                  ! Loop variable
      INTEGER JCOM               ! Index of common frame in global FSET
      INTEGER JBAS               ! Index of Base frame in frameset
      INTEGER JCUR               ! Index of Current frame in frameset
      INTEGER JGRID( CCD1__MXNDF ) ! Relative frame indices of Grid-like frames
      INTEGER JPGP               ! Frame index for PGPIXL plotting frame
      INTEGER JSET               ! Set alignment frame index (dummy)
      INTEGER KEYGID             ! GRP identifier for Set Name attributes
      INTEGER LBGCOL             ! Label text background colour
      INTEGER LBND( 2 )          ! Lower bounds of NDF
      INTEGER LBNDS( 2 )         ! Lower bounds of NDF section
      INTEGER LOCOL              ! Lowest colour index available
      INTEGER MAP                ! AST identifier for a mapping
      INTEGER MAPICK             ! AST identifier for picked frames mapping
      INTEGER MAPU               ! AST identifier for unit mapping
      INTEGER NAXES              ! Number of axes (dimensions) in a frame
      INTEGER NDIM               ! Number of returned dimensions
      INTEGER NFRM               ! Number of frames in frameset
      INTEGER NGOT               ! Number of Set Index values seen
      INTEGER NNDF               ! Number of input NDFs in group
      INTEGER NPEN               ! Number of distinct pens for rotation
      INTEGER NSET               ! Number of distinct Sets in input NDFs
      INTEGER PAXES( 2 )         ! Map for picking two axes from many
      INTEGER PENGID             ! GRP identifier for pen style strings
      INTEGER PICID              ! AGI identifier for initial DATA picture
      INTEGER PICOD              ! AGI identifier for new picture
      INTEGER PLOT               ! AST identifier of plot
      INTEGER SCHEME             ! Resampling scheme for AST_RESAMPLE.
      INTEGER SINDEX             ! Set Index attribute value (dummy)
      INTEGER STATE              ! Parameter state
      INTEGER STYLEN             ! Length of style element
      INTEGER UBND( 2 )          ! Upper bounds of NDF
      INTEGER UBNDS( 2 )         ! Upper bounds of NDF section
      INTEGER XIM                ! X dimension of image pixel array
      INTEGER YIM                ! Y dimension of image pixel array
      LOGICAL AXES               ! Draw axes?
      LOGICAL BAD                ! Might there be bad pixels in the NDF?
      LOGICAL CLEAR              ! Clear the graphics device before plotting?
      LOGICAL DIFERS             ! Not all the same domain?
      LOGICAL EXTSEC             ! Dummy
      LOGICAL IMAGE              ! Draw pixels too?
      LOGICAL LABDOT             ! Use labelling ORIGIN option?
      LOGICAL LABIND             ! Use labelling INDEX option?
      LOGICAL LABNAM             ! Use labelling NAME option?
      LOGICAL LABOPQ             ! Use labelling OPAQUE option?
      LOGICAL LABUP              ! Write labels upright on graphics device?
      LOGICAL LINES              ! Draw an outline?
      LOGICAL PENROT             ! Rotate pen styles for different outlines?
      LOGICAL NOINV              ! Has coordinate system been reflected?
      LOGICAL SECTS              ! Have we actually trimmed to EXTENT?
      LOGICAL TRIM               ! Trim data to EXTENT region?
      LOGICAL USEEXT             ! Use EXTENT values from CCDPACK extension?
      LOGICAL USESET             ! Rotate pen styles by Set?
      REAL X1                    ! Lower X limit of plotting surface
      REAL X2                    ! Upper X limit of plotting surface
      REAL Y1                    ! Lower Y limit of plotting surface
      REAL Y2                    ! Upper Y limit of plotting surface
      REAL XCH                   ! Vertical baseline text character height
      REAL YCH                   ! Horizontal baseline text character height
      REAL UP( 2 )               ! Up direction for text
      DOUBLE PRECISION DIMOD     ! Length of the unit diagonal vector
      DOUBLE PRECISION GLBND( 2 ) ! Lower GRID-like coordinate bounds
      DOUBLE PRECISION GUBND( 2 ) ! Upper GRID-like coordinate bounds
      DOUBLE PRECISION INA( 2 )  ! Coordinates of input A point for winmap
      DOUBLE PRECISION INB( 2 )  ! Coordinates of input B point for winmap
      DOUBLE PRECISION INTPAR( 1 ) ! Parameters for AST_RESAMPLE
      DOUBLE PRECISION IXUN( 4 ) ! Input X corners of the unit square
      DOUBLE PRECISION IYUN( 4 ) ! Input Y corners of the unit square
      DOUBLE PRECISION LIMITS( 2 ) ! Values at percentile limits
      DOUBLE PRECISION LPOS( 2 ) ! Dummy low position
      DOUBLE PRECISION OFS       ! Offset length for text label
      DOUBLE PRECISION OLBND( 2, CCD1__MXNDF ) ! Upper common coordinate bounds
      DOUBLE PRECISION OUBND( 2, CCD1__MXNDF ) ! Upper common coordinate bounds
      DOUBLE PRECISION OUTA( 2 ) ! Coordinates of output A point for winmap
      DOUBLE PRECISION OUTB( 2 ) ! Coordinates of output B point for winmap
      DOUBLE PRECISION OXUN( 4 ) ! Output X corners of the unit square
      DOUBLE PRECISION OYUN( 4 ) ! Output Y corners of the unit square
      DOUBLE PRECISION PERCNT( 2 ) ! Percentile values for image scaling
      DOUBLE PRECISION PSIZE     ! Image magnification factor
      DOUBLE PRECISION PX( 2 )   ! X input coords of test transformation point
      DOUBLE PRECISION PY( 2 )   ! Y input coords of test transformation point
      DOUBLE PRECISION QX( 2 )   ! X output coords of test transformation point
      DOUBLE PRECISION QY( 2 )   ! Y output coords of test transformation point
      DOUBLE PRECISION TPOS( 2 ) ! Text reference position
      DOUBLE PRECISION UPOS( 2 ) ! Dummy high position
      DOUBLE PRECISION VERTEX( 5, 2 ) ! GRID-like coordinates of array corners
      DOUBLE PRECISION WORK( 2 ) ! Workspace
      DOUBLE PRECISION XADD      ! Padding in X direction
      DOUBLE PRECISION XC        ! X coordinate of NDF grid centre
      DOUBLE PRECISION XF        ! X coordinate of NDF grid Far edge
      DOUBLE PRECISION XHI       ! Upper X coordinate of outline
      DOUBLE PRECISION XLO       ! Lower X coordinate of outline
      DOUBLE PRECISION XMAX      ! Upper X coordinate of bounding box
      DOUBLE PRECISION XMIN      ! Lower X coordinate of bounding box
      DOUBLE PRECISION XN        ! X coordinate of NDF grid Near edge
      DOUBLE PRECISION XPT       ! X coordinate of text anchor position
      DOUBLE PRECISION YADD      ! Padding in Y direction
      DOUBLE PRECISION YC        ! Y coordinate of NDF grid centre
      DOUBLE PRECISION YF        ! Y coordinate of NDF grid Far edge
      DOUBLE PRECISION YHI       ! Upper Y coordinate of outline
      DOUBLE PRECISION YLO       ! Lower Y coordinate of outline
      DOUBLE PRECISION YMAX      ! Upper Y coordinate of bounding box
      DOUBLE PRECISION YMIN      ! Lower Y coordinate of bounding box
      DOUBLE PRECISION YN        ! Y coordinate of NDF grid Near edge
      DOUBLE PRECISION YPT       ! Y coordinate of text anchor position
      CHARACTER * ( 2 ) JUST     ! Justification for text placement
      CHARACTER * ( 2 ) LABPOS   ! Label positioning
      CHARACTER * ( 16 ) LFMT    ! Format string for text labels
      CHARACTER * ( 80 ) BUFFER  ! Line buffer for output
      CHARACTER * ( AST__SZCHR ) DMN ! Current domain for this NDF
      CHARACTER * ( AST__SZCHR ) COMDMN ! Domain of common frame
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of the NDF
      CHARACTER * ( GRP__SZNAM ) SNAME ! Set Name attribute value
      CHARACTER * ( GRP__SZNAM ) STYEL ! Style element
      CHARACTER * ( NDF__SZTYP ) TYPE ! Type of NDF data

*  Local data:
      DATA PAXES / 1, 2 /
      DATA BGCOL / 0 /
      DATA PX / 0D0, 1D0 /
      DATA PY / 0D0, 1D0 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up CCDPACK.
      CALL CCD1_START( 'DRAWNDF', STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Initialise GRP identifiers, so that a later call of CCD1_GRDEL on
*  an uninitialised group cannot cause trouble.
      GID = GRP__NOID
      PENGID = GRP__NOID
      KEYGID = GRP__NOID

*  Get the list of NDFs for display.
      NNDF = 0
      CALL CCD1_NDFGL( 'IN', 1, CCD1__MXNDF, GID, NNDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  See if we are only considering EXTENT-trimmed NDF sections.
      CALL PAR_GET0L( 'TRIM', TRIM, STATUS )
      IF ( TRIM ) THEN
         CALL PAR_GET0L( 'USEEXT', USEEXT, STATUS )
      ELSE
         USEEXT = .FALSE.
      END IF

*  See if we are plotting images, and if so get percentile scaling limits.
      CALL PAR_GET0L( 'IMAGE', IMAGE, STATUS )
      IF ( IMAGE ) THEN
         CALL PAR_EXACD( 'PERCENTILES', 2, PERCNT, STATUS )
         PERCNT( 1 ) = MAX( 0D0, PERCNT( 1 ) )
         PERCNT( 2 ) = MAX( PERCNT( 1 ), MIN( 1D2, PERCNT( 2 ) ) )
         PERCNT( 1 ) = 1D-2 * PERCNT( 1 )
         PERCNT( 2 ) = 1D-2 * PERCNT( 2 )
      END IF

*  Determine what labelling and drawing options are required.
      CALL PAR_GET0L( 'LABNAME', LABNAM, STATUS )
      CALL PAR_GET0L( 'LABNUM', LABIND, STATUS )
      CALL PAR_GET0L( 'ORIGIN', LABDOT, STATUS )
      CALL PAR_GET0L( 'LINES', LINES, STATUS )

*  Construct a labelling format string accordingly.
      IF ( LABNAM .AND. LABIND ) THEN
         LFMT = '^INDEX: ^NDF'
      ELSE IF ( LABNAM ) THEN
         LFMT = '^NDF'
      ELSE IF ( LABIND ) THEN
         LFMT = '^INDEX'
      ELSE
         LFMT = ' '
      END IF

*  Get label positioning and orientation options if required.
      IF ( LFMT .NE. ' ' ) THEN

*  Get label background option.
         CALL PAR_GET0L( 'LABOPAQUE', LABOPQ, STATUS )

*  Get label orientation option.
         CALL PAR_GET0L( 'LABUP', LABUP, STATUS )

*  Get the label positioning option.  If LABUP is TRUE,
*  then just use positioning of 'CC' unless the parameter has been
*  explicitly supplied on the command line.
         IF ( LABUP ) THEN
            CALL PAR_STATE( 'LABPOS', STATE, STATUS )
            IF ( STATE .EQ. PAR__ACTIVE ) THEN
               CALL PAR_GET0C( 'LABPOS', LABPOS, STATUS )
            ELSE
               LABPOS = 'CC'
            END IF
         ELSE
            CALL PAR_GET0C( 'LABPOS', LABPOS, STATUS )
         END IF

*  Validate the label positioning option.
         CALL CHR_UCASE( LABPOS )
         IF ( STATUS .EQ. SAI__OK .AND. (
     :        LABPOS( 1:1 ) .NE. 'N' .AND. LABPOS( 1:1 ) .NE. 'C' .AND.
     :        LABPOS( 1:1 ) .NE. 'F' .OR.
     :        LABPOS( 2:2 ) .NE. 'N' .AND. LABPOS( 2:2 ) .NE. 'C' .AND.
     :        LABPOS( 2:2 ) .NE. 'F' ) ) THEN
            STATUS = SAI__ERROR
            CALL CCD1_ERREP( 'DRAWNDF_BADJUST', 'DRAWNDF: LABPOS ' //
     :                       'parameter not of form [NCF][NCF]',
     :                       STATUS )
            GO TO 99
         END IF
      END IF

*  Set text background.
      IF ( LABOPQ ) THEN
         LBGCOL = 0
      ELSE
         LBGCOL = -1
      END IF

*  Determine whether we will be rotating pens for different outlines.
      CALL PAR_GET0L( 'PENROT', PENROT, STATUS )

*  Determine whether we will be rotating pens by Set or by image.
      IF ( PENROT .OR. TRIM ) CALL PAR_GET0L( 'USESET', USESET, STATUS )

*  Determine whether we should clear the display device before plotting.
      IF ( IMAGE ) THEN
         CLEAR = .TRUE.
      ELSE
         CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )
      END IF

*  Determine whether we will draw axes.
      CALL PAR_GTD0L( 'AXES', CLEAR, .TRUE., AXES, STATUS )

*  Get NDF identifiers for all the input NDFs.  If we are restricting
*  display to the defined EXTENT of each NDF, then make sure that
*  the identifiers only refer to an appropriate NDF section.
      SECTS = .FALSE.
      NGOT = 0
      DO I = 1, NNDF

*  Get the NDF identifier.
         CALL NDG_NDFAS( GID, I, 'READ', ID, STATUS )
         IF ( TRIM ) THEN

*  Get the bounds of the NDF.
            CALL NDF_BOUND( ID, 2, LBND, UBND, NDIM, STATUS )

*  If using Sets, we may have to set up the parameter system to retrieve
*  the value of the EXTENT parameter keyed to the Set Index value of
*  this NDF (using CCD1_KPLD).  Ensure however that we only do this
*  the first time we try to get the value for each Set Index.
            IF ( USESET ) THEN
               CALL CCD1_SETRD( ID, AST__NULL, SNAME, SINDEX, JSET,
     :                          STATUS )
               DO J = 1, NGOT
                  IF ( GOTSEC( J ) .EQ. SINDEX ) GO TO 1
               END DO
               CALL CCD1_KPLD( 'EXTENT', SINDEX, STATUS )
               NGOT = NGOT + 1
               IF ( NGOT .GT. CCD1__MXNDF ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'DRAWNDF_MAXMEMB',
     :'DRAWNDF: Too many distinct Set Index values', STATUS )
                  GO TO 99
               END IF
               GOTSEC( NGOT ) = SINDEX
 1             CONTINUE
            END IF

*  Get the value of the EXTENT.
            CALL CCD1_GTSEC( USEEXT, ID, LBND, UBND, LBNDS, UBNDS,
     :                       EXTSEC, STATUS )

*  If this represents a non-complete subset of the NDF, then get an
*  identifier for the indicated NDF Section, and discard the identifier
*  for the complete NDF.
            IF ( LBND( 1 ) .NE. LBNDS( 1 ) .OR.
     :           LBND( 2 ) .NE. LBNDS( 2 ) .OR.
     :           UBND( 1 ) .NE. UBNDS( 1 ) .OR.
     :           UBND( 2 ) .NE. UBNDS( 2 ) ) THEN
               SECTS = .TRUE.
               CALL NDF_SECT( ID, 2, LBNDS, UBNDS, INDFS( I ), STATUS )
               CALL NDF_ANNUL( ID, STATUS )

*  Otherwise we can just use the NDF identifier itself.
            ELSE
               INDFS( I ) = ID
            END IF
         ELSE
            INDFS( I ) = ID
         END IF
      END DO

*  Warn the user if we are only using sections (in at least some cases).
      IF ( SECTS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  NDFs were trimmed to EXTENT region.',
     :                  STATUS )
      END IF

*  Initialise the global frameset with the Current frame of the
*  reference (first) NDF.  First get its WCS component.
      CALL CCD1_GTWCS( INDFS( 1 ), IWCS, STATUS )

*  Get the Current frame of the reference NDF, which we would like to
*  use to represent the common coordinate system.  Save its domain too.
      FRM = AST_COPY( AST_GETFRAME( IWCS, AST__CURRENT, STATUS ),
     :                STATUS )
      COMDMN = AST_GETC( FRM, 'Domain', STATUS )

*  Initialise the global frameset.
      FSET = AST_FRAMESET( FRM, ' ', STATUS )

*  Initialise differing-domains flag.
      DIFERS = .FALSE.

*  Identify the initial (and so far only) frame in the global frameset
*  as the common frame.
      JCOM = AST_GETI( FSET, 'Base', STATUS )

*  Create a UnitMap suitable for gluing similar frames to this one.
      MAPU = AST_UNITMAP( AST_GETI( FSET, 'Naxes', STATUS ), ' ',
     :                    STATUS )

*  Prepare for summary output to the user.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      BUFFER = ' '
      BUFFER( 4: ) = 'Index'
      BUFFER( 12: ) = 'NDF'
      BUFFER( 40: ) = 'Domain'
      CALL CCD1_MSG( ' ', BUFFER( 1:CHR_LEN( BUFFER ) ), STATUS )
      BUFFER( 4: ) = '-----'
      BUFFER( 12: ) = '---'
      BUFFER( 40: ) = '------'
      CALL CCD1_MSG( ' ', BUFFER( 1:CHR_LEN( BUFFER ) ), STATUS )

*  Initialise a group for Set Name attributes if necessary.
      IF ( USESET ) THEN
         CALL GRP_NEW( 'CCD:SETNAME', KEYGID, STATUS )
      ELSE
         KEYGID = GRP__NOID
      END IF

*  Store the information we need from each of the NDFs.
      DO I = 1, NNDF

*  Get the NDF name.
         CALL GRP_GET( GID, I, 1, NDFNAM, STATUS )

*  Get the WCS component from the NDF.
         CALL CCD1_GTWCS( INDFS( I ), IWCS, STATUS )

*  Check that the Current frame has the right number of dimensions.
         NAXES = AST_GETI( IWCS, 'Naxes', STATUS )
         IF ( NAXES .NE. 2 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL MSG_SETI( 'NAXES', NAXES )
            CALL CCD1_ERREP( ' ', 'DRAWNDF: NDF ''^NDF'' has ^NAXES '
     :                    // 'axes in Current coordinates.', STATUS )
            CALL CCD1_ERREP( ' ', '         It must have 2.', STATUS )
            GO TO 99
         END IF

*  Get the Base (GRID-domain) frame of the NDF.
         FRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )

*  Check it has the right number of dimensions.  If it has too few,
*  then bail out.  If it has too many, then generate a new frame and
*  mapping which just represents the first two.
         NAXES = AST_GETI( FRM, 'Naxes', STATUS )
         IF ( NAXES .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL CCD1_ERREP( ' ', 'DRAWNDF: NDF ''^NDF'' must have at '
     :                    // 'least 2 dimensions.', STATUS )
            GO TO 99
         ELSE IF ( NAXES .GT. 2 ) THEN
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL MSG_SETI( 'IG', NAXES - 2 )
            CALL CCD1_MSG( ' ', '  Warning: Ignoring ^IG axes in NDF '
     :                  // '^NDF', STATUS )
            FRM = AST_PICKAXES( FRM, 2, PAXES, MAPICK, STATUS )
            JCUR = AST_GETI( IWCS, 'Current', STATUS )
            CALL AST_ADDFRAME( IWCS, AST__BASE, MAPICK, FRM, STATUS )
            CALL AST_SETI( IWCS, 'Base', AST_GETI( IWCS, 'Current',
     :                                             STATUS ), STATUS )
            CALL AST_SETI( IWCS, 'Current', JCUR, STATUS )
         END IF

*  Get the NDF extent in its Base (GRID-domain) frame.
         CALL NDF_DIM( INDFS( I ), 2, DIMS( 1, I ), NDIM, STATUS )

*  Output the name of the NDF and its Current frame domain to the user.
         BUFFER = ' '
         CALL MSG_FMTI( 'INDEX', 'I3', I )
         CALL MSG_LOAD( ' ', '^INDEX)', BUFFER( 4: ), BL, STATUS )
         CALL MSG_SETC( 'NDF', NDFNAM )
         CALL MSG_LOAD( ' ', '^NDF', BUFFER( 12: ), BL, STATUS )
         IF ( CHR_LEN( BUFFER ) .GT. 38 ) BUFFER( 36: ) = '...'
         DMN = AST_GETC( IWCS, 'Domain', STATUS )
         DIFERS = DIFERS .OR. ( DMN .NE. COMDMN )
         BUFFER( 40: ) = DMN
         CALL CCD1_MSG( ' ', BUFFER( 1:CHR_LEN( BUFFER ) ), STATUS )

*  Work out where the GRID-domain frame will be in relation to the
*  common frame when it is added to the global frameset.
         JBAS = AST_GETI( IWCS, 'Base', STATUS )
         NFRM = AST_GETI( FSET, 'Nframe', STATUS )
         JGRID( I ) = NFRM + JBAS - JCOM

*  Add the new frameset into the global one.  It will have a unit
*  mapping from its Current frame to the common frame.
         CALL AST_ADDFRAME( FSET, JCOM, MAPU, IWCS, STATUS )

*  If necessary, find Set information from its Set header.
         IF ( USESET ) THEN

*  Read the Set header.
            CALL CCD1_SETRD( INDFS( I ), AST__NULL, SNAME, SINDEX, JSET,
     :                       STATUS )

*  See if we have encountered this Set Name before.  If not, add it to
*  the group.
            CALL GRP_INDEX( SNAME, KEYGID, 1, IGOT, STATUS )
            IF ( IGOT .EQ. 0 .OR. SNAME .EQ. ' ' ) THEN
               CALL GRP_PUT( KEYGID, 1, SNAME, 0, STATUS )
               CALL GRP_GRPSZ( KEYGID, IGOT, STATUS )
            END IF

*  Record this Set Name's position in the group of encountered Names.
            ISET( I ) = IGOT
         END IF
      END DO

*  Warn the user if not all the Current frames in the NDFs had the same
*  domain.
      IF ( DIFERS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  Warning: Not all NDF Current coordinate'
     :               // ' frames have the same domain.', STATUS )
      END IF

*  Set the Current frame of the frameset to the common coordinate system.
      CALL AST_SETI( FSET, 'Current', JCOM, STATUS )

*  We now have a global frameset containing one common frame and a frame
*  representing a two-dimensional GRID-frame-like coordinate system
*  corresponding to each of the input NDFs.  We can use this to construct
*  an AST Plot object with which to address the graphics device.
      PLOT = AST__NULL

*  If we have been asked not to clear the graphics device, attempt to
*  construct a Plot object in conjunction with the current contents of
*  the AGI database.
      IF ( .NOT. CLEAR ) THEN

*  Open the graphics device and select the DATA picture.
         CALL AGP_ASSOC( 'DEVICE', 'UPDATE', 'DATA', .FALSE., PICID,
     :                   STATUS )
         CALL AGI_SELP( PICID, STATUS )

*  Attempt to get a plot aligned with the AGI current picture.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL CCD1_APLOT( FSET, PICID, .TRUE., PLOT, STATUS )
      END IF

*  If we have been asked to clear the device, we need to determine the
*  boundaries of the area in which to plot.
      IF ( PLOT .EQ. AST__NULL ) THEN
         XMIN = VAL__MAXD
         YMIN = VAL__MAXD
         XMAX = VAL__MIND
         YMAX = VAL__MIND
         GLBND( 1 ) = 0.5D0
         GLBND( 2 ) = 0.5D0

*  Loop for each NDF.
         DO I = 1, NNDF

*  Find a bounding box which will contain the data array of this NDF in
*  the common frame.
            MAP = AST_GETMAPPING( FSET, JCOM + JGRID( I ), JCOM,
     :                            STATUS )
            GUBND( 1 ) = DBLE( DIMS( 1, I ) ) + 0.5D0
            GUBND( 2 ) = DBLE( DIMS( 2, I ) ) + 0.5D0
            DO J = 1, 2
               CALL AST_MAPBOX( MAP, GLBND, GUBND, .TRUE., J,
     :                          OLBND( J, I ), OUBND( J, I ), LPOS,
     :                          UPOS, STATUS )
            END DO

*  Update limits if necessary.
            IF ( OLBND( 1, I ) .LT. XMIN ) XMIN = OLBND( 1, I )
            IF ( OLBND( 2, I ) .LT. YMIN ) YMIN = OLBND( 2, I )
            IF ( OUBND( 1, I ) .GT. XMAX ) XMAX = OUBND( 1, I )
            IF ( OUBND( 2, I ) .GT. YMAX ) YMAX = OUBND( 2, I )
         END DO

*  Add a little to the limits so the outlines don't butt up to the edges.
         XADD = ( XMAX - XMIN ) * 0.03D0
         YADD = ( YMAX - YMIN ) * 0.03D0
         XMIN = XMIN - XADD
         XMAX = XMAX + XADD
         YMIN = YMIN - YADD
         YMAX = YMAX + YADD

*  Open the graphics device using AGI unless we have already done so.
*  Leave a gap round the outside only if we will be drawing axes.
         IF ( CLEAR ) THEN
            CALL AGP_ASSOC( 'DEVICE', 'WRITE', ' ', AXES, PICID,
     :                      STATUS )
         ELSE
            IF ( AXES ) CALL AGP_NVIEW( .TRUE., STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Set the PGPLOT viewport and world coordinates to give a viewport
*  large enough to hold all the outlines, of the correct aspect ratio,
*  and with enough space for labelling round the outside.
         CALL PGWNAD( REAL( XMIN ), REAL( XMAX ), REAL( YMIN ),
     :                REAL( YMAX ) )

*  Turn the global frameset into an AST Plot.
         CALL CCD1_APLOT( FSET, PICID, .FALSE., PLOT, STATUS )
      END IF

*  Set default text background colour opaque.
      CALL PGSTBG( BGCOL )

*  Apply default and user-selected style settings to the plot.
      CALL CCD1_PLSTY( PLOT, 'Tol=0.001', 'STYLE', STATUS )

*  Save the PGPLOT viewport as a new picture in the AGI database.
      CALL AGP_SVIEW( 'DATA', 'CCDPACK_DRAWNDF', PICOD, STATUS )

*  Save the Plot in the new picture.
      CALL CCD1_SPLOT( PICOD, PLOT, STATUS )

*  Set the common frame index to its correct value for the Plot object;
*  it will be the Current frame of the Plot object, since it was the
*  Current frame of the Frameset passed to CCD1_APLOT.  The relative
*  positions of the grid-like frames within the PLOT frameset will
*  be the same as those in the FSET frameset, so as long as we know
*  the position of the common one, we can easily get the positions of
*  the others.
      JCOM = AST_GETI( PLOT, 'Current', STATUS )

*  If we are going to plot pixels, do it here.  We have to assemble
*  the image to be plotted first, and then plot it, since the pixel
*  plotting routine can only plot orthogonal rectangles, and our
*  blocks may not be that shape.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( IMAGE ) THEN

*  Get an array of integers the same shape as the plotting surface so
*  that transferring it from the array to the plotting device will
*  be efficient.
         CALL PGQVP( 3, X1, X2, Y1, Y2 )
         XIM = INT( X2 - X1 + 1 )
         YIM = INT( Y2 - Y1 + 1 )
         CALL CCD1_MALL( XIM * YIM, '_INTEGER', IPIM, STATUS )

*  Prepare for filling the array.
         CALL PGQCIR( LOCOL, HICOL )
         BADCOL = 0

*  Add a frame to the Plot frameset which represents the array we have
*  just allocated (graphical pixel coordinates).
         CALL PGQVP( 2, X1, X2, Y1, Y2 )
         INA( 1 ) = DBLE( X1 )
         INA( 2 ) = DBLE( Y1 )
         INB( 1 ) = DBLE( X2 )
         INB( 2 ) = DBLE( Y2 )
         OUTA( 1 ) = 0.5D0
         OUTA( 2 ) = 0.5D0
         OUTB( 1 ) = XIM + 0.5D0
         OUTB( 2 ) = YIM + 0.5D0
         MAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )
         FRM = AST_FRAME( 2, 'Domain=CCD_PGPIXL', STATUS )
         CALL AST_ADDFRAME( PLOT, AST__BASE, MAP, FRM, STATUS )
         JPGP = AST_GETI( PLOT, 'Current', STATUS )

*  Initialise the array with empty-coloured pixels.
         CALL CCG1_STVI( BADCOL, XIM * YIM, %VAL( CNF_PVAL( IPIM ) ),
     :                   STATUS )
         DO I = 1, NNDF

*  Get a mapping from the GRID coordinates of the NDF to the coordinates
*  of the image pixels array.
            MAP = AST_GETMAPPING( PLOT, JCOM + JGRID( I ), JPGP,
     :                            STATUS )

*  Calculate the approximate size of an NDF pixel in this frame.
            CALL AST_TRAN2( MAP, 2, PX, PY, .TRUE., QX, QY, STATUS )
            PSIZE = SQRT( ( QX( 2 ) - QX( 1 ) ) ** 2
     :                 + ( QY( 2 ) - QY( 1 ) ) ** 2 ) / SQRT( 2D0 )
            IF ( PSIZE .LT. 0.333D0 ) THEN
               SCHEME = AST__BLOCKAVE
               INTPAR( 1 ) = ( 1D0 / PSIZE - 1D0 ) * 0.5D0
            ELSE
               SCHEME = AST__NEAREST
            END IF

*  Open the NDF and map its data.
            CALL NDF_TYPE( INDFS( I ), 'DATA', TYPE, STATUS )
            CALL NDF_MAP( INDFS( I ), 'DATA', TYPE, 'READ', IPDAT, EL,
     :                    STATUS )
            CALL NDF_BAD( INDFS( I ), 'DATA', .FALSE., BAD, STATUS )

*  Find the percentile values.
            CALL CCD1_FRA( TYPE, EL, IPDAT, 2, PERCNT, BAD,
     :                     WORK, LIMITS, STATUS )

*  Resample the data array onto the pixel grid.
            CALL CCD1_MKIMG( IPDAT, TYPE, DIMS( 1, I ), DIMS( 2, I ),
     :                       MAP, XIM, YIM, LIMITS( 1 ), LIMITS( 2 ),
     :                       LOCOL, HICOL, BADCOL, SCHEME, INTPAR,
     :                       %VAL( CNF_PVAL( IPIM ) ), STATUS )
         END DO

*  Plot the array which now contains all the resampled images.
         CALL PGQWIN( X1, X2, Y1, Y2 )
         CALL PGPIXL( %VAL( CNF_PVAL( IPIM ) ),
     :                XIM, YIM, 1, XIM, 1, YIM,
     :                X1, X2, Y1, Y2 )

*  Free the memory used for the plot.
         CALL CCD1_MFREE( IPIM, STATUS )
         CALL AST_REMOVEFRAME( PLOT, JPGP, STATUS )
      END IF

*  Set the Current frame of the plot to the common coordinate frame.
      CALL AST_SETI( PLOT, 'Current', JCOM, STATUS )

*  Plot the coordinate axes if required.
      IF ( AXES ) THEN
         CALL AST_GRID( PLOT, STATUS )
      END IF

*  Set initial pen number.
      INIPEN = AST_GETI( PLOT, 'Colour(curves)', STATUS )

*  If we are rotating pens between plots, generate a group of pen style
*  attributes to cycle through.  Doing it like this is slightly more
*  involved than just calculating it within the plotting loop, but
*  makes it easier to customise if more complicated changes of style
*  are required.
      PENGID = GRP__NOID
      IF ( PENROT ) THEN

*  Create a new group.
         CALL GRP_NEW( 'CCDPACK:PENS', PENGID, STATUS )

*  Use a semicolon as the separator character since the comma is used
*  within the names themselves.
         CALL GRP_SETCC( PENGID, 'DELIMITER', ';', STATUS )

*  Get the range of available colours.
         CALL PGQCOL( LOCOL, HICOL )

*  Write a group entry for each of the available colours, or one for each
*  NDF, whichever is the fewer.
         IF ( USESET ) THEN
            CALL GRP_GRPSZ( KEYGID, NSET, STATUS )
            NPEN = NSET + INIPEN
         ELSE
            NPEN = NNDF + INIPEN
         END IF
         NPEN = MIN( NPEN, HICOL - LOCOL + 1 )
         DO I = 1, NPEN
            CALL MSG_SETI( 'IPEN', I )
            CALL MSG_LOAD( ' ', 'Colour=^IPEN', STYEL, STYLEN, STATUS )
            CALL GRP_PUT( PENGID, 1, STYEL( 1 : STYLEN ), I, STATUS )
         END DO
      END IF

*  Initialise constant vertex positions.
      XLO = 0.5D0
      YLO = 0.5D0
      DO J = 1, 5
         VERTEX( J, 1 ) = XLO
         VERTEX( J, 2 ) = YLO
      END DO

*  Set the size of the marker to plot at the origin (a fraction of a
*  character height, specified in units of 1/200 inches).
      CALL PGQCS( 1, XCH, YCH )

*  Set the length of the offset vector for text labels.
      CALL PGQCS( 4, XCH, YCH )
      OFS = XCH * 0.7D0

*  Loop for each NDF.
      IF ( LINES .OR. LABDOT ) THEN
         DO I = 1, NNDF

*  Set pen colour if required.
            IF ( PENGID .NE. GRP__NOID ) THEN
               IF ( USESET ) THEN
                  IPEN = 1 + MOD( INIPEN + ISET( I ) - 1, NPEN )
               ELSE
                  IPEN = 1 + MOD( INIPEN + I - 1, NPEN )
               END IF
               CALL GRP_GET( PENGID, IPEN, 1, STYEL, STATUS )
               CALL AST_SET( PLOT, STYEL, STATUS )
            END IF

*  Set up an array giving the GRID-like coordinates of the corners of
*  the data array for this NDF.  The fifth point is a copy of the first
*  one, which makes it easier for plotting a closed loop.
            XHI = DIMS( 1, I ) + 0.5D0
            YHI = DIMS( 2, I ) + 0.5D0
            VERTEX( 2, 1 ) = XHI
            VERTEX( 3, 1 ) = XHI
            VERTEX( 3, 2 ) = YHI
            VERTEX( 4, 2 ) = YHI

*  Set the Current frame of the Plot object to the GRID-like coordinate
*  system of this NDF.
            CALL AST_SETI( PLOT, 'Current', JCOM + JGRID( I ), STATUS )

*  Plot a marker at the origin of each outline.
            IF ( LABDOT ) THEN
               CALL AST_MARK( PLOT, 1, 2, 5, VERTEX, 24, STATUS )
            END IF

*  Plot geodesics in the GRID-like coordinate system along the edges
*  of the data array.  These will appear as the correct outlines in the
*  common coordinate system.
            IF ( LINES ) THEN
               CALL AST_POLYCURVE( PLOT, 5, 2, 5, VERTEX, STATUS )
            END IF
         END DO
      END IF

*  Check if we need to write any text.  We do this after all the lines
*  have been plotted so that if text is written on an opaque background
*  it will not get overwritten by the outlines of other NDFS.
      IF ( LFMT .NE. ' ' ) THEN

*  If so, loop over all the NDFs.
         DO I = 1, NNDF

*  Set pen colour if required.
            IF ( PENGID .NE. GRP__NOID ) THEN
               IF ( USESET ) THEN
                  IPEN = 1 + MOD( INIPEN + ISET( I ) - 1, NPEN )
               ELSE
                  IPEN = 1 + MOD( INIPEN + I - 1, NPEN )
               END IF
               CALL GRP_GET( PENGID, IPEN, 1, STYEL, STATUS )
               CALL AST_SET( PLOT, STYEL, STATUS )
            END IF

*  Set the Current frame of the Plot object to the GRID-like coordinate
*  system of this NDF.
            CALL AST_SETI( PLOT, 'Current', JCOM + JGRID( I ), STATUS )

*  Set the near, centre, and far edge grid coordinates.
            XN = 0.5D0
            YN = 0.5D0
            XC = 0.5D0 + DIMS( 1, I ) * 0.5D0
            YC = 0.5D0 + DIMS( 2, I ) * 0.5D0
            XF = 0.5D0 + DIMS( 1, I )
            YF = 0.5D0 + DIMS( 2, I )

*  Get coordinates of the text labelling reference postion, and translate
*  the label positioning option into an AST_TEXT-friendly form.
            IF ( LABPOS( 1:1 ) .EQ. 'N' ) THEN
               JUST( 2:2 ) = 'L'
               XPT = XN
            ELSE IF ( LABPOS( 1:1 ) .EQ. 'C' ) THEN
               JUST( 2:2 ) = 'C'
               XPT = XC
            ELSE IF ( LABPOS( 1:1 ) .EQ. 'F' ) THEN
               JUST( 2:2 ) = 'R'
               XPT = XF
            END IF
            IF ( LABPOS( 2:2 ) .EQ. 'N' ) THEN
               JUST( 1:1 ) = 'B'
               YPT = YN
            ELSE IF ( LABPOS( 2:2 ) .EQ. 'C' ) THEN
               JUST( 1:1 ) = 'C'
               YPT = YC
            ELSE IF ( LABPOS( 2:2 ) .EQ. 'F' ) THEN
               JUST( 1:1 ) = 'T'
               YPT = YF
            END IF

*  Set up some useful positions in NDF grid coordinates:
*     1: reference position + X unit vector
*     2: reference position + Y unit vector
*     3: centre of the NDF
*     4: reference position
            IXUN( 1 ) = XPT + 1D0
            IYUN( 1 ) = YPT
            IXUN( 2 ) = XPT
            IYUN( 2 ) = YPT + 1D0
            IXUN( 3 ) = XC
            IYUN( 3 ) = YC
            IXUN( 4 ) = XPT
            IYUN( 4 ) = YPT

*  Transform the useful positions into Plot coordinates.
            CALL AST_TRAN2( PLOT, 4, IXUN, IYUN, .FALSE., OXUN, OYUN,
     :                      STATUS )

*  Convert the positions into vectors for convenience:
*     1: X unit vector
*     2: Y unit vector
*     3: Vector towards the centre of the NDF
            DO J = 1, 3
               OXUN( J ) = OXUN( J ) - OXUN( 4 )
               OYUN( J ) = OYUN( J ) - OYUN( 4 )
            END DO

*  Find out whether the coordinate system has suffered a reflection, by
*  testing the sense of the cross-product of the transformed unit X
*  and Y vectors.
            NOINV = OXUN( 1 ) * OYUN( 2 ) - OYUN( 1 ) * OXUN( 2 )
     :              .GT. 0D0

*  Get 'up' direction normal to which to write the text.  This is either
*  the graphics up directions, or the direction of the transformed X
*  unit vector rotated by plus or minus 90 degrees.
            IF ( LABUP ) THEN
               UP( 1 ) = 0.0
               UP( 2 ) = 1.0
            ELSE
               IF ( NOINV ) THEN
                  UP( 1 ) = - REAL( OYUN( 1 ) )
                  UP( 2 ) =   REAL( OXUN( 1 ) )
               ELSE
                  UP( 1 ) =   REAL( OYUN( 1 ) )
                  UP( 2 ) = - REAL( OXUN( 1 ) )
               END IF
            END IF

*  If the coordinates have been reflected then we need to switch left
*  and right for the text justification.
            IF ( .NOT. NOINV ) THEN
               IF ( JUST( 2:2 ) .EQ. 'L' ) THEN
                  JUST( 2:2 ) = 'R'
               ELSE IF ( JUST( 2:2 ) .EQ. 'R' ) THEN
                  JUST( 2:2 ) = 'L'
               END IF
            END IF

*  Get the position of the outline origin, plus a small padding offset
*  towards the centre of the NDF.
            DIMOD = SQRT( OXUN( 3 ) ** 2 + OYUN( 3 ) ** 2 )
            IF ( DIMOD .EQ. 0D0 ) DIMOD = 1D0
            TPOS( 1 ) = OXUN( 4 ) + OFS * OXUN( 3 ) / DIMOD
            TPOS( 2 ) = OYUN( 4 ) + OFS * OYUN( 3 ) / DIMOD

*  Construct the labelling string.
            CALL GRP_GET( GID, I, 1, NDFNAM, STATUS )
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL MSG_SETI( 'INDEX', I )
            CALL MSG_LOAD( ' ', LFMT, BUFFER, BL, STATUS )

*  Write the label in the correct position and orientation.  We set the
*  text background colour appropriately.
            CALL PGSTBG( LBGCOL )
            CALL AST_SETI( PLOT, 'Current', AST__BASE, STATUS )
            CALL AST_TEXT( PLOT, BUFFER( :BL ), TPOS, UP, JUST, STATUS )
            CALL AST_SETI( PLOT, 'Current', JCOM + JGRID( I ), STATUS )
            CALL PGSTBG( BGCOL )
         END DO
      END IF

*  Error exit label.
 99   CONTINUE

*  Close down AGI.
      CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Annul group resources.
      CALL CCD1_GRDEL( GID, STATUS )
      CALL CCD1_GRDEL( PENGID, STATUS )
      CALL CCD1_GRDEL( KEYGID, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'DRAWNDF_ERR',
     :                  'DRAWNDF: Boundary plotting failed.', STATUS )
      END IF

*  Close CCDPACK logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
