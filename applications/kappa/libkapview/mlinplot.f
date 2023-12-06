      SUBROUTINE MLINPLOT( STATUS )
*+
*  Name:
*     MLINPLOT

*  Purpose:
*     Draws a multi-line plot of the data values in a two-dimensional
*     NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MLINPLOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application plots a set of curves giving array value against
*     position in a two-dimensional NDF.  All the curves are drawn
*     within a single set of annotated axes.  Each curve is displaced
*     vertically by a specified offset to minimise overlap between the
*     curves.  These offsets may be chosen automatically or specified by
*     the user (see Parameter SPACE).  The curves may be drawn in
*     several different ways such as a "join-the-dots" plot, a
*     "staircase" plot, a "chain" plot, etc., (see Parameter MODE).
*
*     The data represented by each curve can be either a row or column
*     (chosen using Parameter ABSAXS) of any array component within the
*     supplied NDF (see Parameter COMP).  Vertical error bars may be
*     drawn if the NDF contains a VARIANCE component (see Parameter
*     ERRBAR).  The vertical axis of the plot represents array value
*     (or the logarithm of the array value---see Parameter YLOG).  The
*     horizontal axis represents position, and may be annotated using an
*     axis selected from the Current Frame of the NDF (see Parameter
*     USEAXIS).
*
*     Each curve may be labelled using its pixel index or a label
*     specified by the user (see Parameters LINLAB and LABELS).  The
*     appearance of these labels (size, colour, fount, horizontal
*     position, etc.) can be controlled using Parameter STYLE.  A key
*     may be produced to the left of the main plot listing the vertical
*     offsets of the curves (see Parameter KEY).  The appearance of the
*     key may be controlled using Parameter KEYSTYLE.  Its position may
*     be controlled using Parameter KEYOFF.  Markers indicating the zero
*     point for each curve may also be drawn within the main plot (see
*     Parameter ZMARK).
*
*     The bounds of the plot on both axes can be specified using
*     Parameters XLEFT, XRIGHT, YBOT, and YTOP.  If not specified they
*     take default values which encompass the entire supplied data set.
*     The current picture is usually cleared before plotting the new
*     picture, but Parameter CLEAR can be used to prevent this, allowing
*     several plots to be "stacked" together.  If a new plot is drawn
*     over an existing plot, then the bounds of the new plot are set
*     automatically to the bounds of the existing plot (XLEFT, XRIGHT,
*     YBOT, and YTOP are then ignored).

*  Usage:
*     mlinplot ndf [comp] lnindx [mode] [xleft] [xright] [ybot] [ytop]
*              [device]

*  ADAM Parameters:
*     ABSAXS = _INTEGER (Read)
*        This selects whether to plot rows or columns within the NDF.
*        If ABSAXS is 1, each curve will represent the array values
*        within a single row of pixels within the NDF.  If it is 2, each
*        curve will represent the array values within a single column of
*        pixels within the NDF.  [1]
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        plot.  If a null (!) value is supplied, FALSE is used if the
*        plot is being aligned with an existing plot (see Parameter
*        CLEAR), and TRUE is used otherwise.  Parameters USEAXIS and
*        YLOG determine the quantities used to annotated the horizontal
*        and vertical axes respectively.  The width of the margins left
*        for the annotation may be controlled using Parameter MARGIN.
*        The appearance of the axes (colours, founts, etc.) can be
*        controlled using the Parameter STYLE.  [!]
*     CLEAR = _LOGICAL (Read)
*        If TRUE the current picture is cleared before the plot is
*        drawn.  If CLEAR is FALSE not only is the existing plot
*        retained, but also the previous plot is used to specify the
*        axis limits.  [TRUE]
*     COMP = LITERAL (Read)
*        The NDF component to be plotted.  It may be "Data", "Quality",
*        "Variance", or "Error" (where "Error" is an alternative to
*        "Variance" and causes the square root of the variance values
*        to be displayed).  If "Quality" is specified, then the quality
*        values are treated as numerical values (in the range 0 to
*        255).  ["Data"]
*     DEVICE = DEVICE (Read)
*        The plotting device.  [current graphics device]
*     ERRBAR = _LOGICAL (Read)
*        TRUE if vertical error bars are to be drawn.  This is only
*        possible if the NDF contains a VARIANCE component, and
*        Parameter COMP is set to "Data".  The length of the error bars
*        (in terms of standard deviations) is set by Parameter SIGMA.
*        The appearance of the error bars (width, colour, etc.) can be
*        controlled using Parameter STYLE.  See also Parameter FREQ.
*        [FALSE]
*     FREQ = _INTEGER (Read)
*        The frequency at which error bars are to be plotted.  For
*        instance, a value of 2 would mean that alternate points have
*        error bars plotted.  This lets some plots be less cluttered.
*        FREQ must lie in the range 1 to half of the number of points
*        to be plotted.  FREQ is only accessed when Parameter ERRBAR is
*        TRUE.  [1]
*     KEY = _LOGICAL (Read)
*        TRUE if a key giving the offset of each curve is to be
*        produced.  The appearance of this key can be controlled using
*        Parameter KEYSTYLE, and its position can be controlled using
*        Parameter KEYPOS.  [TRUE]
*     KEYPOS() = _REAL (Read)
*        Two values giving the position of the key.  The first value
*        gives the gap between the right-hand edge of the multiple-line
*        plot and the left-hand edge of the key (0.0 for no gap, 1.0 for
*        the largest gap).  The second value gives the vertical position
*        of the top of the key (1.0 for the highest position, 0.0 for
*        the lowest).  If the second value is not given, the top of the
*        key is placed level with the top of the multiple-line plot.
*        Both values should be in the range 0.0 to 1.0.  If a key is
*        produced, then the right-hand margin specified by parameter
*        MARGIN is ignored.  [current value]
*     KEYSTYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use for the key (see Parameter KEY).
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
*        The heading in the key can be changed by setting a value for
*        the Title attribute (the supplied heading is split into lines
*        of no more than 17 characters).  The appearance of the heading
*        is controlled by attributes Colour(Title), Font(Title), etc.
*        The appearance of the curve labels is controlled by attributes
*        Colour(TextLab), Font(TextLab), etc. (the synonym Labels can be
*        used in place of TextLab).  The appearance of the offset values
*        is controlled by attributes Colour(NumLab), Font(NumLab), etc.
*        (the synonym Offset can be used in place of NumLab).  Offset
*        values are formatted using attributes Format(2), etc. (the
*        synonym Offset can be used in place of the value 2).
*        [current value]
*     LABELS = LITERAL (Read)
*        A group of strings with which to label the plotted curves.  A
*        comma-separated list of strings should be given, or the name
*        of a text file preceded by an up-arrow character "^".  Such
*        text files should contain further comma-separated lists which
*        will be read and interpreted in the same manner.  The first
*        string obtained is used as the label for the first curve
*        requested using Parameter LNINDX, the second string is used as
*        the label for the second curve, etc.  If the number of supplied
*        strings is fewer than the number of curves requested using
*        LNINDX, then extra default labels are used.  These are equal to
*        the NDF pixel index of the row or column, preceded by a hash
*        character ("#").  If a null (!) value is supplied for LABELS,
*        then default labels are used for all curves.  [!]
*     LINLAB = _LOGICAL (Read)
*        If TRUE, the curves in the plot will be labelled using the
*        labels specified by Parameter LABELS.  A single label is placed
*        in-line with the curve.  The horizontal position and appearance
*        of these labels can be controlled using Parameter STYLE.
*        [TRUE]
*     LNINDX = LITERAL (Read)
*        Specifies the NDF pixel indices of the rows or columns to be
*        displayed (see Parameter ABSAXS).  A maximum of 100 lines may
*        be selected.  It can take any of the following values.
*
*        - "ALL" or "*" --  All lines (rows or columns).
*
*        - "xx,yy,zz" -- A list of line indices.
*
*        - "xx:yy" --  Line indices between xx and yy inclusively.  When
*        xx is omitted the range begins from the lower bound of the line
*        dimension; when yy is omitted the range ends with the maximum
*        value it can take, that is the upper bound of the line
*        dimension or the maximum number of lines this routine can plot.
*
*        - Any reasonable combination of above values separated by
*        commas.
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave around the multiple-line
*        plot for axis annotation.  The widths should be given as
*        fractions of the corresponding dimension of the current
*        picture.  Four values may be given, in the order; bottom,
*        right, top, left.  If fewer than four values are given, extra
*        values are used equal to the first supplied value.  If these
*        margins are too narrow any axis annotation may be clipped.  See
*        also Parameter KEYPOS.  [current value]
*     MARKER = _INTEGER (Read)
*        This parameter is only accessed if Parameter MODE is set to
*        "Chain" or "Mark".  It specifies the symbol with which each
*        position should be marked, and should be given as an integer
*        PGPLOT marker type.  For instance, 0 gives a box, 1 gives a
*        dot, 2 gives a cross, 3 gives an asterisk, 7 gives a triangle.
*        The value must be larger than or equal to -31.  [current value]
*     MODE = LITERAL (Read)
*        Specifies the way in which each curve is drawn.  MODE can take
*        the following values.
*
*        - "Histogram" -- An histogram of the points is plotted in the
*        style of a "staircase" (with vertical lines only joining the y
*        values and not extending to the base of the plot).  The
*        vertical lines are placed midway between adjacent x positions.
*
*        - "Line" -- The points are joined by straight lines.
*
*        - "Point" -- A dot is plotted at each point.
*
*        - "Mark" -- Each point is marker with a symbol specified by
*        Parameter MARKER.
*
*        - "Chain" -- A combination of "Line" and "Mark".
*
*        [current value]
*     NDF = NDF (Read)
*        NDF structure containing the array to be plotted.
*     OFFSET() = _DOUBLE (Read)
*        This parameter is used to obtain the vertical offsets for the
*        data curve when Parameter SPACE is given the value "Free".  The
*        number of values supplied should equal the number of curves
*        being drawn.
*     PENS = GROUP (Read)
*        A group of strings, separated by semicolons, each of which
*        specifies the appearance of a pen to be used to draw a curve.
*        The first string in the group describes the pen to use for the
*        first curve, the second string describes the pen for the
*        second curve, etc.  If there are fewer strings than curves,
*        then the supplied pens are cycled through again, starting at
*        the beginning.  Each string should be a comma-separated list of
*        plotting attributes to be used when drawing the curve.  For
*        instance, the string "width=0.02,colour=red,style=2" produces a
*        thick, red, dashed curve.  Attributes which are unspecified in
*        a string default to the values implied by Parameter STYLE.  If
*        a null value (!) is given for PENS, then the pen attributes
*        implied by Parameter STYLE are used.  [!]
*     SIGMA = LITERAL (Read)
*        If vertical error bars are produced (see Parameter ERRBAR),
*        then SIGMA gives the number of standard deviations that the
*        error bars are to represent.  [current value]
*     SPACE = LITERAL (Read)
*        The value of this parameter specifies how the vertical offset
*        for each data curve is determined.  It should be given one of
*        the following values:
*
*        - "Average" -- The offsets are chosen automatically so that
*        the average data values of the curves are evenly spaced between
*        the upper and lower limits of the plotting area.  Any line-
*        to-line striping is thus hidden and the amount of overlap of
*        adjacent traces is minimised.
*
*        - "Constant" -- The offsets are chosen automatically so that
*        the zero points of the curves are evenly spaced between the
*        upper and lower limits of the plotting area.  The width of any
*        line-to-line strip is constant, which could result in the
*        curves becoming confused if the bias of a curve from its zero
*        point is so large that it overlaps another curve.
*
*        - "Free" -- The offsets to use are obtained explicitly using
*        Parameter OFFSET.
*
*        - "None" -- No vertical offsets are used.  All curves are
*        displayed with the same zero point.
*
*        The input can be abbreviated to an unambiguous length and
*        is case insensitive.  ["Average"]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use when drawing the annotated axes, data curves, error bars,
*        zero markers, and curve labels.
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

*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported).
*
*        The appearance of the data curves is controlled by the
*        attributes Colour(Curves), Width(Curves), etc. (the synonym
*        Lines may be used in place of Curves).  The appearance of
*        markers used if Parameter MODE is set to "Point", "Mark" or
*        "Chain" is controlled by Colour(Markers), Width(Markers), etc.
*        (the synonym Symbols may be used in place of Markers).  The
*        appearance of the error bars is controlled using
*        Colour(ErrBars), Width(ErrBars), etc.  (see Parameter ERRBAR).
*        The appearance of the zero-point markers is controlled using
*        Colour(ZeroMark), Size(ZeroMark), etc.  The appearance of the
*        curve labels is controlled using Colour(Labels), Size(Labels),
*        etc.  LabPos(Left) controls the horizontal position of the
*        in-line curve label (see Parameter LINLAB), and LabPos(Right)
*        controls the horizontal position of the curve label associated
*        with the right-hand zero-point marker (see Parameter ZMARK).
*        LabPos without any qualifier is equivalent to LabPos(Left).
*        LabPos values are floating point, with 0.0 meaning the left
*        edge of the plotting area, and 1.0 the right edge.  Values
*        outside the range 0 to 1 may be used.   [current value]
*     USEAXIS = LITERAL (Read)
*        The quantity to be used to annotate the horizontal axis
*        of the plot specified by using one of the following options.
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
*        The quantity used to annotate the horizontal axis must have a
*        defined value at all points in the array, and must increase or
*        decrease monotonically along the array.  For instance, if RA is
*        used to annotate the horizontal axis, then an error will be
*        reported if the profile passes through RA=0 because it will
*        introduce a non-monotonic jump in axis value (from 0h to 24h,
*        or 24h to 0h).  If a null (!) value is supplied, the value of
*        Parameter ABSAXS is used.  [!]
*     XLEFT = LITERAL (Read)
*        The axis value to place at the left-hand end of the horizontal
*        axis.  If a null (!) value is supplied, the value used is the
*        first element in the data being displayed.  The value supplied
*        may be greater than or less than the value supplied for XRIGHT.
*        A formatted value for the quantity specified by parameter
*        USEAXIS should be supplied.  [!]
*     XRIGHT = LITERAL (Read)
*        The axis value to place at the right-hand end of the horizontal
*        axis.  If a null (!) value is supplied, the value used is the
*        last element in the data being displayed.  The value supplied
*        may be greater than or less than the value supplied for XLEFT.
*        A formatted value for the quantity specified by parameter
*        USEAXIS should be supplied.  [!]
*     YBOT = _DOUBLE (Read)
*        The data value to place at the bottom end of the vertical axis.
*        If a null (!) value is supplied, the value used is the lowest
*        data value to be displayed, after addition of the vertical
*        offsets.  The value supplied may be greater than or less than
*        the value supplied for YTOP.  [!]
*     YLOG = _LOGICAL (Read)
*        TRUE if the value displayed on the vertical axis is to be the
*        logarithm of the supplied data values.  If TRUE, then the
*        values supplied for parameters YTOP and YBOT should be values
*        for the logarithm of the data value, not the data value itself.
*        [FALSE]
*     YTOP = _DOUBLE (Read)
*        The data value to place at the top end of the vertical axis.
*        If a null (!) value is supplied, the value used is the highest
*        data value to be displayed, after addition of the vertical
*        offsets.  The value supplied may be greater than or less than
*        the value supplied for YBOT.  [!]
*     ZMARK = _LOGICAL (Read)
*        If TRUE, then a pair of short horizontal lines are drawn at the
*        left and right edges of the main plot for each curve.  The
*        vertical position of these lines corresponds to the zero point
*        for the corresponding curve.  The right-hand marker is
*        annotated with the curve label (see Parameter LABELS).  The
*        appearance of these markers can be controlled using the
*        Parameter STYLE.  [TRUE]

*  Examples:
*     mlinplot rcw3_b1 reset \
*        Plot the first five rows of the two-dimensional NDF file,
*        rcw3_b1 on the current graphics device.  The lines are offset
*        such that the averages of the rows are evenly separated in the
*        direction of the vertical axis.
*     mlinplot rcw3_b1 lnindx="1,3,5,7:10" \
*        Plot the rows 1, 3, 5, 7, 8, 9 and 10 of the two-dimensional
*        NDF file, rcw3_b1, on the current graphics device.
*     mlinplot rcw3_b1 lnindx=* \
*        Plot all rows of the two-dimensional NDF file, rcw3_b1, on the
*        current graphics device.
*     mlinplot rcw3_b1 lnindx=* style="colour(curve)=red+width(curve)=4" \
*        As the previous example, but the rows are drawn in red at four
*        times normal thickness.  The change of line coluor persists
*        to the next invocation, but not the temporary widening of the
*        lines.
*     mlinplot rcw3_b1 lnindx=* style="+width(curve)=4" \
*        As the previous example, but now the rows are drawn in the
*        current line colour.
*     mlinplot rcw3_b1 absaxs=2 lnindx="20:25,30,31" \
*        Plot columns 20, 21, 22, 23, 24, 25, 30 and 31 of the
*        two-dimensional NDF file, rcw3_b1, on the current graphics
*        device.
*     mlinplot rcw3_b1 style="Title=CRDD rcw3_b1" \
*        Plot the currently selected rows of the two-dimensional NDF
*        file, rcw3_b1, on the current graphics device.  The plot has a
*        title of "CRDD rcw3_b1".
*     mlinplot rcw3_b1(100:500,) ybot=0.0 ytop=1.0E-3 \
*        Plot the currently selected rows of the two-dimensional NDF,
*        rcw3_b1, between column 100 and column 500.  The vertical
*        display range is from 0.0 to 1.0E-3.
*     mlinplot rcw3_b1 space=constant device=ps_p \
*        Plot the currently selected rows of the two-dimensional NDF
*        file, rcw3_b1, on the ps_p device.  The base lines are evenly
*        distributed over the range of the vertical axis.
*     mlinplot rcw3_b1 space=free offset=[0.,2.0E-4,4.0E-4,6.0E-4,0.1] \
*        Plot the currently selected rows of the two-dimensional NDF
*        file, rcw3_b1.  The base lines are set at 0.0 for the first
*        row, 2.0E-4 for the second, 4.0E-4 for the third, 6.0E-4 for
*        the fourthm and 0.1 for the fifth.

*  Notes:
*     -  If no Title is specified via the STYLE parameter, then the
*     TITLE component in the NDF is used as the default title for the
*     annotated axes.  If the NDF does not have a TITLE component, then
*     the default title is taken from current co-ordinate Frame in the
*     NDF.  If this has not been set explicitly, then the name of the
*     NDF is used as the default title.
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME picture containing the
*     annotated axes, data plot, and optional key; a KEY picture to
*     store the key if present; and a DATA picture containing just the
*     data plot.  Note, the FRAME picture is only created if annotated
*     axes or a key has been drawn, or if non-zero margins were
*     specified using Parameter MARGIN.

*  Related Applications:
*     KAPPA: LINPLOT; Figaro: ESPLOT, IPLOTS, MSPLOT, SPLOT, SPECGRID;
*     SPLAT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     QUALITY, LABEL, TITLE, WCS and UNITS components of the NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Only
*     double-precision floating-point data can be processed directly.
*     Other non-complex data types will undergo a type conversion
*     before the plot is drawn.

*  Copyright:
*     Copyright (C) 1999, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council.  Copyright (C) 2010, 2023 Science & Technology
*     Facilities Council. All Rights Reserved.

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
*     Malcolm Currie STARLINK (RAL::CUR)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-AUG-1999 (DSB):
*        Original AST version, based on earlier version by MJC.
*     26-OCT-1999 (DSB):
*        Made MARGIN a fraction of the current picture, not the DATA
*        picture.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     19-NOV-2004 (DSB):
*        Correct use of CNF_PVAL.
*     27-JAN-2006 (DSB):
*        Ignore blank titles supplied in STYLE.
*     6-FEB-2006 (DSB):
*        Use KPG1_ASTTL to get the title.
*     2006 April 12 (MJC):
*        Remove unused variables, remove contour-plot references,
*        corrected punctuation, and wrapped long lines.
*     20-JUL-2006 (TIMJ):
*        Fix valgrind warning with IPVIN.
*     2010 October 6 (MJC):
*        Permit temporary style attributes.
*     2023 December 6 (MJC):
*        Increase the maximum number of plots tenfold.  While 100 is
*        sensible for labelled plots, many more can be accommodated
*        without labelling, say for examining time-series spectra.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'AST_PAR'          ! AST constants and function
                                 ! declarations
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER NDIM               ! Dimensionality of input array
      PARAMETER( NDIM = 2 )

      INTEGER MXLIN              ! Max. number of lines that can be
      PARAMETER( MXLIN = 1000 )  ! displayed

      REAL KW                    ! Width of key as a fraction of width
      PARAMETER( KW = 0.15 )     ! of current picture

*  Local Variables:
      CHARACTER CLONUM*( 5 )     ! String of low-limit of default list
      CHARACTER COMP*( 8 )       ! Component to be displayed
      CHARACTER CUPNUM*( 5 )     ! String of up-limit of default list
      CHARACTER DEFLIS*( 20 )    ! Default list of the selection
      CHARACTER MCOMP*( 8 )      ! Component to be mapped
      CHARACTER NDFNAM*( 255 )   ! Full NDF specification
      CHARACTER TEXT*( 255 )     ! A general text string
      CHARACTER UNITS*( 20 )     ! Units of the data
      DOUBLE PRECISION BOX( 4 )  ! Bounds of DATA picture
      DOUBLE PRECISION GOFF( MXLIN ) ! Curve offsets as GRAPHICS axis-2
                                 ! values
      DOUBLE PRECISION OFFSET( MXLIN )!  Offset for each data curve
      DOUBLE PRECISION XL        ! Left annotated axis value limit
      DOUBLE PRECISION XR        ! Right annotated axis value limit
      DOUBLE PRECISION YB        ! Nominal data value at bottom
      DOUBLE PRECISION YT        ! Nominal data value at top
      INTEGER ABSAXS             ! Index of abscissa grid axis
      INTEGER ABSDIM             ! Length of abscissa grid axis
      INTEGER AXMAPS( 2 )        ! Axis mappings for displayed data plot
      INTEGER CFRM               ! Pointer to Current Frame from NDF WCS
                                 ! FrameSet
      INTEGER DEFLN              ! Used length of string DEFLIS
      INTEGER DIM( NDIM )        ! Number of elements in the input array
      INTEGER EL                 ! Number of mapped elements
      INTEGER FREQ               ! Interval between error bars
      INTEGER FSET               ! Pointer to FrameSet
      INTEGER I                  ! General variable
      INTEGER IAXIS              ! Index of axis used to annotate
                                 ! horizontal axis
      INTEGER IGRP               ! Group containing textual curve labels
      INTEGER IMODE              ! Mode identifier
      INTEGER INDF               ! NDF identifier for input NDF
      INTEGER IPBAR              ! Pointer to error bar values
      INTEGER IPDAT              ! Pointer to nominal data values
      INTEGER IPDIN              ! Pointer to input data array
      INTEGER IPICD              ! AGI id.  for DATA picture
      INTEGER IPICF              ! AGI id.  for new FRAME picture
      INTEGER IPICK              ! AGI id.  for the KEY picture
      INTEGER IPLOT              ! Pointer to AST Plot for DATA picture
      INTEGER IPLOTK             ! Pointer to AST Plot for KEY picture
      INTEGER IPNOM              ! Pointer to nominal GRID values
      INTEGER IPVIN              ! Pointer to input variance array
      INTEGER IPW1               ! Pointer to work space
      INTEGER IWCS               ! Pointer to the WCS FrameSet from NDF
      INTEGER LNINDX( MXLIN )    ! Indices of the lines to plot
      INTEGER LUTMAP             ! Mapping from nominal GRID to
                                 ! annotated axis
      INTEGER MTYPE              ! PGPLOT marker type
      INTEGER NAX                ! No.  of axes in current Frame
      INTEGER NC                 ! No.  of characters in NDFNAM
      INTEGER NCU                ! Number of characters in the units
      INTEGER NDISP              ! No.  of lines to display
      INTEGER NFRM               ! Frame index increment between IWCS
                                 ! and IPLOT
      INTEGER NKP                ! Number  of values supplied for
                                 ! Parameter KEYPOS
      INTEGER NLONUM             ! Used length of string CLONUM
      INTEGER NMARG              ! No.  of margin values given
      INTEGER NUPNUM             ! Used length of string CUPNUM
      INTEGER ORDAXS             ! Index of ordinate grid axis
      INTEGER SDIM( NDIM )       ! The significant NDF axes
      INTEGER SLBND( NDIM )      ! Significant lower bounds of the image
      INTEGER SUBND( NDIM )      ! Significant upper bounds of the image
      INTEGER WCSMAP             ! Mapping from NDF GRID to current
                                 ! Frame
      INTEGER WWGOT              ! Index of "what we've got" Frame in
                                 ! FSET
      INTEGER WWWANT             ! Index of "what we want" Frame in FSET
      LOGICAL ALIGN              ! DATA picture aligned with a previous
                                 ! picture?
      LOGICAL AXES               ! Produce annotated axes?
      LOGICAL ERRBAR             ! Display error bars?
      LOGICAL KEY                ! Key of the curve offsets to be
                                 ! produced?
      LOGICAL USE( MXLIN )       ! Should a line be used?
      LOGICAL VAR                ! Variance component available
      LOGICAL YLOG               ! Show log of data value?
      REAL DEFMAR( 4 )           ! Default margins round DATA picture
      REAL DUMMY                 ! Un-required argument value
      REAL KEYOFF                ! Offset to top of key
      REAL KEYPOS( 2 )           ! Key position
      REAL MARGIN( 4 )           ! Width of margins round DATA picture
      REAL SIGMA                 ! No.  of std.  devn's for Y error bars
      REAL Y1,Y2                 ! Vertical bounds of PGPLOT viewport
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise pointers to hold zero so that we can see which ones have
*  been used when it is time to tidy up.
      IPW1 = 0

*  Access the input NDF and get WCS information.
*  =============================================

*  Obtain the identifier of the NDF to be ploted.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  Find which component to plot.
      CALL KPG1_ARCOG( 'COMP', INDF, MCOMP, COMP, STATUS )

*  Obtain the units if present.
      CALL KPG1_DAUNI( INDF, MCOMP, UNITS, NCU, STATUS )

*  Get an AST pointer to a FrameSet describing the co-ordinate Frames
*  present in the NDF's WCS component.  This application assumes a
*  two-dimensional data array.  Check the NDF has no more than two
*  significant dimensions (i.e. axes spanning more than one pixel).  If
*  it only has one significant axis, pretend it is two-dimensional with
*  the second axis spanning only a single pixel (1:1).  The Current
*  Frame from the NDFs WCS FrameSet is retained unchanged.  We do not
*  check that the inverse Mapping is defined since the inverse will be
*  implemented within KPS1_MLPNG by a look-up table (if possible).
      CALL KPG1_ASGET( INDF, NDIM, .FALSE., .FALSE., .FALSE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Save the lengths of the significant axes.
      DIM( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
      DIM( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1

*  Get a pointer to the Current Frame.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Save the number of current Frame axes.
      NAX = AST_GETI( CFRM, 'NAXES', STATUS )

*  Get the simplified Mapping from GRID Frame to Current Frame.
      WCSMAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE,
     :                                       AST__CURRENT, STATUS ),
     :                       STATUS )

*  Find which significant axis will be regarded as abscissa, and which
*  as the line-index (ordinate) axis.
      CALL PAR_GET0I( 'ABSAXS', ABSAXS, STATUS )
      ORDAXS = 3 - ABSAXS

*  Use shorthand for abscissa dimension.
      IF ( ABSAXS .EQ. 1 ) THEN
         ABSDIM = DIM( 1 )
      ELSE
         ABSDIM = DIM( 2 )
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  See which Current Frame axis should be used for the horizontal axis
*  annotation.  Choose from all the axes of the WCS Current Frame.  Use
*  a dynamic default equal to ABSAXS.
      IAXIS = ABSAXS
      CALL KPG1_GTAXI( 'USEAXIS', CFRM, 1, IAXIS, STATUS )

*  Obtain the indices of lines to plot.  First get a temporary array for
*  use when selecting the lines to display.
      CALL PSX_CALLOC( DIM( ORDAXS ), '_INTEGER', IPW1, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Construct the default numbers.  Set low-limit of selectable
*  numbers as the low-limit of the default list.
      CALL CHR_ITOC( SLBND( ORDAXS ), CLONUM, NLONUM )

*  If the total number of selectable numbers is less then 5, set
*  up-limit of selectable number as the upper limit of default list.
      IF ( DIM( ORDAXS ) .LT. 5 ) THEN
         CALL CHR_ITOC( SUBND( ORDAXS ), CUPNUM, NUPNUM )

*  If the total number is more than 5, set the fifth from bottom as
*  upper limit of default list.
      ELSE
         CALL CHR_ITOC( SLBND( ORDAXS ) + 4, CUPNUM, NUPNUM )
      END IF

*  Set the default value for the parameter.
      DEFLIS = CLONUM( : NLONUM )//':'//CUPNUM( : NUPNUM )
      DEFLN = NLONUM + NUPNUM + 1
      CALL PAR_DEF0C( 'LNINDX', DEFLIS( : DEFLN ), STATUS )

*  Get the indices of the lines to be displayed.
      CALL KPG1_GILST( SLBND( ORDAXS ), SUBND( ORDAXS ), MXLIN,
     :                 'LNINDX', %VAL( CNF_PVAL( IPW1 ) ),
     :                 LNINDX, NDISP,
     :                 STATUS )

*  If an error occurred, exit.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If the number of lines to plot is 0, report and exit.
      IF ( NDISP .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'MLINPLOT_NULIN', 'There are no lines to plot.',
     :                 STATUS )
         GO TO 999
      END IF

*  See if the Y axis is to display logged data values.
      CALL PAR_GET0L( 'YLOG', YLOG, STATUS )

*  Get the horizontal positions of every value to be displayed.
      CALL KPS1_MLPNG( ABSDIM, NDISP,  SLBND( ORDAXS ), LNINDX, WCSMAP,
     :                 ABSAXS, CFRM, IAXIS, IPNOM, LUTMAP, XL, XR,
     :                 STATUS )

*  Map the component of the NDF which is to be plotted on the vertical
*  axis, in double precision.
      CALL NDF_MAP( INDF, MCOMP, '_DOUBLE', 'READ', IPDIN, EL,
     :              STATUS )

*  See if error bars are required.
      CALL PAR_GET0L( 'ERRBAR', ERRBAR, STATUS )

*  If so...
      IPVIN = 0
      IF ( ERRBAR ) THEN

*  Issue a warning if the data being displayed is not from the DATA
*  component.
         IF ( MCOMP .NE. 'Data' ) THEN
            CALL MSG_SETC( 'C', MCOMP )
            CALL MSG_OUT( 'MLINPLOT_MSG', 'Cannot display error bars '//
     :                    'for the ^C component.', STATUS )
            ERRBAR = .FALSE.

*  Otherwise, see if the NDF has a variance component.
         ELSE
            CALL NDF_STATE( INDF, 'VARIANCE', VAR, STATUS )

*  Issue a warning if the NDF has no variance component.
            IF ( .NOT. VAR ) THEN
               CALL NDF_MSG( 'NDF', INDF )
               CALL MSG_OUT( 'MLINPLOT_MSG', 'Cannot display error '//
     :                       'bars because ''^NDF'' has no Variance '//
     :                       'component.', STATUS )
               ERRBAR = .FALSE.

*  Otherwise, map the variance values.
            ELSE
               CALL NDF_MAP( INDF, 'VARIANCE', '_DOUBLE', 'READ', IPVIN,
     :                       EL, STATUS )

*  See how many standard deviations are to be used for a vertical error
*  bar.
               CALL PAR_GET0R( 'SIGMA', SIGMA, STATUS )

*  Obtain the spacing between points showing the error bars.
               CALL PAR_GDR0I( 'FREQ', 1, 1, MAX( 1, ABSDIM/2 ), .TRUE.,
     :                         FREQ, STATUS )

            END IF

         END IF

      END IF

*  Get the vertical positions of every value to be displayed.
      CALL KPS1_MLPND( DIM( 1 ), DIM( 2 ), %VAL( CNF_PVAL( IPDIN ) ),
     :                 %VAL( CNF_PVAL( IPVIN ) ),
     :                 ERRBAR, SIGMA, ABSAXS, NDISP, SLBND( ORDAXS ),
     :                 LNINDX, YLOG, IPNOM, USE, IPDAT, IPBAR, OFFSET,
     :                 YB, YT, STATUS )

*  Construct a FrameSet holding two two-dimensional Frames.  The first
*  axis in the Base Frame is the nominal GRID axis (see KPS1_MLPNG), and
*  the second axis is the nominal data value axis (see KPS1_MLPND).  The
*  first axis in the Current Frame is the select abscissa axis from the
*  NDFs Current Frame, and the second axis is the nominal data-value
*  axis.
      CALL KPS1_MLPFS( LUTMAP, INDF, CFRM, IAXIS, YLOG, MCOMP,
     :                 UNITS( : NCU ), FSET, STATUS )

*  Save the indices of the "what we've got" and "what we want" Frames.
      WWGOT = 1
      WWWANT = 2

*  Get the plotting mode.
      CALL PAR_CHOIC( 'MODE', 'Line', 'Histogram,Line,Point,Mark,Chain',
     :                .FALSE., TEXT, STATUS )

*  Get an identifier for the mode, and get the marker type if required.
      MTYPE = 0
      IF ( TEXT .EQ. 'HISTOGRAM' ) THEN
         IMODE = 1
      ELSE IF ( TEXT .EQ. 'LINE' ) THEN
         IMODE = 2
      ELSE IF ( TEXT .EQ. 'POINT' ) THEN
         IMODE = 3
         MTYPE = -1
      ELSE IF ( TEXT .EQ. 'MARK' ) THEN
         IMODE = 3
         CALL PAR_GET0I( 'MARKER', MTYPE, STATUS )

c  STEP mode is not yet available.
c      ELSE IF ( TEXT .EQ. 'STEP' ) THEN
c         IMODE = 4

      ELSE
         IMODE = 5
         CALL PAR_GET0I( 'MARKER', MTYPE, STATUS )
      ENDIF

*  Ensure marker type (if used) is legal.
      MTYPE = MAX( -31, MTYPE )

*  Start the graphics system.
*  ==========================

*  Set the dynamic defaults for MARGIN.
      DEFMAR( 1 ) = 0.15
      DEFMAR( 2 ) = 0.2
      DEFMAR( 3 ) = 0.15
      DEFMAR( 4 ) = 0.2
      CALL PAR_DEF1R( 'MARGIN', 4, DEFMAR, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the MARGIN values.
      CALL PAR_GDRVR( 'MARGIN', 4, -0.49, 10.0, MARGIN, NMARG, STATUS )

*  If a null value was supplied, annul the error and use the default.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MARGIN( 1 ) = DEFMAR( 1 )
         MARGIN( 2 ) = DEFMAR( 2 )
         MARGIN( 3 ) = DEFMAR( 3 )
         MARGIN( 4 ) = DEFMAR( 4 )
         NMARG = 4
      END IF

*  Only use the first 4 values.
      NMARG = MIN( 4, NMARG )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Use the first value for any unspecified edges.
      DO I = NMARG + 1, 4
         MARGIN( I ) = MARGIN( 1 )
      END DO

*  Store the bounds for the new DATA picture.  These are only used if
*  the new DATA picture is not based on an existing DATA picture.  Note,
*  the corresponding PGPLOT window created by KPG1_PLOT will have world
*  co-ordinates of millimetres from the bottom-left corner of the view
*  surface, NOT pixels.  This box is only used to define the bounds of
*  the picture within the AGI database for the benefit of non-AST
*  applications.
      BOX( 1 ) = XL
      BOX( 2 ) = YB
      BOX( 3 ) = XR
      BOX( 4 ) = YT

*  Generate a reference for the NDF to be stored in the graphics
*  database.
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_LOAD( ' ', '^NDF', NDFNAM, NC, STATUS )

*  Establish synonyms for AST graphical element names to be recognised
*  during the following call to KPG1_PLOT.
      CALL KPG1_ASPSY( '(LIN*ES)', '(CURVES)', STATUS )

*  Start up the graphics system.  This stores a new DATA picture in the
*  AGI database with the given bounds.  A KEY picture is also created if
*  necessary, together with an enclosing FRAME picture.  The PGPLOT
*  viewport is set so that it matches the area of the DATA picture.
*  World co-ordinates within the PGPLOT window are set to millimetres
*  from the bottom-left corner of the view surface.  An AST Plot is
*  returned for drawing in the DATA picture.  The Base (GRAPHICS) Frame
*  in the Plot corresponds to millimetres from the bottom-left corner
*  of the view port, and the Current Frame is inherited from the
*  supplied FrameSet (i.e. the Current Frame is the "What we want"
*  Frame).

*  First deal with cases where a key is required...
      CALL PAR_GET0L( 'KEY', KEY, STATUS )
      IF ( KEY ) THEN

*  Get the position required for the key.  The margin between DATA and
*  KEY Frames is determined by the horizontal position requested for
*  the key.
         CALL PAR_GDRVR( 'KEYPOS', 2, -1.0, 1.0, KEYPOS, NKP, STATUS )
         IF ( KEYPOS( 1 ) .GE.  0.0 ) THEN
            MARGIN( 2 ) = KEYPOS( 1 )
         ELSE
            MARGIN( 2 ) = KEYPOS( 1 ) - KW
         END IF

*  Start up the graphics system, creating a KEY picture.
         CALL KPG1_PLOT( FSET, 'UNKNOWN', 'KAPPA_MLINPLOT',
     :                   NDFNAM( : NC ), MARGIN, 1, 'KEY', 'R', KW,
     :                   0.0, 'DATAPLOT', BOX, IPICD, IPICF, IPICK,
     :                   IPLOT, NFRM, ALIGN, STATUS )

*  Otherwise, start up the graphics system, creating no KEY picture.
      ELSE
         CALL KPG1_PLOT( FSET, 'UNKNOWN', 'KAPPA_MLINPLOT',
     :                   NDFNAM( : NC ), MARGIN, 0, ' ', ' ', 0.0,
     :                   0.0, 'DATAPLOT', BOX, IPICD, IPICF, IPICK,
     :                   IPLOT, NFRM, ALIGN, STATUS )
      END IF

*  Ensure the Title attribute of the Plot has a useful value.
      CALL KPG1_ASTTL( IPLOT, IWCS, INDF, STATUS )

*  Produce the plot.
*  =================
*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  See if axes are required.  Default is YES unless the plot was drawn
*  over an existing plot.
      CALL PAR_DEF0L( 'AXES', .NOT.  ALIGN, STATUS )
      CALL PAR_GET0L( 'AXES', AXES, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         AXES = .NOT.  ALIGN
      END IF

*  Draw the grid if required.
      IF ( AXES ) CALL KPG1_ASGRD( IPLOT, IPICF, .TRUE., STATUS )

*  Make the "What we've got" Frame Current in the Plot.  Axis 1 is
*  nominal grid value, and axis 2 is nominal data value.
      CALL AST_SETI( IPLOT, 'CURRENT', WWGOT + NFRM, STATUS )

*  Get the one-dimensional mappings which transform each of the GRAPHICS
*  Frame axes on to the corresponding "what we've got" Frame axes.
      CALL KPG1_ASSPL( IPLOT, 2, AXMAPS, STATUS )

*  Loop round each usable line.
      DO I = 1, NDISP
         IF ( USE( I ) ) THEN

*  Map all the required axis values from "what we've got" into GRAPHICS.
            CALL AST_TRAN1( AXMAPS( 1 ), ABSDIM,
     :                      %VAL( CNF_PVAL( IPNOM ) + ( I - 1 )*ABSDIM*
     :                            VAL__NBD ), .FALSE.,
     :                      %VAL( CNF_PVAL( IPNOM ) + ( I - 1 )*ABSDIM*
     :                            VAL__NBD ), STATUS )

            CALL AST_TRAN1( AXMAPS( 2 ), ABSDIM,
     :                      %VAL( CNF_PVAL( IPDAT ) + ( I - 1 )*ABSDIM*
     :                            VAL__NBD ), .FALSE.,
     :                      %VAL( CNF_PVAL( IPDAT ) + ( I - 1 )*ABSDIM*
     :                            VAL__NBD ), STATUS )

*  If required, map the error bar limits into GRAPHICS.
            IF ( ERRBAR ) THEN

*  Do the transformation.
               CALL AST_TRAN1( AXMAPS( 2 ), 2*ABSDIM,
     :                         %VAL( CNF_PVAL( IPBAR ) + 2*( I - 1 )*
     :                               ABSDIM*VAL__NBD ), .FALSE.,
     :                         %VAL( CNF_PVAL( IPBAR ) + 2*( I - 1 )*
     :                               ABSDIM*VAL__NBD ), STATUS )

            END IF

         END IF

      END DO

*  Produce the data curves.  The plus requests support of temporary
*  STYLE attributes.  Also allow for more than one call to KPG1_ASSET
*  for STYLE by using the delimiter suffix.
      CALL KPS1_MLPML( NDISP, USE, ABSDIM, 1, ABSDIM,
     :                 %VAL( CNF_PVAL( IPNOM ) ),
     :                 %VAL( CNF_PVAL( IPDAT ) ), .FALSE., ERRBAR,
     :                 %VAL( CNF_PVAL( IPBAR ) ),
     :                 %VAL( CNF_PVAL( IPBAR ) ),
     :                 %VAL( CNF_PVAL( IPBAR ) ),
     :                 '+STYLE+', 'PENS', IPLOT, IMODE, MTYPE, 1, FREQ,
     :                 'KAPPA_MLINPLOT', STATUS )

*  Transform the curve offsets into GRAPHICS Frame axis-2 values.
      CALL AST_TRAN1( AXMAPS( 2 ), NDISP, OFFSET, .FALSE., GOFF,
     :                STATUS )

*  Add annotation to the plot (zero point markers and curve labels).
*  The plus requests support of temporary STYLE attributes.
      CALL KPS1_MLPLB( NDISP, USE, ABSDIM, %VAL( CNF_PVAL( IPNOM ) ),
     :                 %VAL( CNF_PVAL( IPDAT ) ), GOFF,
     :                 IPLOT, '+STYLE', 'PENS', 'LABELS', 'LINLAB',
     :                 'KAPPA_MLINPLOT', LNINDX, KEY, IGRP, STATUS )

*  Plot the key if necessary.
      IF ( KEY ) THEN

*  If no value was supplied for the vertical position of the KEY using
*  Parameter KEYPOS, find the value which puts the top of the key level
*  with the top of the DATA picture.
         IF ( NKP .LT. 2 ) THEN

*  Report an error if there is insufficient room within the current
*  picture for the key.
            IF ( IPICK .EQ. -1 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'MLINPLOT_KEY', 'There is insufficient '//
     :                       'room in the current picture for a key.',
     :                       STATUS )
               GO TO 999
            END IF

*  We need to know the position of the top of the DATA picture so that
*  the top of the key can be put at the same height on the screen.  Get
*  the bounds of the current PGPLOT viewport, in mm.  Only the vertical
*  position at the top is needed.
            CALL PGQVP( 2, DUMMY, DUMMY, DUMMY, KEYOFF )

*  Activate the KEY picture.  This returns a pointer to an AST Plot
*  which can be used to draw in the KEY picture, and sets the current
*  PGPLOT viewport so that it corresponds to the KEY picture.
            CALL KPG1_GDGET( IPICK, AST__NULL, .FALSE., IPLOTK, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find the vertical position in the key picture which corresponds to
*  the top of the DATA picture, as a fraction of the height of the key
*  picture.
            CALL PGQVP( 2, DUMMY, DUMMY, Y1, Y2 )
            KEYOFF = ( KEYOFF - Y1 )/( Y2 - Y1 )

*  If the horizontal positions was given using Parameter KEYPOS, just
*  activate the KEY picture.
         ELSE
            KEYOFF = KEYPOS( 2 )
            CALL KPG1_GDGET( IPICK, AST__NULL, .FALSE., IPLOTK, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 999
         END IF

*  Cancel the Title set for the Current Frame in the key Plot.  This
*  will be "Graphical co-ordinates" and is not useful.  Clearing the
*  Title will allow KPS1_MLPKY to use its own more appropriate title.
         CALL AST_CLEAR( IPLOTK, 'TITLE', STATUS )

*  Ensure that any previous synonyms for AST attributes are cleared.
         CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Establish some synonyms for AST attribute names to be used when
*  setting the plotting style for the key.  Note, the order is
*  important---see KPG1_ASPSY.
         CALL KPG1_ASPSY( 'FORMAT(LABELS)', 'FORMAT(1)', STATUS )
         CALL KPG1_ASPSY( '(LABELS)', '(TEXTLAB)', STATUS )
         CALL KPG1_ASPSY( 'FORMAT(OFF*SET)', 'FORMAT(2)', STATUS )
         CALL KPG1_ASPSY( '(OFF*SET)', '(NUMLAB)', STATUS )
         CALL KPG1_ASPSY( '(TEXT)', '(TITLE)', STATUS )

         CALL KPG1_ASSET( 'KAPPA_MLINPLOT', '+KEYSTYLE', IPLOTK,
     :                    STATUS )

*  Draw the key to the right of the plot and aligned with the top axis.
         CALL KPS1_MLPKY( IPLOTK, NDISP, OFFSET, IGRP, USE, KEYOFF,
     :                    UNITS( : NCU ), STATUS )

*  Report a context message if anything went wrong.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'MLINPLOT_NOKEY', 'MLINPLOT: Error while '//
     :                    'plotting the key.', STATUS )
         END IF

      END IF

*  Shutdown procedure.
*  ===================
 999  CONTINUE

*  Free resources.
      CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Free any memory used.
      IF ( IPW1 .NE. 0 ) CALL PSX_FREE( IPW1, STATUS )
      IF ( IPNOM .NE. 0 ) CALL PSX_FREE( IPNOM, STATUS )
      IF ( IPDAT .NE. 0 ) CALL PSX_FREE( IPDAT, STATUS )
      IF ( IPBAR .NE. 0 ) CALL PSX_FREE( IPBAR, STATUS )

*  Free the group holding curve labels.
      CALL GRP_DELET( IGRP, STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MLINPLOT_ERR6', 'MLINPLOT: Failed to display '//
     :                 'a multi-line plot of a two-dimensional data '//
     :                 'set.',  STATUS )
      END IF

      END
