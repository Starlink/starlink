      SUBROUTINE CONTOUR( STATUS )
*+
*  Name:
*     CONTOUR

*  Purpose:
*     Contours a 2-d NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CONTOUR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application produces a contour map of a two-dimensional NDF
*     on the current graphics device, with single-pixel resolution.
*     Contour levels can be chosen automatically in various ways, or
*     specified explicitly (see Parameter MODE).  In addition, this
*     application can also draw an outline around either the whole data
*      array, or around the good pixels in the data array (set MODE to
*     "Bounds" or "Good").
*
*     The plot is produced within the current graphics database picture,
*     and may be aligned with an existing DATA picture if the existing
*     picture contains suitable co-ordinate Frame information (see
*     Parameter CLEAR).
*
*     The appearance of each contour can be controlled in several ways.
*     The pens used can be rotated automatically (see Parameter PENROT).
*     Contours below a given threshold value can be drawn dashed (see
*     Parameter DASHED).  Alternatively, the appearance of each contour
*     can be set explicitly (see Parameter PENS).
*
*     Annotated axes can be produced (see Parameter AXES), and the
*     appearance of the axes can be controlled in detail (see Parameter
*     STYLE).  The axes show co-ordinates in the current co-ordinate
*     Frame of the supplied NDF.
*
*     A list of the contour levels can be displayed to the right of the
*     contour map (see Parameter KEY).  The appearance and position of
*     this key may be controlled using Parameters KEYSTYLE and KEYPOS.

*  Usage:
*     contour ndf [comp] mode ncont [key] [device]
*       { firstcnt=? stepcnt=?
*       { heights=?
*       { percentiles=?
*       mode

*  ADAM Parameters:
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        contour map, showing the current co-ordinate Frame of the
*        supplied NDF.  The appearance of the axes can be controlled
*        using the STYLE parameter.  If a null (!) value is supplied,
*        then axes will be drawn unless the CLEAR parameter indicates
*        that the graphics device is not being cleared.  [!]
*     CLEAR = _LOGICAL (Read)
*        TRUE if the graphics device is to be cleared before displaying
*        the contour map.  If you want the contour map to be drawn over
*        the top of an existing DATA picture, then set CLEAR to FALSE.
*        The contour map will then be drawn in alignment with the
*        displayed data.  If possible, alignment occurs within the
*        current co-ordinate Frame of the NDF.  If this is not possible,
*        (for instance if suitable WCS information was not stored with
*        the existing DATA picture), then alignment is attempted in
*        PIXEL co-ordinates.  If this is not possible, then alignment is
*        attempted in GRID co-ordinates.  If this is not possible, then
*        alignment is attempted in the first suitable Frame found in the
*        NDF irrespective of its domain.  A message is displayed
*        indicating the domain in which alignment occurred.  If there
*        are no suitable Frames in the NDF then an error is reported.
*        [TRUE]
*     COMP = LITERAL (Read)
*        The NDF component to be contoured.  It may be "Data",
*        "Quality", "Variance", or "Error" (where "Error" is an
*        alternative to "Variance" and causes the square root of the
*        variance values to be displayed).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255).  ["Data"]
*     DASHED = _REAL (Read)
*        The height below which the contours will be drawn with dashed
*        lines (if possible).  A null value (!) results in contours
*        being drawn with the styles specified by Parameters PENS,
*        PENROT, and STYLE.  [!]
*     DEVICE = DEVICE (Read)
*        The plotting device. [current image-display device]
*     FAST = _LOGICAL (Read)
*        If TRUE, then a faster, but in certain cases less-accurate,
*        method is used to draw the contours.  In fast mode, contours
*        may be incorrectly placed on the display if the mapping between
*        graphics co-ordinates and the current co-ordinate Frame of the
*        supplied NDF has any discontinuities, or is strongly
*        non-linear.  This may be the case, for instance, when
*        displaying all-sky maps on top of each other.  [TRUE]
*     FILL = _LOGICAL (Read)
*        The contour plot normally has square pixels, in other words
*        a specified length along each axis corresponds to the same
*        number of pixels.  However, for images with markedly different
*        dimensions this default behaviour may not be suitable or give
*        the clearest plot.  When FILL is TRUE, the square-pixel
*        constraint is relaxed and the contour plot is the largest
*        possible within the current picture.  When FILL is FALSE, the
*        pixels are square.  [FALSE]
*     FIRSTCNT = _REAL (Read)
*        Height of the first contour (Linear and Magnitude modes).
*     HEIGHTS() = _REAL (Read)
*        The required contour levels (Free mode).
*     KEY = _LOGICAL (Read)
*        TRUE if a key of the contour level versus pixel value is to be
*        produced. The appearance of this key can be controlled using
*        Parameter KEYSTYLE, and its position can be controlled using
*        Parameter KEYPOS.  [TRUE]
*     KEYPOS() = _REAL (Read)
*        Two values giving the position of the key.  The first value
*        gives the gap between the right-hand edge of the contour map
*        and the left-hand edge of the key (0.0 for no gap, 1.0 for the
*        largest gap).  The second value gives the vertical position of
*        the top of the key (1.0 for the highest position, 0.0 for the
*        lowest).  If the second value is not given, the top of the key
*        is placed level with the top of the contour map.  Both values
*        should be in the range 0.0 to 1.0.  If a key is produced, then
*        the right-hand margin specified by Parameter MARGIN is ignored.
*        [current value]
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
*        The appearance of the contour indices is controlled by
*        attributes Colour(TextLab), Font(TextLab), etc. (the synonym
*        Index can be used in place of TextLab).  The appearance of the
*        contour values is controlled by attributes Colour(NumLab),
*        Font(NumLab), etc (the synonym Value can be used in place of
*        NumLab).  Contour indices are formatted using attributes
*        Format(1), Digits(1), etc. (the synonym Index can be used in
*        place of value 1).  Contour values are formatted using
*        attributes Format(2), etc. (the synonym Value can be used in
*        place of the value 2).  [current value]
*     LABPOS() = _REAL (Read)
*        Only used if Parameter MODE is set to "Good" or "Bounds".  It
*        specifies the position at which to place a label identifying
*        the input NDF within the plot.  The label is drawn parallel to
*        the first pixel axis.  Two values should be supplied for
*        LABPOS.  The first value specifies the distance in millimetres
*        along the first pixel axis from the centre of the bottom-left
*        pixel to the left edge of the label.  The second value
*        specifies the distance in millimetres along the second pixel
*        axis from the centre of the bottom-left pixel to the baseline
*        of the label.  If a null (!) value is given, no label is
*        produced.  The appearance of the label can be set by using the
*        STYLE parameter (for instance "Size(strings)=2").
*        [current value]
*     LASTCNT = _REAL (Read)
*        Height of the last contour (Linear and Magnitude modes).
*     LENGTH() = _REAL (Write)
*        On exit this holds the total length in pixels of the contours
*        at each selected height.  These values are only computed when
*        Parameter STATS is TRUE.
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave around the contour map for
*        axis annotation.  The widths should be given as fractions of
*        the corresponding dimension of the current picture.  The actual
*        margins used may be increased to preserve the aspect ratio of
*        the DATA picture.  Four values may be given, in the order:
*        bottom, right, top, left.  If fewer than four values are given,
*        extra values are used equal to the first supplied value.  If
*        these margins are too narrow any axis annotation may be
*        clipped.  If a null (!) value is supplied, the value used is
*        0.15 (for all edges) if annotated axes are being produced, and
*        zero otherwise. See also Parameter KEYPOS.  [current value]
*     MODE = LITERAL (Read)
*        The method used to select the contour levels. The options are:
*
*          - "Area" -- The contours enclose areas of the array for which
*          the equivalent radius increases by equal increments.  You
*          specify the number of levels.
*
*          - "Automatic" -- The contour levels are equally spaced
*          between the maximum and minimum pixel values in the array.
*          You supply the number of contour levels.
*
*          - "Bounds" -- A single "contour" is drawn representing the
*          bounds of the input array. A label may also be added (see
*          Parameter LABPOS).
*
*          - "Equalised" -- You define the number of equally spaced
*          percentiles.
*
*          - "Free" -- You specify a series of contour values
*          explicitly.
*
*          - "Good" -- A single "contour" is drawn outlining the good
*          pixel values.  A label may also be added (see Parameter
*          LABPOS).
*
*          - "Linear" -- You define the number of contours, the start
*          contour level and linear step between contours.
*
*          - "Magnitude" -- You define the number of contours, the start
*          contour level and step between contours.  The step size is in
*          magnitudes so the nth contour is dex(-0.4*(n-1)*step) times
*          the start contour level.
*
*          - "Percentiles" -- You specify a series of percentiles.
*
*          - "Scale" --  The contour levels are equally spaced between
*          two pixel values that you specify.  You also supply the
*          number of contour levels, which must be at least two.
*
*        If the contour map is aligned with an existing DATA picture
*        (see Parameter CLEAR), then only part of the supplied NDF may
*        be displayed.  In this case, the choice of contour levels is
*        based on the data within a rectangular section of the input NDF
*        enclosing the existing DATA picture.  Data values outside this
*        section are ignored.
*     NCONT = _INTEGER (Read)
*        The number of contours to draw (only required in certain
*        modes).  It must be between 1 and 50.  If the number is large,
*        the plot may be cluttered and take longer to produce.  The
*        initial suggested default of 6 gives reasonable results.
*     NDF = NDF (Read)
*        NDF structure containing the 2-dimensional image to be
*        contoured.
*     NUMBER() = _INTEGER (Write)
*        On exit this holds the number of closed contours at each
*        selected height.  Contours are not closed if they intersect a
*        bad pixel or the edge of the image.  These values are only
*        computed when Parameter STATS is TRUE.
*     PENROT = _LOGICAL (Read)
*        If TRUE, the plotting pens are cycled through the contours to
*        aid identification of the contour heights.  Only accessed if
*        pen definitions are not supplied using Parameter PENS.  [FALSE]
*     PENS = GROUP (Read)
*        A group of strings, separated by semicolons, each of which
*        specifies the appearance of a pen to be used to draw a contour.
*        The first string in the group describes the pen to use for the
*        first contour, the second string describes the pen for the
*        second contour, etc.  If there are fewer strings than contours,
*        then the supplied pens are cycled through again, starting at
*        the beginning.  Each string should be a comma-separated list of
*        plotting attributes to be used when drawing the contour.  For
*        instance, the string "width=10.0,colour=red,style=2" produces a
*        thick, red, dashed contour.  Attributes that are unspecified in
*        a string default to the values implied by Parameter STYLE.  If
*        a null value (!) is given for PENS, then the pens implied by
*        Parameters PENROT, DASHED and STYLE are used.  [!]
*     PERCENTILES() = _REAL (Read)
*        Contour levels given as percentiles.  The values must lie
*        between 0.0 and 100.0. (Percentiles mode).
*     STATS = _LOGICAL (Read)
*        If TRUE, the LENGTH and NUMBER statistics are computed.
*        [FALSE]
*     STEPCNT = _REAL (Read)
*        Separation between contour levels, linear for Linear mode
*        and in magnitudes for Magnitude mode.
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use for the contours and annotated axes.
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
*        The appearance of the contours is controlled by the attributes
*        Colour(Curves), Width(Curves), etc (the synonym Contours may be
*        used in place of Curves). The contour appearance established in
*        this way may be modified using Parameters PENS, PENROT and
*        DASHED.  [current value]
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the current co-ordinate Frame of
*        the NDF has more than two axes.  A group of two strings should
*        be supplied specifying the two axes which are to be used when
*        annotating and aligning the contour map.  Each axis can be
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
*        supplied.  If a null (!) value is supplied, the axes with the
*        same indices as the two significant NDF pixel axes are used.
*        [!]

*  Examples:
*     contour myfile
*        Contours the data array in the NDF called myfile on the current
*        graphics device.  All other settings are defaulted, so for
*        example the current mode for determining heights is used, and
*        a key is plotted.
*     contour taurus1(100:199,150:269,4)
*        Contours a 2-dimensional section of the three-dimensional NDF
*        called taurus1 on the current graphics device.  The section
*        extends from pixel (100,150,4) to pixel (199,269,4).
*     contour ngc6872 mode=au ncont=5 device=ps_l pens="style=1;style=2"
*        Contours the data array in the NDF called ngc6872 on the ps_l
*        graphics device.  Five equally spaced contours between the
*        maximum and minimum data values are drawn, alternating between
*        line styles 1 and 2 (solid and dashed).
*     contour ndf=ngc6872 mode=au ncont=5 penrot style="^mysty,grid=1"
*        As above except that the current graphics device is used, pens
*        are cycled automatically, and the appearance of the axes is
*        read from text file mysty.  The plotting attribute Grid is set
*        explicitly to 1 to ensure that a co-ordinate grid is drawn over
*        the plot. The text file mysty could, for instance, contain the
*        two lines "Title=NGC6872 at 25 microns" and "grid=0".  The
*        Title setting gives the title to display at the top of the
*        axes.  The Grid setting would normally prevent a co-ordinate
*        grid being drawn, but is overridden in this example by the
*        explicit setting for Grid which follows the file name.
*     contour m51 mode=li firstcnt=10 stepcnt=2 ncont=4 keystyle=^keysty
*        Contours the data array in the NDF called m51 on the
*        current graphics device.  Four contours at heights 10, 12, 14,
*        and 16 are drawn.  A key is plotted using the style specified
*        in the text file keysty.  This file could, for instance,
*        contain the two lines "font=3" and "digits(2)=4" to cause all
*        text in the key to be drawn using PGPLOT font 3 (an italic
*        font), and 4 digits to be used when formatting the contour
*        values.
*     contour ss443 mode=pe percentiles=[80,90,95] stats keypos=0.02
*        Contours the data array in the NDF called ss443 on the current
*        graphics device.  Contours at heights corresponding to the 80,
*        90 and 95 percentiles are drawn.  The key is placed closer
*        to the contour map than usual.  Contour statistics are
*        computed.
*     contour skyflux mode=eq ncont=5 dashed=0 pens='colour=red' noclear
*        Contours the data array in the NDF called skyflux on the
*        current graphics device.  The contour map is automatically
*        aligned with any existing DATA picture, if possible.  Contours
*        at heights corresponding to the 10, 30, 50, 70 and 90
*        percentiles (of the data within the picture) are drawn in red.
*        Those contours whose values are negative will appear as dashed
*        lines.
*     contour comp=d nokey penrot style="grid=1,title=My data" \
*        Contours the data array in the current NDF on
*        the current graphics device using the current method for
*        height selection.  No key is drawn.  The appearance of the
*        contours cycles every third contour. A co-ordinate grid is
*        drawn over the plot, and a title of "My data" is displayed at
*        the top.
*     contour comp=v mode=fr heights=[10,20,40,80] \
*        Contours the variance array in the current NDF on the
*        current graphics device.  Contours at 10, 20, 40 and 80 are
*        drawn.

*  Notes:
*     -  If no Title is specified via the STYLE parameter, then the
*     Title component in the NDF is used as the default title for the
*     annotated axes.  If the NDF does not have a Title component, then
*     the default title is taken from current co-ordinate Frame in the
*     NDF.  If this has not been set explicitly, then the name of the
*     NDF is used as the default title.
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME picture containing the
*     annotated axes, contours, and key; a KEY picture to store
*     the key if present; and a DATA picture containing just the
*     contours.  Note, the FRAME picture is only created if annotated
*     axes or a key has been drawn, or if non-zero margins were
*     specified using Parameter MARGIN.  The world co-ordinates in the
*     DATA picture will be pixel co-ordinates.  A reference to the
*     supplied NDF, together with a copy of the WCS information in the
*     NDF are stored in the DATA picture.  On exit the current database
*     picture for the chosen device reverts to the input picture.

*  Related Applications:
*     KAPPA: WCSFRAME, PICDEF; Figaro: ICONT; SPECDRE: SPECCONT.

*  Implementation Status:
*     -  Only real data can be processed directly.  Other non-complex
*     numeric data types will undergo a type conversion before the
*     contour plot is drawn.
*     -  Bad pixels and automatic quality masking are supported.

*  Implementation Deficiencies:
*     Smooth-contour function is no longer available.

*  Copyright:
*     Copyright (C) 1988-1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997-1999, 2001, 2004 Central Laboratory of
*     the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1988 Aug  1 (MJC):
*        Original version.
*     1989 Feb  1 (MJC):
*        Made defaults for PXSIZE and PYSIZE parameters slightly
*        smaller to accommodate a change to ADAM, where pressing the
*        default could result in a value out of range and a confusing
*        error message.
*     1989 Apr  3 (MJC):
*        Added graphics database V1.0.
*     1989 Apr 14 (MJC):
*        Redefined colours of SGS pens to predefined state if
*        workstation has dynamic colour representation.
*     1989 Jun 23 (MJC):
*        AGI/SGS rounding error bugs fixed so workarounds for fuzzy
*        edges removed; new AGI_ANNUL routine used.
*     1989 Aug  8 (MJC):.
*        Passed array dimensions as separate variables to CNTDRA,
*        CNTDRW and CNTSEL.
*     1989 Aug 29 (MJC):
*        Used NAG routine to produce sub-pixel resolution and annotated
*        contours replacing CNTDRA, removed NOISY parameter as this
*        functionality is not provided in the routine, added LABELFREQ
*        parameter.
*     1989 Oct 17 (MJC):
*        Revised the world co-ordinates of the contour picture to the
*        Starlink standard --- had to create a smaller zone for NAG
*        plotting, but still record the full image zone in the
*        database.
*     1989 Dec 21 (MJC):
*        Workspace managed by AIF_TEMP.
*     1990 Jan 9  (MJC):
*        Corrected SGS status.
*     1990 Feb 18 (MJC):
*        Large blocks of bad pixels removed before contouring.
*     1990 Mar 30 (MJC):
*        Added axis annotation parameters and reports the chosen
*        contour heights to the user.
*     1990 May 22 (MJC):
*        Corrected aspect ratio of the contour region to give the same
*        scale factor in x and y.
*     1990 Aug 29 (MJC):
*        NDF version supporting variance and quality, bad-pixel checks,
*        units and title to annotate the plot.
*     1991 February 7 (MJC):
*        Added NDF reference into the database, AGI context control and
*        removed fuzzy-picture fudge.
*     1991 April 8 (MJC):
*        Added data co-ordinate transformation and optional axes.
*        Re-organised world co-ordinates so that CNTDRW no longer
*        handles the full array, merely the section.
*     1991 May 1 (MJC):
*        Renamed IN parameter to NDF for consistency.
*     1991 July 31 (MJC):
*        No longer redefines colours of SGS pens to predefined state if
*        workstation has dynamic colour representation, now there is
*        palette control.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1991 September 17 (MJC):
*        Moved getting the PENROT and LABELFREQ parameters until after
*        the number of contour heights is known to fix a bug in the
*        permitted range of LABELFREQ.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 4 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 November 28 (MJC):
*        Does not use non-monotonic axis centres.
*     1992 December 17 (MJC):
*        Added the FILL option.
*     1993 January 19 (MJC):
*        Added the BORDER option.
*     1995 October 19 (MJC):
*        Supports Error component.
*     1997 May 28 (MJC):
*        Added percentiles and equalised options for the MODE including
*        a new parameter PERCENTILES.  Added CONCOL and DASHED
*        parameters, and further examples.  Improved efficiency by
*        using PSX to obtain workspace.  Increased the maximum
*        thickness from 5 to 10.  Rewrote the Notes on contour colour
*        and line style.  Obtained the contour heights before line
*        style parameters.
*     1997 May 30 (MJC):
*        Removed NAG library calls and smooth and/or annotated contours
*        are no longer available.  Parameters ANNOTA, LABELFREQ, MAXRES,
*        NOISY, RESOLUTION, and SMOOTHING withdrawn.
*     1997 May 31 (MJC):
*              Added STATS, LENGTH, and NUMBER.
*     12-AUG-1998 (DSB):
*        Major changes to base graphics on PGPLOT and handling of
*        co-ordinate systems on the AST library.
*     26-OCT-1999 (DSB):
*        Margin changed to be a fraction of the current picture instead
*        of the DATA picture.
*     6-FEB-2001 (DSB):
*        Added modes Good and Bounds, and Parameter LABPOS.
*     20-AUG-2001 (DSB):
*        Change default Format (to "%g") and Colour (to the colour used
*        to draw the contour) for the contour indices in the key.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     2006 January 23 (MJC):
*        Added "Scale" mode.
*     27-JAN-2006 (DSB):
*        - Use a runtime default of ".NOT.CLEAR" for Parameter AXIS.
*        - Ignore blank titles supplied in STYLE.
*     6-FEB-2006 (DSB):
*        Use KPG1_ASTTL to get the title.
*     3-APR-2006 (DSB):
*        Removed the warning about loss of precision when contour
*        _DOUBLE data (at the request of TimJ).
*     2006 April 12 (MJC):
*        Remove unused variables and wrapped long lines.
*     23-MAY-2006 (DSB):
*        - Take account of ROI Frames in the WCS FrameSet.
*        - Ensure all variable comments are no longer than 1 line.
*     30-MAY-2006 (DSB):
*        Correct logic of IF-THEN-ELSE block controlling drawing of
*        axes.
*     2006 June 1 (MJC):
*        Use KPS1_DISTL to draw title in correct place when ROI Frames
*        are present.
*     2-JUN-2006 (DSB):
*        Modified to use ATL_PLROI.
*     2010 October 13 (MJC):
*        Permit temporary style attributes.
*     10-FEB-2015 (DSB):
*        Since 20-OCT-2009, setting the Ident attribute for a FrameSet
*        changes the Ident for the FrameSet itself, not for the current
*        Frame. So now we need to extract a pointer to the current Frame
*        before setting the Ident attribute.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE constants
      INCLUDE 'PRM_PAR'        ! VAL constants
      INCLUDE 'NDF_PAR'        ! NDF constants
      INCLUDE 'NDF_ERR'        ! NDF error constants
      INCLUDE 'PAR_ERR'        ! PAR error constants
      INCLUDE 'AST_PAR'        ! AST constants
      INCLUDE 'GRP_PAR'        ! GRP constants
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER CUNITS             ! Maximum number of visible characters
      PARAMETER( CUNITS = 14 )   ! in units

      REAL KW                    ! Width of KEY picture as a fraction of
      PARAMETER( KW = 0.15 )     ! current picture width

      INTEGER NDIM               ! Dimensionality of input array
      PARAMETER( NDIM = 2 )

      INTEGER MXCONT             ! Maximum number of contour heights
      PARAMETER( MXCONT = 50 )

*  Local Variables:
      CHARACTER COMP*8          ! Component to be displayed
      CHARACTER DTYPE*(NDF__SZFTP)! Type of the image after processing
      CHARACTER IDENT*20       ! Ident attribute for a Frame
      CHARACTER ITYPE*(NDF__SZTYP)! Processing type of the image
      CHARACTER MCOMP*8         ! Component to be mapped
      CHARACTER MODE*20         ! Method for selecting contour heights
      CHARACTER NDFNAM*255      ! Full NDF specification
      CHARACTER UNITS*(CUNITS + 5)! Units of the data
      DOUBLE PRECISION BOX( 4 ) ! Bounds of image in pixel co-ordinates
      DOUBLE PRECISION POS( 2 ) ! Label reference position
      DOUBLE PRECISION XP( 2 )  ! Label text positions
      DOUBLE PRECISION YP( 2 )  ! Label test positions
      INTEGER CFRM              ! Pointer to current Frame
      INTEGER CNTCLS( MXCONT )  ! No. of closed contours at each height
      INTEGER CNTPEN( MXCONT )  ! Pen index used to draw each contour
      INTEGER DIMS( NDIM )      ! Dimensions of input array
      INTEGER EL                ! Number of elements in the input array
      INTEGER I                 ! General variable
      INTEGER ICURR             ! Index of Current Frame
      INTEGER IGRID             ! Index of GRID Frame in WCS FrameSet
      INTEGER IGRP              ! GRP id. for pen definitions group
      INTEGER INDF              ! NDF id. for input NDF
      INTEGER INDFS             ! NDF id. for visible NDF section
      INTEGER IPICD             ! AGI id. for DATA picture
      INTEGER IPICF             ! AGI id. for new FRAME picture
      INTEGER IPICK             ! AGI id. for the KEY picture
      INTEGER IPLOT             ! Pointer to AST Plot for DATA picture
      INTEGER IPLOTK            ! Pointer to AST Plot for KEY picture
      INTEGER IPLOTR            ! Pointer to AST Plot for a ROI
      INTEGER IWCS              ! Pointer to the WCS FrameSet from NDF
      INTEGER NC                ! Number of characters in NDFNAM
      INTEGER NCONT             ! Number of contour heights
      INTEGER NCU               ! Number of characters in the units
      INTEGER NFRM              ! Frame index increment
      INTEGER NKP               ! No. of values supplied for KEYPOS
      INTEGER NMARG             ! No. of margin values given
      INTEGER IREG              ! Region index
      INTEGER NVAL              ! No. of parameter values supplied
      INTEGER PNTR              ! Pointer to array data
      INTEGER RPLOTS            ! KeyMap holding ROI plots
      INTEGER SDIM( NDIM )      ! The significant NDF axes
      INTEGER SLBND( NDIM )     ! Significant lower bounds of the image
      INTEGER SUBND( NDIM )     ! Significant upper bounds of the image
      INTEGER WKPNTR            ! Pointer to workspace
      LOGICAL ALIGN             ! DATA pic aligned with a previous pic?
      LOGICAL AXES              ! Annotated axes are to be drawn?
      LOGICAL BAD               ! Bad pixels are present in the image?
      LOGICAL CLEAR             ! Clear screen before plotting?
      LOGICAL CNTUSD( MXCONT )  ! Contour plotted at height in CNTLEV?
      LOGICAL FAST              ! Draw contours quickly?
      LOGICAL KEY               ! Make a key of the contour heights?
      LOGICAL STATS             ! Contour statistics required?
      REAL AREA( MXCONT )       ! Work array for areas in CNTSEL
      REAL ASPECT               ! Aspect ratio of the input array
      REAL CNTLEN( MXCONT )     ! Length of contours at each height
      REAL CNTLEV( MXCONT )     ! Contour heights
      REAL DUMMY                ! Un-required argument value
      REAL GSZ                  ! Size of 1 grid pixel in millimetres
      REAL KEYOFF               ! Offset to top of key
      REAL KEYPOS( 2 )          ! Key position
      REAL LABPOS( 2 )          ! Position for outline labels
      REAL MARGIN( 4 )          ! Width of margins round DATA picture
      REAL PERCNT( MXCONT )     ! Contour heights as percentiles
      REAL UP( 2 )              ! Label up-vector
      REAL Y1,Y2                ! Vertical bounds of PGPLOT viewport
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF to be contoured and get its WCS information.
*  ===========================================================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the identifier of the NDF to be contoured.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  Find which component to contour.
      CALL KPG1_ARCOG( 'COMP', INDF, MCOMP, COMP, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  This application can only process real components directly.
*  Therefore for the given type of the image find in which type
*  it should be processed.  Currently, it is obvious since only
*  one type is supported, but this acts as a placeholder when this
*  is no longer true.  It may still be possible to handle d.p.
*  data provided the dynamic range is not too small.
      CALL NDF_MTYPE( '_REAL', INDF, INDF, COMP, ITYPE, DTYPE, STATUS )
      IF ( STATUS .EQ. NDF__TYPNI ) THEN
         CALL ERR_ANNUL( STATUS )
         ITYPE = '_REAL'
      END IF

*  Get an AST pointer to a FrameSet describing the co-ordinate Frames
*  present in the NDF's WCS component.  Modify it to ensure that the
*  Base, PIXEL and Current frames all have two dimensions.  The NDF
*  must have exactly two significant dimensions (i.e. axes spanning
*  more than one pixel).
      CALL KPG1_ASGET( INDF, NDIM, .TRUE., .TRUE., .TRUE., SDIM, SLBND,
     :                 SUBND, IWCS, STATUS )

*  Store the index of the GRID Frame (i.e. the Base Frame).  We need
*  this so that we can easily find the GRID Frame in the AST Plot used
*  to do the graphics.
      IGRID = AST_GETI( IWCS, 'BASE', STATUS )

*  Obtain the units if present.  A null units field does not
*  cause a blank line to appear in the key.  Quality has no units.
      CALL KPG1_DAUNI( INDF, MCOMP, UNITS, NCU, STATUS )

*  Get some other parameter values.
*  =================================
      CALL PAR_GET0L( 'STATS', STATS, STATUS )
      CALL PAR_GET0L( 'KEY', KEY, STATUS )
      CALL PAR_GET0L( 'FAST', FAST, STATUS )
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0L( 'AXES', AXES, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AXES = CLEAR
         END IF
      END IF

*  Start the graphics system.
*  ==========================

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set the dynamic default for MARGIN.
      IF ( AXES ) THEN
         MARGIN( 1 ) = 0.15
      ELSE
         MARGIN( 1 ) = 0.0
      END IF

      CALL PAR_DEF1R( 'MARGIN', 1, MARGIN( 1 ), STATUS )

*  Get new values.
      CALL PAR_GDRVR( 'MARGIN', 4, -0.49, 10.0, MARGIN, NMARG, STATUS )

*  Use the default if a null value was supplied.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NMARG = 1
      END IF

*  Ignore any suplus values.
      NMARG = MIN( 4, NMARG )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Use the first value for any unspecified edges.
      DO I = NMARG + 1, 4
         MARGIN( I ) = MARGIN( 1 )
      END DO

*  Store the pixel co-ordinates bounds for the new DATA picture.  These
*  are only used if the new DATA picture is not based on an existing
*  DATA picture.  Note, the corresponding PGPLOT window created by
*  KPG1_PLOT will have world co-ordinates of millimetres from the
*  bottom-left corner of the view surface, NOT pixels.  This box is only
*  used to define the bounds of the picture within the AGI database for
*  the benefit of non-AST applications.
      BOX( 1 ) = DBLE( SLBND( 1 ) ) - 1.0D0
      BOX( 2 ) = DBLE( SLBND( 2 ) ) - 1.0D0
      BOX( 3 ) = DBLE( SUBND( 1 ) )
      BOX( 4 ) = DBLE( SUBND( 2 ) )

*  Store the aspect ratio of the data array, assuming square pixels.
      ASPECT = ( BOX( 4 ) - BOX( 2 ) )/( BOX( 3 ) - BOX( 1 ) )

*  Generate a reference for the NDF to be stored in the graphics
*  database.
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_LOAD( ' ', '^NDF', NDFNAM, NC, STATUS )

*  Establish synonyms for AST graphical element names to be recognised
*  during the following call to KPG1_PLOT.
      CALL KPG1_ASPSY( '(CON*TOURS)', '(CURVES)', STATUS )

*  Start up the graphics system.  This stores a new DATA picture in the
*  AGI database with the given pixel co-ordinate bounds (a KEY picture
*  is also created if necessary, together with an enclosing FRAME
*  picture ). The PGPLOT viewport is set so that it matches the area of
*  the DATA picture.  World co-ordinates within the PGPLOT window are
*  set to millimetres from the bottom-left corner of the view surface.
*  An AST Plot is returned for drawing in the DATA picture.  The Base
*  (GRAPHICS) Frame in the Plot corresponds to millimetres from the
*  bottom-left corner of the viewport, and the Current Frame is
*  inherited from the NDF's WCS FrameSet.   First deal with cases
*  where a key is required...
      IF ( KEY ) THEN

*  Get the position required for the key.  The margin between DATA and
*  KEY Frames is determined by the horizontal position requested for
*  the key.
         CALL PAR_GDRVR( 'KEYPOS', 2, -1.0, 1.0, KEYPOS, NKP, STATUS )
         IF ( KEYPOS( 1 ) .GE. 0.0 ) THEN
            MARGIN( 2 ) = KEYPOS( 1 )
         ELSE
            MARGIN( 2 ) = KEYPOS( 1 ) - KW
         END IF

*  Start up the graphics system, creating a KEY picture.
         CALL KPG1_PLOT( IWCS, 'UNKNOWN', 'KAPPA_CONTOUR',
     :                   NDFNAM( : NC ), MARGIN, 1, 'KEY', 'R', KW,
     :                   ASPECT, 'PIXEL', BOX, IPICD, IPICF, IPICK,
     :                   IPLOT, NFRM, ALIGN, STATUS )

*  Otherwise, start up the graphics system, creating no KEY picture.
      ELSE
         CALL KPG1_PLOT( IWCS, 'UNKNOWN', 'KAPPA_CONTOUR',
     :                   NDFNAM( : NC ), MARGIN, 0, ' ', ' ', 0.0,
     :                   ASPECT, 'PIXEL', BOX, IPICD, IPICF, IPICK,
     :                   IPLOT, NFRM, ALIGN, STATUS )
      END IF


*  Ensure the Title attribute of the Plot has a useful value.
      CALL KPG1_ASTTL( IPLOT, IWCS, INDF, STATUS )

*  Obtain sorted contour heights.
*  ==============================
*  If the new DATA picture has been aligned with an existing DATA
*  picture it is possible that only a small part of the supplied NDF
*  will be visible.  We want to base the selection of contour levels on
*  the visible data, rather than the entire NDF.  We therefore obtain
*  an NDF section spanning just the visible data.  The GRID Frame in the
*  Plot is re-mapped so that it refers to GRID co-ordinates in the NDF
*  section.
      CALL KPS1_CNTSC( INDF, IPLOT, IGRID + NFRM, SDIM, SLBND, SUBND,
     :                 INDFS, STATUS )

*  Map the section.
      CALL NDF_MAP( INDFS, MCOMP, ITYPE, 'READ', PNTR, EL, STATUS )

*  Check whether or not bad pixels are present.
      CALL NDF_BAD( INDFS, COMP, .FALSE., BAD, STATUS )

*  Select the method of defining contour heights and evaluate them.
      CALL KPS1_CNSER( 'MODE', 'NCONT', 'FIRSTCNT', 'LASTCNT',
     :                 'STEPCNT', 'HEIGHTS', 'PERCENTILES', BAD, EL,
     :                 %VAL( CNF_PVAL( PNTR ) ), MXCONT, CNTLEV, PERCNT,
     :                 AREA, NCONT, MODE, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Sort the contour heights into increasing order.
      CALL KPG1_QSRTR( NCONT, 1, NCONT, CNTLEV, STATUS )

*  Add a context message if anything went wrong while sorting.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CONTOUR_GTLEV', 'CONTOUR: Error sorting the '//
     :                 'contour levels', STATUS )
         GO TO 999
      END IF

*  Get the drawing attributes to use for each contour.
      CALL KPS1_CNTPN( 'PENS', 'PENROT', 'DASHED', IPLOT, NCONT, CNTLEV,
     :                  IGRP, STATUS )

*  Produce the plot.
*  =================

*  Store the size of each significant dimension in the displayed NDF
*  section.
      DIMS( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
      DIMS( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1

*  Create a scratch area in which to log pixels contoured.
      CALL PSX_CALLOC( DIMS( 1 )*DIMS( 2 ), '_LOGICAL', WKPNTR, STATUS )

*  Contours are drawn by KPS1_CNTDR in GRID co-ordinates.  Therefore,
*  make the GRID Frame from the supplied NDF the Current Frame in the
*  Plot.  Note the index of the Current Frame first.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )
      CALL AST_SETI( IPLOT, 'CURRENT', IGRID + NFRM, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Draw a data outline if required.
      IF ( MODE .EQ. 'BOUNDS' .OR. MODE .EQ. 'GOOD' ) THEN
         CALL KPS1_CNTGD( (MODE .EQ. 'BOUNDS'), IPLOT, IGRP, DIMS( 1 ),
     :                    DIMS( 2 ), %VAL( CNF_PVAL( PNTR ) ),
     :                    1, 1, DIMS( 1 ),
     :                    DIMS( 2 ), FAST, CNTUSD, CNTLEN, CNTCLS,
     :                    STATUS )

*  Otherwise, draw a contour plot.
      ELSE
         CALL KPS1_CNTDR( IPLOT, IGRP, DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( PNTR ) ),
     :                    1, 1, DIMS( 1 ), DIMS( 2 ),
     :                    NCONT, CNTLEV, STATS, FAST,
     :                    %VAL( CNF_PVAL( WKPNTR ) ),
     :                    CNTUSD, CNTLEN, CNTCLS, CNTPEN, STATUS )
      END IF

*  Add a context message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CONTOUR_SCA', 'CONTOUR: Error contouring the '//
     :                 'array.', STATUS )
      END IF

*  Write the statistics to output parameters.
      IF ( STATS ) THEN
         CALL PAR_PUT1I( 'NUMBER', NCONT, CNTCLS, STATUS )
         CALL PAR_PUT1R( 'LENGTH', NCONT, CNTLEN, STATUS )
      END IF

*  If we are using Bounds or Good mode, see where the label is to be
*  drawn.
      IF ( ( MODE .EQ. 'BOUNDS' .OR. MODE .EQ. 'GOOD' ) .AND.
     :     STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET1R( 'LABPOS', 2, LABPOS, NVAL, STATUS )

*  If a null value was given, annul the error.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  Otherwise draw the label.
         ELSE

*  Duplicate the supplied value if only one value was supplied.
            IF ( NVAL .EQ. 1 ) LABPOS( 2 ) = LABPOS( 1 )

*  We want the up-vector for the label to be parallel to the Y pixel
*  axis, so transform two positions from GRID to GRAPHICS to find the
*  the corresponding up-vector in graphics co-ordinates.
            XP( 1 ) = 1.0D0
            YP( 1 ) = 1.0D0
            XP( 2 ) = XP( 1 )
            YP( 2 ) = YP( 1 ) + 1.0D0
            CALL AST_TRAN2( IPLOT, 2, XP, YP, .FALSE., XP, YP, STATUS )

*  Check these are good.
            IF ( XP( 1 ) .NE. AST__BAD .AND. YP( 1 ) .NE. AST__BAD .AND.
     :           XP( 2 ) .NE. AST__BAD .AND. YP( 2 ) .NE. AST__BAD )THEN

*  Form the up-vector. Negate it if the text would be upside-down.
               UP( 1 ) = XP( 2 ) - XP( 1 )
               UP( 2 ) = YP( 2 ) - YP( 1 )
               IF ( UP( 2 ) .LT. 0 ) THEN
                  UP( 1 ) = -UP( 1 )
                  UP( 2 ) = -UP( 2 )
               END IF

*  Find the size in millimetres of 1 GRID pixel.
               GSZ = SQRT( UP( 1 )**2 + UP( 2 )**2 )

*  Find the GRID position at which to place the bottom-left corner of
*  the text.
               IF ( GSZ .NE. 0.0 ) THEN
                  POS( 1 ) = LABPOS( 1 )/GSZ
                  POS( 2 ) = LABPOS( 2 )/GSZ
               ELSE
                  POS( 1 ) = 1.0
                  POS( 2 ) = 1.0
               END IF

*  Choose the colour for the text.
               IF ( IGRP .NE. GRP__NOID ) THEN
                  CALL KPS1_CNTST( IPLOT, IGRP, 1, STATUS )
               ELSE IF ( .NOT. AST_TEST( IPLOT, 'COLOUR(STRINGS)',
     :                               STATUS ) ) THEN
                  CALL AST_SETI( IPLOT, 'COLOUR(STRINGS)',
     :                     AST_GETI( IPLOT, 'COLOUR(CURVES)', STATUS ),
     :                     STATUS )
               END IF

*  Get the NDF basename and write it out.
               CALL KPG1_NDFNM( INDF, NDFNAM, NC, STATUS )
               CALL AST_TEXT( IPLOT, NDFNAM( : NC ), POS, UP, 'BL',
     :                     STATUS )

            END IF

         END IF

      END IF

*  Re-instate the original Current Frame and draw the axes if required.
      CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )
      IF ( AXES ) THEN

*  See if the WCS FrameSet in the supplied NDF defines any "Regions of
*  interest". If so, each such region is given a separate set of
*  annotated axes. We can do this test by looking at the Ident attribute
*  of the current Frame since KPG1_ASGET will have set this to something
*  begining with "ROI" if any regions of interest were found within the
*  WCS FrameSet.
         CFRM = AST_GETFRAME( IPLOT, AST__CURRENT, STATUS )
         IDENT = AST_GETC( CFRM, 'Ident', STATUS )
         CALL AST_ANNUL( CFRM, STATUS )
         IF( IDENT( : 3 ) .EQ. 'ROI' ) THEN

*  Get an AST KeyMap holding Plots covering the area of each ROI.
            CALL ATL_PLROI( IPLOT, RPLOTS, STATUS )

*  Get the number of entries in the KeyMap and loop round them all.
            DO IREG = 1, AST_MAPSIZE( RPLOTS, STATUS )

*  Get the key with index IREG, and get a pointer to the Plot stored in
*  the KeyMap with that key. Only proceed if the key is found (which is
*  will be).
               IF( AST_MAPGET0A( RPLOTS, AST_MAPKEY( RPLOTS, IREG,
     :                                               STATUS ),
     :                           IPLOTR, STATUS ) ) THEN

*  Draw the Annotated axes, and annul the Plot pointer.
                  CALL KPG1_ASGRD( IPLOTR, IPICF, .TRUE., STATUS )
                  CALL AST_ANNUL( IPLOTR, STATUS )

               END IF
            END DO

*  If there are no ROI Frames, just draw the grid over the whole Plot.
         ELSE
            CALL KPG1_ASGRD( IPLOT, IPICF, .TRUE., STATUS )
         END IF
      END IF

*  Plot the key if necessary (no key is needed if no real contours have
*  been drawn).
      IF ( KEY .AND. MODE .NE. 'GOOD' .AND. MODE .NE. 'BOUNDS' ) THEN

*  If no value was supplied for the vertical position of the KEY using
*  Parameter KEYPOS, find the value which puts the top of the key level
*  with the top of the DATA picture.
         IF ( NKP .LT. 2 ) THEN

*  Report an error if there is insufficient room within the current
*  picture for the key.
            IF ( IPICK .EQ. -1 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CONTOUR_KEY', 'There is insufficient '//
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

*  Cancel attributes set for the Current Frame in the key Plot.  These
*  will be appropriate to AGI world co-ordinates and are not useful.
*  Clearing these attributes will allow KPS1_CNTKY to use its own
*  more-appropriate values.
         CALL AST_CLEAR( IPLOTK, 'TITLE', STATUS )
         CALL AST_CLEAR( IPLOTK, 'FORMAT(1)', STATUS )
         CALL AST_CLEAR( IPLOTK, 'FORMAT(2)', STATUS )

*  Ensure that any previous synonyms for AST attributes are cleared.
         CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Establish some synonyms for AST attribute names to be used when
*  setting the plotting style for the key.  Note, the order is
*  important---see KPG1_ASPSY.
         CALL KPG1_ASPSY( 'FORMAT(IND*EX)', 'FORMAT(1)', STATUS )
         CALL KPG1_ASPSY( '(IND*EX)', '(TEXTLAB)', STATUS )
         CALL KPG1_ASPSY( 'FORMAT(VAL*UE)', 'FORMAT(2)', STATUS )
         CALL KPG1_ASPSY( '(VAL*UE)', '(NUMLAB)', STATUS )
         CALL KPG1_ASPSY( '(TEXT)', '(TITLE)', STATUS )

*  Set the style for plotting in the key picture.  The plus sign
*  requests support of temporary attributes.
         CALL KPG1_ASSET( 'KAPPA_CONTOUR', '+KEYSTYLE', IPLOTK, STATUS )

*  Draw the key to the right of the contour plot and aligned with
*  the top axis.
         CALL KPS1_CNTKY( IPLOTK, NCONT, CNTLEV, CNTUSD, KEYOFF, UNITS,
     :                    CNTPEN, STATUS )

*  Report a context message if anything went wrong.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'CONTOUR_NOKEY', 'CONTOUR: Error while '//
     :                    'plotting the key.', STATUS )
         END IF

      END IF

*  Report the heights actually used as they are not always clearly
*  visible on small plots and/or on low-resolution workstations.
      IF ( MODE .NE. 'GOOD' .AND. MODE .NE. 'BOUNDS' ) THEN
         CALL CNTHLT( NCONT, CNTLEV, CNTUSD, STATUS )
      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  Free resources used to hold attribute synonyms.
      CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Delete any group holding pen definitions.
      IF ( IGRP .NE. GRP__NOID ) CALL GRP_DELET( IGRP, STATUS )

*  Free workspace.
      CALL PSX_FREE( WKPNTR, STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Add a context report if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CONTOUR_ERR', 'CONTOUR: Failed to contour a '//
     :                 '2-dimensional data set.', STATUS )
      END IF

      END
