#if !defined( PLOT_INCLUDED ) /* Include this file only once */
#define PLOT_INCLUDED
/*
*+
*  Name:
*     plot.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Plot class.

*  Invocation:
*     #include "plot.h"

*  Description:
*     This include file defines the interface to the Plot class and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this class.
*
*     The Plot class provides facilities for producing graphical information
*     describing positions within coordinate systems. These include the
*     creation of annotated coordinate axes, the plotting of markers at given
*     physical positions, etc.

*  Inheritance:
*     The Plot class inherits from the FrameSet class.

*  Attributes Over-Ridden:

*  New Attributes Defined:
*     Border (int)
c        This attribute controls whether or not function astGrid should draw
f        This attribute controls whether or not routine AST_GRID should draw
*        a border around regions of valid physical coordinates. If the 
*        value is non-zero (the default), then the border will be drawn.
*        Otherwise, the border is not drawn (axis labels and tick marks will
*        still be drawn unless the relevant attributes indicate that they
*        should not be drawn).
*     ClipOp (int)
*        This attribute, if non-zero, indicates that the clipping specified by 
c        function astClip is to be performed using a logical OR operation 
f        routine AST_CLIP is to be performed using a logical OR operation 
*        between the axes. Otherwise a logical AND operation is used. An OR 
*        operation results in points being retained if one or more axis 
*        values satisfy the clipping bounds. An AND operation results in 
*        points being retained only if all axis values satisfy the clipping 
*        bounds. The default is zero (i.e. use a logical AND). A likely 
*        situation in which this default would need to be over-ridden is if an 
*        area of the plot is to be left clear (to act as a background for some 
*        text for instance). In this case, the bounds of the area should be
c        supplied in reversed order to function astClip, and ClipOp should
f        supplied in reversed order to routine AST_CLIP, and ClipOp should
*        be set to a non-zero value.
*     Colour(label) (int)
*        This attribute has a value for each of the types of graphical items
*        listed in the class prologue. For each type, it gives the colour
*        index to use when drawing graphical items of that type. For 
*        instance, "colour(title)=2" causes the plot title to be drawn 
*        with colour 2. The number of available colours and their appearance
*        is determined by the underlying graphics package. The default 
*        behaviour for all types is to retain the current colour index. If no 
*        type is specified, (i.e. "Colour" instead of "Colour(Title)", etc ), 
c        astSet sets the attribute for all types to the supplied  value, 
c        astClear clears the attributes for all types, astTest tests the 
c        attribute Colour(TextLab), and astGet gets the value of attribute
f        AST_SET sets the attribute for all types to the supplied  value, 
f        AST_CLEAR clears the attributes for all types, AST_TEST tests the 
f        attribute Colour(TextLab), and AST_GET gets the value of attribute
*        Colour(TextLab).
*     DrawAxes (int)
c        This attribute, if non-zero, indicates that function astGrid should
f        This attribute, if non-zero, indicates that routine AST_GRID should
*        draw the grid lines on which ticks marks would be placed. Otherwise, 
*        they will only be drawn if they correspond to a major axis value, and
*        if attribute Grid is non-zero. The default is to draw these grid
*        lines. This attribute is ignored if tick marks would be drawn around the 
*        edges of the grid (see Labelling).
*     DrawTitle (int)
*        This attribute, if non-zero, indicates that the title from
*        the current Frame of the Plot should be displayed at the top of
c        the annotated grid drawn by function astGrid. Otherwise, no title is
f        the annotated grid drawn by routine AST_GRID. Otherwise, no title is
*        displayed. The vertical placing of the title can be controlled
*        using TitleGap. The default is to display the title.
*     Edge(axis) (int)
*        This attribute has a value for each physical axis of the grid drawn
c        by function astGrid. For each axis, it gives the edge of the plotting
f        by routine AST_GRID. For each axis, it gives the edge of the plotting
*        area on which to place labels. Edge 0 is the left hand edge. Edge 1 
*        is the top edge. Edge 2 is the right-hand edge. Edge 3 is the bottom 
*        edge. The default is 3 (the bottom edge) for axis 1, and 0 (the 
*        left-hand edge) for axis 2. If numerical labels would be drawn along 
*        a grid lines instead of around the edges of the grid (see Labelling), 
*        then this attribute is used only to specify the edges for the 
*        descriptive labels.
*     Escape (int)
*        This attribute controls the appearance of text strings and
c        numerical labels drawn by the astGrid and astText functions,
f        numerical labels drawn by the AST_GRID and AST_TEXT functions,
*        by determining if any escape sequences contained within the strings
*        should be used to control the appearance of the text, or should
*        be printed literally. If the Escape value of a Plot is one (the 
*        default), then escape sequences are printed literally rather than 
*        producing the usual effects.
*     Font(label) (int)
*        This attribute has a value for each of the types of graphical items
*        listed in the class prologue. For each item type, it gives the 
*        character font to use when drawing graphical items of that type.
*        For instance, "font(title)=2" causes the plot title to be 
*        drawn with font 2. The number of available fonts and their 
*        appearance is determined by the underlying graphics package. The 
*        default behaviour for all types is to retain the current font. If 
*        no type is specified (i.e. "Font" instead of "Font(Title)", etc ), 
c        astSet sets the attribute for all types to the supplied  value, 
c        astClear clears the attributes for all types, astTest tests the 
c        attribute Font(TextLab), and astGet gets the value of attribute
f        AST_SET sets the attribute for all types to the supplied  value, 
f        AST_CLEAR clears the attributes for all types, AST_TEST tests the 
f        attribute Font(TextLab), and AST_GET gets the value of attribute
*        Font(TextLab).
*     Gap(axis) (double)
*        This attribute has a value for each physical axis of the grid drawn
c        by function astGrid. For each axis, it gives the gap between major
f        by routine AST_GRID. For each axis, it gives the gap between major
*        axis values. The supplied value will be rounded to the nearest "nice" 
*        value. If a value of AST__BAD is supplied (the default), then the 
*        Plot class will choose suitable values itself. Note, the values 
*        supplied for Gap should refer to the internal representation of the
*        axis. For instance, with an RA/DEC grid the values should be
*        supplied in radians, not hours or degrees. If no axis is specified
c        (i.e. "Gap" instead of "Gap(1)" or "Gap(2)") astSet sets the 
c        attribute for both axes to the supplied value, astClear clears the 
c        attributes for both axes, astTest tests the attribute for axis 1, and 
c        astGet gets the value for axis 1.
f        (i.e. "Gap" instead of "Gap(1)" or "Gap(2)") AST_SET sets the 
f        attribute for both axes to the supplied value, AST_CLEAR clears the 
f        attributes for both axes, AST_TEST tests the attribute for axis 1, 
f        and AST_GET gets the value for axis 1.
*     Grf (int)
*        This attribute, if non-zero, indicates that the graphics
*        functions registered using astGrfSet are to be used to draw 
*        graphics. Otherwise, the graphics interface selected at
*        link-time (using ast_link options) is used.
*     Grid (int)
c        This attribute controls whether or not function astGrid should draw
f        This attribute controls whether or not routine AST_GRID should draw
*        a grid of curves across the plotting area to mark the major values on 
*        each axis. If the attribute value is non-zero then such 
*        curves are drawn. Otherwise, short tick marks are used to mark the
*        major axis values. The default is to draw tick marks if the entire 
*        plotting area is filled by valid physical coordinates, and to draw 
*        curves otherwise.
*     Invisible (int)
*        This attribute controls the appearance of all graphics produced by
*        Plot methods by determining whether graphics should be visible or
*        invisible. If the Invisible value of a Plot is non-zero, then all 
*        the Plot methods which normally generate graphical output do not 
*        do so (you can think of them drawing with "invisible ink"). Such 
*        methods do, however, continue to do all the calculations which 
*        would be needed to produce the graphics. In particular, the bounding 
*        box encoding the graphics is still calculated and can be retrieved 
*        as normal using astBoundingBox. The default value is zero, resulting 
*        in all methods drawing graphics as normal, using visible ink.
*     LabelAt(axis) (double)
*        This attribute has a value for each physical axis of the grid drawn
c        by function astGrid. For each axis, it gives the value on the other
f        by routine AST_GRID. For each axis, it gives the value on the other
*        axis at which numerical labels and tick marks will be placed. For 
*        instance, in an all-sky RA/DEC grid, LabelAt(1) gives the DEC value 
*        at which the numerical RA labels will be placed, and LabelAt(2) gives
*        the RA value at which the numerical DEC labels will be placed. If a 
*        value of AST__BAD is supplied (the default), then the Plot class will 
*        choose where to place the labels. Note, the values supplied for 
*        LabelAt should refer to the internal representation of the axis. For 
*        instance, with an RA/DEC grid the values should be supplied in 
*        radians, not hours or degrees. This attribute is ignored if tick 
*        marks would be drawn around the edges of the grid (see Labelling).
*     NumLabGap(axis) (double)
*        This attribute has a value for each physical axis of the grid drawn
c        by function astGrid. For each axis, it gives the space between the
f        by routine AST_GRID. For each axis, it gives the space between the
*        numerical labels and the corresponding grid line or plotting area 
*        edge, as a fraction of the minimum dimension of the plotting area. 
*        Negative values cause the labels to be placed on the same side of 
*        the axis as the tick marks. Otherwise, they are on the side away 
*        from the tick marks. A default value of +0.007 will be used if no 
*        value has been set. If no axis is specified (i.e. "NumLabGap" instead
c        of "NumLabGap(1)" or "NumLabGap(2)") astSet sets the attribute for 
c        both axes to the supplied value, astClear clears the attributes for 
c        both axes, astTest tests the attribute for axis 1, and astGet gets 
c        the value for axis 1.
f        of "NumLabGap(1)" or "NumLabGap(2)") AST_SET sets the attribute for 
f        both axes to the supplied value, AST_CLEAR clears the attributes for 
f        both axes, AST_TEST tests the attribute for axis 1, and AST_GET gets 
f        the value for axis 1.
*     Labelling (int)
*        This attribute controls the placing of numerical axis values and 
*        tick marks. If it is zero, then they are placed around the edges 
*        of the plotting area if possible. If this is not possible, or if
*        the attribute value is non-zero, they are placed along specified 
*        grid lines through the interior of the plot. The LabetAt
*        attribute is used to specify which grid lines to use for this
*        purpose. The default is to produce edge labels if possible.
*     LabelUnits(axis) (int)
*        This attribute has a value for each physical axis of the grid drawn
c        by function astGrid. For each axis, if it is non-zero, the textual
f        by routine AST_GRID. For each axis, if it is non-zero, the textual
*        label describing the axis will include a description of the units 
*        on the axis. The default is to include the units, unless the current
*        frame of the Plot is a SkyFrame, in which case the default is not
*        to include the units. If no axis is specified (i.e. "LabelUnits" 
c        instead of "LabelUnits(1)" or "LabelUnits(2)") astSet sets the
c        attribute for both axes to the supplied value, astClear clears the
c        attributes for both axes, astTest tests the attribute for axis 1,
c        and astGet gets the value for axis 1.
f        instead of "LabelUnits(1)" or "LabelUnits(2)") AST_SET sets the
f        attribute for both axes to the supplied value, AST_CLEAR clears the
f        attributes for both axes, AST_TEST tests the attribute for axis 1,
f        and AST_GET gets the value for axis 1.
*     LabelUp (int)
*        This attribute has a value for each physical axis of the grid drawn
c        by function astGrid. For each axis, if it is non-zero, the numerical
f        by routine AST_GRID. For each axis, if it is non-zero, the numerical
*        labels are drawn upright. Otherwise, they rotate to follow the axis 
*        they are labelling. The default is to allow labels to rotate. 
*        This attribute is ignored if tick marks would be drawn around the 
*        edges of the grid (see Labelling).
*        If no axis is
c        specified astSet sets the attribute for both axes to the supplied
c        value, astClear clears the attributes for both axes, astTest
c        tests the attribute for axis 1, and astGet gets the value for
f        specified AST_SET sets the attribute for both axes to the supplied
f        value, AST_CLEAR clears the attributes for both axes, AST_TEST
f        tests the attribute for axis 1, and AST_GET gets the value for
*        axis 1.
*     LogPlot (int)
*        This attribute controls the appearance of all graphics produced
*	 by the Plot, by determining whether the axes of the plotting
*	 surface are mapped logarithmically or linearly onto the base
*	 Frame of the FrameSet supplied when the Plot was constructed. It
*	 takes a separate value for each axis of the graphics coordinate
*	 system (i.e. the base Frame in the Plot) so that, for instance,
*	 the setting "LogPlot(2)=1" specifies that the second axis of the
*	 graphics coordinate system (usually the vertical axis) should be
*	 mapped logarithmically onto the second axis of the base Frame of
*	 the FrameSet supplied when the Plot was constructed. See also the
*	 LogGap attribute. If the LogPlot value of a Plot axis is
*	 non-zero, it causes that axis to be mapped logarithmically,
*	 otherwise (the default) the axis is mapped linearly.
*     LogTicks (int)
*        This attribute controls the appearance of an annotated coordinate
*	 grid (drawn with the astGrid function) by determining whether the
*	 major tick marks should be spaced logarithmically or linearly. It
*	 takes a separate value for each physical axis of the Plot so
*	 that, for instance, the setting "LogTicks(2)=1" specifies that
*	 the major tick marks on the second axis should be spaced
*	 logarithmically. If the LogTicks value for a physical axis is
*	 non-zero, the major tick marks on that axis will be spaced
*	 logarithmically (that is, there will be a constant ratio betwen
*	 adjacent tick mark values). If the LogTicks value is zero, the
*	 major tick marks will be evenly spaced (that is, there will be a
*	 constant difference betwen adjacent tick mark values). The
*	 default is to produce logarithmically spaced tick marks if the
*	 corresponding LogPlot attribute is non-zero.
*     MajTickLen(axis) (double)
*        This attribute gives the length of the major tick marks drawn by
c        function astGrid as a fraction of the minimum dimension of the 
f        routine AST_GRID as a fraction of the minimum dimension of the 
*        plotting area. If a negative value is given, the tick marks are 
*        placed on the outside of the grid edge or grid line, subject to 
*        any clipping imposed by the underlying graphics system. Otherwise, 
*        they are placed on the inside. The default value used depends on
*        whether a grid of lines has been drawn (see attribute Grid). If a 
*        grid has been drawn a default of zero is used (i.e. major tick 
*        marks are not drawn). Otherwise a default of +0.015 is used.
*     MinTickLen(axis) (double)
*        This attribute gives the length of the minor tick marks drawn by
c        function astGrid as a fraction of the minimum dimension of the 
f        routine AST_GRID as a fraction of the minimum dimension of the 
*        plotting area. If a negative value is given, the tick marks are 
*        placed on the outside of the grid edge or grid line, subject to 
*        any clipping imposed by the underlying graphics system. Otherwise, 
*        they are placed on the inside. A default value of +0.007 will be 
*        used if no value has been set. 
*     MinTick(axis) (int)
*        This attribute has a value for each physical axis of the grid drawn
c        by function astGrid. For each axis, it gives the number of divisions
f        by function AST_GRID. For each axis, it gives the number of divisions
*        into which to divide the gap between adjacent numerical axis values. 
*        By default, a value is chosen which depends on the size of the gap
*        between adjacent numerical axis values, and on the nature of the
*        axis values. If no axis is specified (i.e. MinTick instead of
*        MinTick(1) or MinTick(2) ),
c        astSet sets the attribute for both axes to the supplied
c        value, astClear clears the attributes for both axes, astTest
c        tests the attribute for axis 1, and astGet gets the value for
f        specified AST_SET sets the attribute for both axes to the supplied
f        value, AST_CLEAR clears the attributes for both axes, AST_TEST
f        tests the attribute for axis 1, and AST_GET gets the value for
*        axis 1.
*     NumLab(axis) (int)
*        This attribute has a value for each physical axis of the grid drawn
c        by function astGrid. For each axis, if it is non-zero, the axis
f        by routine AST_GRID. For each axis, if it is non-zero, the axis
*        will be labelled with numerical axis values. Otherwise, no numerical
*        labels are displayed for the axis. The default is to display the
*        labels. Textual labels are controlled by TextLab. If no axis is
c        specified astSet sets the attribute for both axes to the supplied
c        value, astClear clears the attributes for both axes, astTest
c        tests the attribute for axis 1, and astGet gets the value for
f        specified AST_SET sets the attribute for both axes to the supplied
f        value, AST_CLEAR clears the attributes for both axes, AST_TEST
f        tests the attribute for axis 1, and AST_GET gets the value for
*        axis 1.
*     Size(label) (double)
*        This attribute has a value for each of the types of graphical items
*        listed in the class prologue. For each item type, it gives the 
*        character size to use when drawing graphical items of that type. 
*        For instance, "size(title)=1.5" causes the plot title to be 
*        drawn with a character size 1.5 times the default character size 
*        for the underlying graphics package. The default behaviour for all 
*        types is to retain the current character size. If no type is 
c        specified, (i.e. "Size" instead of "Size(Title)", etc ), astSet sets 
c        the attribute for all types to the supplied  value, astClear clears 
c        the attributes for all types, astTest tests the attribute Size(TextLab)
c        and astGet gets the value 
f        specified, (i.e. "Size" instead of "Size(Title)", etc ), AST_SET sets 
f        the attribute for all types to the supplied  value, AST_CLEAR clears 
f        the attributes for all types, AST_TEST tests the attribute Size(TextLab)
f        and AST_GET gets the value 
*        of attribute Size(TextLab).
*     Style(label) (int)
*        This attribute has a value for each of the types of graphical items
*        listed in the class prologue. For each type, it gives the line style 
*        to use when drawing graphical items of that type. For instance, 
*        "style(border)=2" causes the plot border to be drawn with 
*        line style 2. The number of available line styles and their 
*        appearance is determined by the underlying graphics package. The 
*        default behaviour for all types is to retain the current line style.
*        If no type is specified (i.e. "Style" instead of "Style(border)",
c        etc ), astSet sets the attribute for all types to the supplied value, 
c        astClear clears the attributes for all types, astTest tests the 
c        attribute Style(Border), and astGet gets the value of attribute
f        etc ), AST_SET sets the attribute for all types to the supplied value, 
f        AST_CLEAR clears the attributes for all types, AST_TEST tests the 
f        attribute Style(Border), and AST_GET gets the value of attribute
*        Style(Border).
*     TextLab(axis) (int)
*        This attribute has a value for each physical axis of the grid drawn
c        by function astGrid. For each axis, if it is non-zero, a descriptive 
f        by routine AST_GRID. For each axis, if it is non-zero, a descriptive 
*        label is displayed on the edge specified by Edge. Otherwise, no 
*        descriptive label is displayed. The default is to display both labels
*        if tick marks would be drawn around the edges of the grid (see 
*        Labelling), and to draw neither label otherwise. The display of 
*        numerical labels is controlled by NumLab. If no axis is specified 
*        (i.e. "TextLab" instead of "TextLab(1)" or "TextLab(2)")
c        astSet sets the attribute for both axes to the supplied
c        value, astClear clears the attributes for both axes, astTest
c        tests the attribute for axis 1, and astGet gets the value for
f        AST_SET sets the attribute for both axes to the supplied
f        value, AST_CLEAR clears the attributes for both axes, AST_TEST
f        tests the attribute for axis 1, and AST_GET gets the value for
*        axis 1.
*     TextLabGap(axis) (double)
*        This attribute has a value for each physical axis of the grid drawn
c        by function astGrid. For each axis, it gives the space between the
f        by routine AST_GRID. For each axis, it gives the space between the
*        descriptive labels and the corresponding edge of a bounding box 
*        enclosing all other parts of the annotated grid. It is given as 
*        a fraction of the minimum dimension of the plotting area. 
*        Positive values cause the labels to be placed on the outside of 
*        the box. Otherwise, they are placed on the inside of the box.
*     TickAll (int)
c        This attribute controls whether or not function astGrid should draw
f        This attribute controls whether or not routine AST_GRID should draw
*        tick marks on opposite edges of the plotting area. If the attribute 
*        value is non-zero (the default), then tick marks are drawn on
*        opposite edges (if possible). Otherwise, they are drawn on the 
*        edges specified by attribute Edge, but not on the opposite edges.
*        This attribute is ignored unless tick marks would be drawn around the 
*        edges of the grid (see Labelling).
*     TitleGap (double)
*        This attribute gives the vertical space between the title drawn
c        by function astGrid, and the top edge of the plotting area, as 
f        by routine AST_GRID, and the top edge of the plotting area, as 
*        a fraction of the minimum dimension of the plotting area. 
*        Negative values cause the title to be placed inside the plotting
*        area. A default value of +0.05 will be used if no value has been 
*        set.
*     Tol (double)
*        This attribute gives the plotting tolerance as a fraction of the 
*        plotting area. This should be less than 1.0 and greater than 1.0E-10. 
*        Smaller values result in smoother and more accurate curves being drawn, 
*        but may slow down the drawing process. A value of 0.001 is used if no 
*        value is assigned to this attribute.
*     Width(label) (double)
*        This attribute has a value for each of the types of graphical items
*        listed in the class prologue. For each item type, it gives the line 
*        width to use when drawing graphical items of that type. For 
*        instance, "width(border)=0.1" causes the plot border to be 
*        drawn with a line width of 0.1. Line widths should be in the range 
*        0.0 (corresponding to the minimum available line width) to 1.0 
*        (corresponding to the maximum available line width). The default
*        behaviour for all types is to retain the current line width. If no 
*        type is specified, (i.e. "Width" instead of "Width(Border)", etc ), 
c        astSet sets the attribute for all types to the supplied  value, 
c        astClear clears the attributes for all types, astTest tests the 
c        attribute Width(Border), and astGet gets the value of attribute
f        AST_SET sets the attribute for all types to the supplied  value, 
f        AST_CLEAR clears the attributes for all types, AST_TEST tests the 
f        attribute Width(Border), and AST_GET gets the value of attribute
*        Width(Border).

*     The colour index, line style and width, and the character font
*     and size, can be set independantly for various items of graphical
*     output produced by the Plot class. These items are identified
*     by appending the following case-insensitive labels to the 
*     attributes Colour, Style, Width, Font and Size (unambiguous
*     abbreviations of these labels may also be used):
*
c     - Border: The plot boundary drawn using astBorder or astGrid
f     - Border: The plot boundary drawn using AST_BORDER or AST_GRID
c     - Grid: Curves drawn using astGridLine or astGrid
f     - Grid: Curves drawn using AST_GRIDLINE or AST_GRID
c     - Curves: Curves drawn using astCurve, astGenCurve or astPolyCurve
f     - Curves: Curves drawn using AST_CURVE, AST_GENCURVE or AST_POLYCURVE
c     - NumLab: Numerical axis labels drawn using astGrid
f     - NumLab: Numerical axis labels drawn using AST_GRID
c     - TextLab: Textual axis labels drawn using astGrid
f     - TextLab: Textual axis labels drawn using AST_GRID
c     - Title: The plot title drawn using astGrid
f     - Title: The plot title drawn using AST_GRID
c     - Markers: Graphical markers drawn using astMark
f     - Markers: Graphical markers drawn using AST_MARK
c     - Strings: Text strings drawn using astText
f     - Strings: Text strings drawn using AST_TEXT
*     - Ticks: Tick marks (both major and minor)
*     - Axes: Axes drawn through tick marks within the plotting area

*  Methods Over-Ridden:
*     Public:
*        astRemoveFrame
*           Remove a Frame from a Plot, and update the clipping Frame index.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a Plot.
*        astGetAttrib
*           Get an attribute value for a Plot.
*        astSetAttrib
*           Set an attribute value for a Plot.
*        astTestAttrib
*           Test if an attribute value has been set for a Plot.
*        astTransform
*           Transform a set of points between physical and graphics
*           coordinates, incorporating clipping.

*  New Methods Defined:
*     Public:
*        astBorder
*           Draw a curve outlining the regions containing valid physical
*           coordinates.
*        astBoundingBox
*           Return a bounding box for previously drawn graphics.
*        astClip
*           Set up or remove additional clipping for a Plot.
*        astGrid
*           Draw a complete annotated coordinate grid.
*        astGridLine
*           Draw a curve following a constant axis value.
*        astCurve
*           Draw a geodesic curve between two points.
*        astGenCurve
*           Draw a generalized curve.
*        astMark
*           Draw a set of graphical markers at given physical coordinates.
*        astPolyCurve
*           Draw a set of connected geodesic curves.
*        astText
*           Draw a text string at given physical coordinates.

*     Protected:
*        astCvBrk
*           Return information about the most recent curve drawn by
*           astGridLine, astGenCurve or astCurve (NOT astPolyCurve).

*        Protected methods are supplied to set, test, clear and test each
*        of the attributes described in the above section entitled "New
*        Attributes Defined:". They have the following names:
*
*        astClear<attrib>
*           Clear the <attrib> attribute named for a Plot.
*        astGet<attrib>
*           Get the value of the <attrib> attribute for Plot.
*        astSet<attrib>
*           Set the value of the <attrib> attribute for a Plot.
*        astTest<attrib>
*           Test whether a value has been set for the <attrib> attribute of 
*           a Plot.

*  Other Class Functions:
*     Public:
*        astPlot
*           Create a Plot.
*        astIsAPlot
*           Test class membership.
*
*     Protected:
*        astCheckPlot
*           Validate class membership.
*        astInitPlot
*           Initialise a Plot.
*        astInitPlotVtab
*           Initialise the virtual function table for the Plot class.
*        astLoadPlot
*           Load a Plot.

*  Macros:
*     Public:
*        AST__NPID 
*           Number of different graphics objects for which graphics attributes 
*           can be set using Style, Font, etc.

*     Protected:
*        AST__MXBRK 
*           Maximum number of breaks allowed in a curve drawn by astGridLine
*           or astCurve.

*  Type Definitions:
*     Public:
*        AstPlot
*           Plot object type.
*
*     Protected:
*        AstPlotVtab
*           Plot virtual function table type.
*        AstCurveData
*           Structure holding information about drawn curves.

*  Feature Test Macros:
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     18-SEP-1996 (DSB):
*        Original version.
*     28-OCT-1998 (DSB):
*        Added method astPolyCurve. 
*     12-OCT-1999 (DSB):
*        Allow tick marks to be specified separately for both axes.
*     9-JAN-2001 (DSB):
*        Change argument "in" for astMark and astPolyCurve from type
*        "const double (*)[]" to "const double *".
*     13-JUN-2001 (DSB):
*        Added methods astGenCurve, astGrfSet, astGrfPop, astGrfPush and 
*        attribute Grf.
*     8-JAN-2003 (DSB):
*        Added protected astInitPlotVtab method.
*     13-JAN-2004 (DSB):
*        Added bbox, logticks and logplot to the AstPlot structure. Added 
*        LogPlot and LogTicks accessor methods.
*     19-JAN-2004 (DSB):
*        Added loggap and loglabel to the AstPlot structure. Added 
*        LogGap and LogLabel accessor methods.
*     21-MAR-2005 (DSB):
*        - Added the Clip attribute.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "frameset.h"              /* Parent FrameSet class */

/* C header files. */
/* --------------- */
#include <stddef.h>

/* Macros. */
/* ======= */
#define AST__NPID      15   /* No. of different genuine plot object id's */

#define AST__GATTR	0   /* Identifiers for GRF functions */
#define AST__GFLUSH	1   /* Note, if any items are added or changed here, */
#define AST__GLINE	2   /* make sure that the astGrfFunID function is */
#define AST__GMARK	3   /* updated in plot.c */
#define AST__GTEXT	4
#define AST__GTXEXT	5
#define AST__GSCALES	6
#define AST__GQCH	7
#define AST__GCAP	8

#define AST__NGRFFUN    9   /* No. of Grf functions used by Plot */

#if defined(astCLASS)       /* Protected */
#define AST__MXBRK     100  /* Max. no. of breaks in a drawn curve */
#endif

/* Type Definitions */
/* ================ */

/* Pre-declare the AstPlot structure so that it can be used within the 
   GRF function typedefs. */
struct AstPlot;

/* Interfaces for GRF functions */
/* ---------------------------- */
/* A general interface into which actual Grf functions should be cast 
   before being passed as an argument to astGrfSet. */
typedef void (* AstGrfFun)( void );              

/* Interfaces for specific Grf funstions implemented in C (other languages
   may have different interfaces). */
typedef int (* AstGAttrFun)( int, double, double *, int );
typedef int (* AstGFlushFun)( void );
typedef int (* AstGLineFun)( int, const float *, const float * );
typedef int (* AstGMarkFun)( int, const float *, const float *, int );
typedef int (* AstGTextFun)( const char *, float, float, const char *, float, float );
typedef int (* AstGCapFun)( int, int );
typedef int (* AstGTxExtFun)( const char *, float, float, const char *, float, float, float *, float * );
typedef int (* AstGScalesFun)( float *, float * );
typedef int (* AstGQchFun)( float *, float * );

/* A general interface into which Grf Wrapper functions should be cast 
   before being passed as an argument to astGrfWrapper. */
typedef void (* AstGrfWrap)( void );              

/* Interfaces for the wrapper functions which wrap specific Grf funstions. */
typedef int (* AstGAttrWrap)( struct AstPlot *, int, double, double *, int );
typedef int (* AstGFlushWrap)( struct AstPlot * );
typedef int (* AstGLineWrap)( struct AstPlot *, int, const float *, const float * );
typedef int (* AstGMarkWrap)( struct AstPlot *, int, const float *, const float *, int );
typedef int (* AstGTextWrap)( struct AstPlot *, const char *, float, float, const char *, float, float );
typedef int (* AstGCapWrap)( struct AstPlot *, int, int );
typedef int (* AstGTxExtWrap)( struct AstPlot *, const char *, float, float, const char *, float, float, float *, float * );
typedef int (* AstGScalesWrap)( struct AstPlot *, float *, float * );
typedef int (* AstGQchWrap)( struct AstPlot *, float *, float * );

/* A structure in which a collection of Grf function pointers can be
   stored. */
typedef struct AstGrfPtrs {
   AstGrfFun grffun[AST__NGRFFUN];
   AstGAttrWrap GAttr;
   AstGFlushWrap GFlush;
   AstGLineWrap GLine;
   AstGMarkWrap GMark;
   AstGTextWrap GText;
   AstGCapWrap GCap;
   AstGTxExtWrap GTxExt;
   AstGScalesWrap GScales;
   AstGQchWrap GQch;
} AstGrfPtrs;

/* Structure holding current graphical attribute values for text. */
typedef struct AstGat {
   float rise;
   double size;
   double width;
   double col;
   double font;
   double style;
} AstGat;

/* Plot structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstPlot {

/* Attributes inherited from the parent class. */
   AstFrameSet parent;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   double *clip_lbnd;
   double *clip_ubnd;
   double centre[ 2 ];
   double gap[ 2 ];
   double loggap[ 2 ];
   double labelat[ 2 ];
   double majticklen[ 2 ];
   double minticklen[ 2 ];
   double numlabgap[ 2 ];
   double size[ AST__NPID ];
   double textlabgap[ 2 ];
   double titlegap;
   double tol;
   double ucentre[ 2 ];
   double ugap[ 2 ];
   double uloggap[ 2 ];
   double ulblat[ 2 ];
   double umjtkln[ 2 ];
   double width[ AST__NPID ];
   double xhi;
   double xlo;
   double yhi;
   double ylo;
   double bbox[ 4 ];
   int border;
   int clip_axes;
   int clip_frame;
   int clip;
   int clipop;
   int colour[ AST__NPID ];
   int drawaxes[ 2 ];
   int abbrev[ 2 ];
   int escape;
   int drawtitle;
   int edge[ 2 ];
   int font[ AST__NPID ];
   int grf;
   int grid;
   int invisible;
   int labelling;
   int labelunits[ 2 ];
   int labelup[ 2 ];
   int mintick[ 2 ];
   int numlab[ 2 ];
   int style[ AST__NPID ];
   int textlab[ 2 ];
   int tickall;
   int uborder;
   int uedge[ 2 ];
   int ugrid;
   int ulbling;
   int ulbunit[ 2 ];
   int ulgtk[ 2 ];
   int ulglb[ 2 ];
   int umintk[ 2 ];
   int utxtlb[ 2 ];
   int xrev;
   int yrev;      
   int ink;
   int logplot[ 2 ];
   int logticks[ 2 ];
   int loglabel[ 2 ];
   AstGrfFun grffun[AST__NGRFFUN];
   AstGAttrWrap GAttr;
   AstGFlushWrap GFlush;
   AstGLineWrap GLine;
   AstGMarkWrap GMark;
   AstGTextWrap GText;
   AstGCapWrap GCap;
   AstGTxExtWrap GTxExt;
   AstGScalesWrap GScales;
   AstGQchWrap GQch;
   AstGrfPtrs *grfstack;
   int grfnstack;
   AstGat **gat;
   int ngat;

} AstPlot;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */

typedef struct AstPlotVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameSetVtab FrameSet_vtab;/* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   int (* Border)( AstPlot * );
   void (* BoundingBox)( AstPlot *, float[2], float[2] );
   void (* Clip)( AstPlot *, int, const double [], const double [] );
   int (* CvBrk)( AstPlot *, int, double *, double *, double * );
   void (* GridLine)( AstPlot *, int, const double [], double );
   void (* Curve)( AstPlot *, const double [], const double [] );
   void (* GenCurve)( AstPlot *, AstMapping * );
   void (* PolyCurve)( AstPlot *, int, int, int, const double * );
   void (* GrfSet)( AstPlot *, const char *, AstGrfFun );
   void (* GrfPush)( AstPlot * );
   void (* GrfPop)( AstPlot * );
   void (* GrfWrapper)( AstPlot *, const char *, AstGrfWrap );
   void (* Grid)( AstPlot * ); 
   void (* Mark)( AstPlot *, int, int, int, const double *, int  ); 
   void (* Text)( AstPlot *, const char *, const double [], const float [2], const char * );
   double (* GetTol)( AstPlot * );
   int (* TestTol)( AstPlot * );
   void (* SetTol)( AstPlot *, double );
   void (* ClearTol)( AstPlot * );
   int (* GetGrid)( AstPlot * );
   int (* TestGrid)( AstPlot * );
   void (* SetGrid)( AstPlot *, int );
   void (* ClearGrid)( AstPlot * );
   int (* GetTickAll)( AstPlot * );
   int (* TestTickAll)( AstPlot * );
   void (* SetTickAll)( AstPlot *, int );
   void (* ClearTickAll)( AstPlot * );
   int (* GetInvisible)( AstPlot * );
   int (* TestInvisible)( AstPlot * );
   void (* SetInvisible)( AstPlot *, int );
   void (* ClearInvisible)( AstPlot * );
   int (* GetBorder)( AstPlot * );
   int (* TestBorder)( AstPlot * );
   void (* SetBorder)( AstPlot *, int );
   void (* ClearBorder)( AstPlot * );
   int (* GetClipOp)( AstPlot * );
   int (* TestClipOp)( AstPlot * );
   void (* SetClipOp)( AstPlot *, int );
   void (* ClearClipOp)( AstPlot * );
   int (* GetClip)( AstPlot * );
   int (* TestClip)( AstPlot * );
   void (* SetClip)( AstPlot *, int );
   void (* ClearClip)( AstPlot * );
   int (* GetGrf)( AstPlot * );
   int (* TestGrf)( AstPlot * );
   void (* SetGrf)( AstPlot *, int );
   void (* ClearGrf)( AstPlot * );
   int (* GetDrawTitle)( AstPlot * );
   int (* TestDrawTitle)( AstPlot * );
   void (* SetDrawTitle)( AstPlot *, int );
   void (* ClearDrawTitle)( AstPlot * );
   int (* GetLabelUp)( AstPlot *, int );
   int (* TestLabelUp)( AstPlot *, int );
   void (* SetLabelUp)( AstPlot *, int, int );
   void (* ClearLabelUp)( AstPlot *, int );
   int (* GetLogPlot)( AstPlot *, int );
   int (* TestLogPlot)( AstPlot *, int );
   void (* SetLogPlot)( AstPlot *, int, int );
   void (* ClearLogPlot)( AstPlot *, int );
   int (* GetLogTicks)( AstPlot *, int );
   int (* TestLogTicks)( AstPlot *, int );
   void (* SetLogTicks)( AstPlot *, int, int );
   void (* ClearLogTicks)( AstPlot *, int );
   int (* GetLogLabel)( AstPlot *, int );
   int (* TestLogLabel)( AstPlot *, int );
   void (* SetLogLabel)( AstPlot *, int, int );
   void (* ClearLogLabel)( AstPlot *, int );
   int (* GetDrawAxes)( AstPlot *, int );
   int (* TestDrawAxes)( AstPlot *, int );
   void (* SetDrawAxes)( AstPlot *, int, int );
   void (* ClearDrawAxes)( AstPlot *, int );
   int (* GetAbbrev)( AstPlot *, int );
   int (* TestAbbrev)( AstPlot *, int );
   void (* SetAbbrev)( AstPlot *, int, int );
   void (* ClearAbbrev)( AstPlot *, int );
   int (* GetEscape)( AstPlot * );
   int (* TestEscape)( AstPlot * );
   void (* SetEscape)( AstPlot *, int );
   void (* ClearEscape)( AstPlot * );
   int (* GetLabelling)( AstPlot * );
   int (* TestLabelling)( AstPlot * );
   void (* SetLabelling)( AstPlot *, int );
   void (* ClearLabelling)( AstPlot * );
   double (* GetMajTickLen)( AstPlot *, int );
   int (* TestMajTickLen)( AstPlot *, int );
   void (* SetMajTickLen)( AstPlot *, int, double );
   void (* ClearMajTickLen)( AstPlot *, int );
   double (* GetMinTickLen)( AstPlot *, int );
   int (* TestMinTickLen)( AstPlot *, int );
   void (* SetMinTickLen)( AstPlot *, int, double );
   void (* ClearMinTickLen)( AstPlot *, int );
   double (* GetNumLabGap)( AstPlot *, int );
   int (* TestNumLabGap)( AstPlot *, int );
   void (* SetNumLabGap)( AstPlot *, int, double );
   void (* ClearNumLabGap)( AstPlot *, int );
   double (* GetTextLabGap)( AstPlot *, int );
   int (* TestTextLabGap)( AstPlot *, int );
   void (* SetTextLabGap)( AstPlot *, int, double );
   void (* ClearTextLabGap)( AstPlot *, int );
   double (* GetTitleGap)( AstPlot * );
   int (* TestTitleGap)( AstPlot * );
   void (* SetTitleGap)( AstPlot *, double );
   void (* ClearTitleGap)( AstPlot * );
   double (* GetLabelAt)( AstPlot *, int  );
   int (* TestLabelAt)( AstPlot *, int  );
   void (* SetLabelAt)( AstPlot *, int, double );
   void (* ClearLabelAt)( AstPlot *, int );
   double (* GetGap)( AstPlot *, int  );
   int (* TestGap)( AstPlot *, int  );
   void (* SetGap)( AstPlot *, int, double );
   void (* ClearGap)( AstPlot *, int );
   double (* GetLogGap)( AstPlot *, int  );
   int (* TestLogGap)( AstPlot *, int  );
   void (* SetLogGap)( AstPlot *, int, double );
   void (* ClearLogGap)( AstPlot *, int );
   double (* GetCentre)( AstPlot *, int  );
   int (* TestCentre)( AstPlot *, int  );
   void (* SetCentre)( AstPlot *, int, double );
   void (* ClearCentre)( AstPlot *, int );
   int (* GetEdge)( AstPlot *, int );
   int (* TestEdge)( AstPlot *, int );
   void (* SetEdge)( AstPlot *, int, int );
   void (* ClearEdge)( AstPlot *, int );
   int (* GetNumLab)( AstPlot *, int );
   int (* TestNumLab)( AstPlot *, int );
   void (* SetNumLab)( AstPlot *, int, int );
   void (* ClearNumLab)( AstPlot *, int );
   int (* GetMinTick)( AstPlot *, int );
   int (* TestMinTick)( AstPlot *, int );
   void (* SetMinTick)( AstPlot *, int, int );
   void (* ClearMinTick)( AstPlot *, int );
   int (* GetTextLab)( AstPlot *, int );
   int (* TestTextLab)( AstPlot *, int );
   void (* SetTextLab)( AstPlot *, int, int );
   void (* ClearTextLab)( AstPlot *, int );
   int (* GetLabelUnits)( AstPlot *, int );
   int (* TestLabelUnits)( AstPlot *, int );
   void (* SetLabelUnits)( AstPlot *, int, int );
   void (* ClearLabelUnits)( AstPlot *, int );
   int (* GetStyle)( AstPlot *, int );
   int (* TestStyle)( AstPlot *, int );
   void (* SetStyle)( AstPlot *, int, int );
   void (* ClearStyle)( AstPlot *, int );
   int (* GetFont)( AstPlot *, int );
   int (* TestFont)( AstPlot *, int );
   void (* SetFont)( AstPlot *, int, int );
   void (* ClearFont)( AstPlot *, int );
   int (* GetColour)( AstPlot *, int );
   int (* TestColour)( AstPlot *, int );
   void (* SetColour)( AstPlot *, int, int );
   void (* ClearColour)( AstPlot *, int );
   double (* GetWidth)( AstPlot *, int );
   int (* TestWidth)( AstPlot *, int );
   void (* SetWidth)( AstPlot *, int, double );
   void (* ClearWidth)( AstPlot *, int );
   double (* GetSize)( AstPlot *, int );
   int (* TestSize)( AstPlot *, int );
   void (* SetSize)( AstPlot *, int, double );
   void (* ClearSize)( AstPlot *, int );
   int (* GetInk)( AstPlot * );
   int (* TestInk)( AstPlot * );
   void (* SetInk)( AstPlot *, int );
   void (* ClearInk)( AstPlot * );

} AstPlotVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Plot)          /* Check class membership */
astPROTO_ISA(Plot)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPlot *astPlot_( void *, const float *, const double *, const char *, ... );
#else
AstPlot *astPlotId_( void *, const float [], const double [], const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPlot *astInitPlot_( void *, size_t, int, AstPlotVtab *, 
                       const char *, AstFrame *, const float *, const double * );

/* Vtab initialiser. */
void astInitPlotVtab_( AstPlotVtab *, const char * );

/* Loader. */
AstPlot *astLoadPlot_( void *, size_t, AstPlotVtab *,
                       const char *, AstChannel *channel );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
   const char *astStripEscapes_( const char * );
   int astFindEscape_( const char *, int *, int *, int * );
   int astBorder_( AstPlot * );
   void astBoundingBox_( AstPlot *, float[2], float[2] );
   void astClip_( AstPlot *, int, const double [], const double [] );
   void astGridLine_( AstPlot *, int, const double [], double );
   void astCurve_( AstPlot *, const double [], const double [] );
   void astGrid_( AstPlot * );
   void astMark_( AstPlot *, int, int, int, const double *, int  ); 
   void astGrfSet_( AstPlot *, const char *, AstGrfFun );
   void astGrfPush_( AstPlot * );
   void astGrfPop_( AstPlot * );
   void astGenCurve_( AstPlot *, AstMapping * );
   void astPolyCurve_( AstPlot *, int, int, int, const double * );
   void astText_( AstPlot *, const char *, const double [], const float [2], const char * );

   void astGrfWrapper_( AstPlot *, const char *, AstGrfWrap );
   int astGrfFunID_( const char *, const char *, const char *  );

#if defined(astCLASS)            /* Protected */
   int astCvBrk_( AstPlot *, int, double *, double *, double * );

   double astGetTol_( AstPlot * );
   int astTestTol_( AstPlot * );
   void astSetTol_( AstPlot *, double );
   void astClearTol_( AstPlot * );

   int astGetGrid_( AstPlot * );
   int astTestGrid_( AstPlot * );
   void astSetGrid_( AstPlot *, int );
   void astClearGrid_( AstPlot * );

   int astGetTickAll_( AstPlot * );
   int astTestTickAll_( AstPlot * );
   void astSetTickAll_( AstPlot *, int );
   void astClearTickAll_( AstPlot * );

   int astGetInvisible_( AstPlot * );
   int astTestInvisible_( AstPlot * );
   void astSetInvisible_( AstPlot *, int );
   void astClearInvisible_( AstPlot * );

   int astGetBorder_( AstPlot * );
   int astTestBorder_( AstPlot * );
   void astSetBorder_( AstPlot *, int );
   void astClearBorder_( AstPlot * );

   int astGetClip_( AstPlot * );
   int astTestClip_( AstPlot * );
   void astSetClip_( AstPlot *, int );
   void astClearClip_( AstPlot * );

   int astGetClipOp_( AstPlot * );
   int astTestClipOp_( AstPlot * );
   void astSetClipOp_( AstPlot *, int );
   void astClearClipOp_( AstPlot * );

   int astGetGrf_( AstPlot * );
   int astTestGrf_( AstPlot * );
   void astSetGrf_( AstPlot *, int );
   void astClearGrf_( AstPlot * );

   int astGetDrawTitle_( AstPlot * );
   int astTestDrawTitle_( AstPlot * );
   void astSetDrawTitle_( AstPlot *, int );
   void astClearDrawTitle_( AstPlot * );

   int astGetLabelUp_( AstPlot *, int );
   int astTestLabelUp_( AstPlot *, int );
   void astSetLabelUp_( AstPlot *, int, int );
   void astClearLabelUp_( AstPlot *, int );

   int astGetLogPlot_( AstPlot *, int );
   int astTestLogPlot_( AstPlot *, int );
   void astSetLogPlot_( AstPlot *, int, int );
   void astClearLogPlot_( AstPlot *, int );

   int astGetLogTicks_( AstPlot *, int );
   int astTestLogTicks_( AstPlot *, int );
   void astSetLogTicks_( AstPlot *, int, int );
   void astClearLogTicks_( AstPlot *, int );

   int astGetLogLabel_( AstPlot *, int );
   int astTestLogLabel_( AstPlot *, int );
   void astSetLogLabel_( AstPlot *, int, int );
   void astClearLogLabel_( AstPlot *, int );

   int astGetDrawAxes_( AstPlot *, int );
   int astTestDrawAxes_( AstPlot *, int );
   void astSetDrawAxes_( AstPlot *, int, int );
   void astClearDrawAxes_( AstPlot *, int );

   int astGetAbbrev_( AstPlot *, int );
   int astTestAbbrev_( AstPlot *, int );
   void astSetAbbrev_( AstPlot *, int, int );
   void astClearAbbrev_( AstPlot *, int );

   int astGetEscape_( AstPlot * );
   int astTestEscape_( AstPlot * );
   void astSetEscape_( AstPlot *, int );
   void astClearEscape_( AstPlot * );

   double astGetLabelAt_( AstPlot *, int  );
   int astTestLabelAt_( AstPlot *, int  );
   void astSetLabelAt_( AstPlot *, int, double );
   void astClearLabelAt_( AstPlot *, int );

   double astGetGap_( AstPlot *, int  );
   int astTestGap_( AstPlot *, int  );
   void astSetGap_( AstPlot *, int, double );
   void astClearGap_( AstPlot *, int );

   double astGetLogGap_( AstPlot *, int  );
   int astTestLogGap_( AstPlot *, int  );
   void astSetLogGap_( AstPlot *, int, double );
   void astClearLogGap_( AstPlot *, int );

   double astGetCentre_( AstPlot *, int  );
   int astTestCentre_( AstPlot *, int  );
   void astSetCentre_( AstPlot *, int, double );
   void astClearCentre_( AstPlot *, int );

   int astGetLabelling_( AstPlot * );
   int astTestLabelling_( AstPlot * );
   void astSetLabelling_( AstPlot *, int );
   void astClearLabelling_( AstPlot * );

   double astGetMajTickLen_( AstPlot *, int );
   int astTestMajTickLen_( AstPlot *, int );
   void astSetMajTickLen_( AstPlot *, int, double );
   void astClearMajTickLen_( AstPlot *, int );

   double astGetMinTickLen_( AstPlot *, int );
   int astTestMinTickLen_( AstPlot *, int );
   void astSetMinTickLen_( AstPlot *, int, double );
   void astClearMinTickLen_( AstPlot *, int );

   double astGetNumLabGap_( AstPlot *, int );
   int astTestNumLabGap_( AstPlot *, int );
   void astSetNumLabGap_( AstPlot *, int, double );
   void astClearNumLabGap_( AstPlot *, int );

   double astGetTextLabGap_( AstPlot *, int );
   int astTestTextLabGap_( AstPlot *, int );
   void astSetTextLabGap_( AstPlot *, int, double );
   void astClearTextLabGap_( AstPlot *, int );

   double astGetTitleGap_( AstPlot * );
   int astTestTitleGap_( AstPlot * );
   void astSetTitleGap_( AstPlot *, double );
   void astClearTitleGap_( AstPlot * );

   int astGetEdge_( AstPlot *, int );
   int astTestEdge_( AstPlot *, int );
   void astSetEdge_( AstPlot *, int, int );
   void astClearEdge_( AstPlot *, int );

   int astGetMinTick_( AstPlot *, int );
   int astTestMinTick_( AstPlot *, int );
   void astSetMinTick_( AstPlot *, int, int );
   void astClearMinTick_( AstPlot *, int );

   int astGetNumLab_( AstPlot *, int );
   int astTestNumLab_( AstPlot *, int );
   void astSetNumLab_( AstPlot *, int, int );
   void astClearNumLab_( AstPlot *, int );

   int astGetTextLab_( AstPlot *, int );
   int astTestTextLab_( AstPlot *, int );
   void astSetTextLab_( AstPlot *, int, int );
   void astClearTextLab_( AstPlot *, int );

   int astGetLabelUnits_( AstPlot *, int );
   int astTestLabelUnits_( AstPlot *, int );
   void astSetLabelUnits_( AstPlot *, int, int );
   void astClearLabelUnits_( AstPlot *, int );

   int astGetStyle_( AstPlot *, int );
   int astTestStyle_( AstPlot *, int );
   void astSetStyle_( AstPlot *, int, int );
   void astClearStyle_( AstPlot *, int );

   int astGetFont_( AstPlot *, int );
   int astTestFont_( AstPlot *, int );
   void astSetFont_( AstPlot *, int, int );
   void astClearFont_( AstPlot *, int );

   int astGetColour_( AstPlot *, int );
   int astTestColour_( AstPlot *, int );
   void astSetColour_( AstPlot *, int, int );
   void astClearColour_( AstPlot *, int );

   double astGetWidth_( AstPlot *, int );
   int astTestWidth_( AstPlot *, int );
   void astSetWidth_( AstPlot *, int, double );
   void astClearWidth_( AstPlot *, int );

   double astGetSize_( AstPlot *, int );
   int astTestSize_( AstPlot *, int );
   void astSetSize_( AstPlot *, int, double );
   void astClearSize_( AstPlot *, int );

   int astGetInk_( AstPlot * );
   int astTestInk_( AstPlot * );
   void astSetInk_( AstPlot *, int );
   void astClearInk_( AstPlot * );
#endif

/* Function interfaces. */
/* ==================== */
/* These macros are wrap-ups for the functions defined by this class to make
   them easier to invoke (e.g. to avoid type mis-matches when passing pointers
   to objects from derived classes). */

/* Interfaces to standard class functions. */
/* --------------------------------------- */
/* Some of these functions provide validation, so we cannot use them to
   validate their own arguments. We must use a cast when passing object
   pointers (so that they can accept objects from derived classes). */

/* Check class membership. */
#define astCheckPlot(this) astINVOKE_CHECK(Plot,this)

/* Test class membership. */
#define astIsAPlot(this) astINVOKE_ISA(Plot,this)

#if defined(astCLASS)            /* Protected */
#define astPlot astINVOKE(F,astPlot_)
#else
#define astPlot astINVOKE(F,astPlotId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitPlot(mem,size,init,vtab,name,frame,graph,base) \
astINVOKE(O,astInitPlot_(mem,size,init,vtab,name,frame,graph,base))

/* Vtab Initialiser. */
#define astInitPlotVtab(vtab,name) astINVOKE(V,astInitPlotVtab_(vtab,name))
/* Loader. */
#define astLoadPlot(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPlot_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to member functions. */
/* ------------------------------- */
/* Here we make use of astCheckPlot (et al.) to validate Plot
   pointers before use. This provides a contextual error report if a
   pointer to the wrong sort of object is supplied. */


#define astBorder(this) \
astINVOKE(V,astBorder_(astCheckPlot(this)))

#define astBoundingBox(this,lbnd,ubnd) \
astINVOKE(V,astBoundingBox_(astCheckPlot(this),lbnd,ubnd))

#define astClip(this,iframe,lbnd,ubnd) \
astINVOKE(V,astClip_(astCheckPlot(this),iframe,lbnd,ubnd))

#define astMark(this,nmark,ncoord,indim,in,type) \
astINVOKE(V,astMark_(astCheckPlot(this),nmark,ncoord,indim,in,type))

#define astText(this,text,pos,up,just) \
astINVOKE(V,astText_(astCheckPlot(this),text,pos,up,just))

#define astGrid(this) \
astINVOKE(V,astGrid_(astCheckPlot(this)))

#define astGridLine(this,axis,start,length) \
astINVOKE(V,astGridLine_(astCheckPlot(this),axis,start,length))

#define astCurve(this,start,finish) \
astINVOKE(V,astCurve_(astCheckPlot(this),start,finish))

#define astGenCurve(this,map) \
astINVOKE(V,astGenCurve_(astCheckPlot(this),astCheckMapping(map)))

#define astPolyCurve(this,npoint,ncoord,dim,in) \
astINVOKE(V,astPolyCurve_(astCheckPlot(this),npoint,ncoord,dim,in))

#define astGrfSet(this,name,fun) \
astINVOKE(V,astGrfSet_(astCheckPlot(this),name,fun))

#define astGrfPush(this) \
astINVOKE(V,astGrfPush_(astCheckPlot(this)))

#define astGrfPop(this) \
astINVOKE(V,astGrfPop_(astCheckPlot(this)))


#define astGrfFunID astGrfFunID_
#define astFindEscape astFindEscape_
#define astStripEscapes(text) astStripEscapes_(text)

#define astGrfWrapper(this,name,wrapper) \
astINVOKE(V,astGrfWrapper_(astCheckPlot(this),name,wrapper))

#if defined(astCLASS)            /* Protected */

#define astCvBrk(this,ibrk,brk,vbrk,len) \
astINVOKE(V,astCvBrk_(astCheckPlot(this),ibrk,brk,vbrk,len))

#define astClearTol(this) \
astINVOKE(V,astClearTol_(astCheckPlot(this)))
#define astGetTol(this) \
astINVOKE(V,astGetTol_(astCheckPlot(this)))
#define astSetTol(this,tol) \
astINVOKE(V,astSetTol_(astCheckPlot(this),tol))
#define astTestTol(this) \
astINVOKE(V,astTestTol_(astCheckPlot(this)))

#define astClearGrid(this) \
astINVOKE(V,astClearGrid_(astCheckPlot(this)))
#define astGetGrid(this) \
astINVOKE(V,astGetGrid_(astCheckPlot(this)))
#define astSetGrid(this,grid) \
astINVOKE(V,astSetGrid_(astCheckPlot(this),grid))
#define astTestGrid(this) \
astINVOKE(V,astTestGrid_(astCheckPlot(this)))

#define astClearInk(this) \
astINVOKE(V,astClearInk_(astCheckPlot(this)))
#define astGetInk(this) \
astINVOKE(V,astGetInk_(astCheckPlot(this)))
#define astSetInk(this,ink) \
astINVOKE(V,astSetInk_(astCheckPlot(this),ink))
#define astTestInk(this) \
astINVOKE(V,astTestInk_(astCheckPlot(this)))

#define astClearTickAll(this) \
astINVOKE(V,astClearTickAll_(astCheckPlot(this)))
#define astGetTickAll(this) \
astINVOKE(V,astGetTickAll_(astCheckPlot(this)))
#define astSetTickAll(this,tickall) \
astINVOKE(V,astSetTickAll_(astCheckPlot(this),tickall))
#define astTestTickAll(this) \
astINVOKE(V,astTestTickAll_(astCheckPlot(this)))

#define astClearBorder(this) \
astINVOKE(V,astClearBorder_(astCheckPlot(this)))
#define astGetBorder(this) \
astINVOKE(V,astGetBorder_(astCheckPlot(this)))
#define astSetBorder(this,border) \
astINVOKE(V,astSetBorder_(astCheckPlot(this),border))
#define astTestBorder(this) \
astINVOKE(V,astTestBorder_(astCheckPlot(this)))

#define astClearClip(this) \
astINVOKE(V,astClearClip_(astCheckPlot(this)))
#define astGetClip(this) \
astINVOKE(V,astGetClip_(astCheckPlot(this)))
#define astSetClip(this,clip) \
astINVOKE(V,astSetClip_(astCheckPlot(this),clip))
#define astTestClip(this) \
astINVOKE(V,astTestClip_(astCheckPlot(this)))

#define astClearClipOp(this) \
astINVOKE(V,astClearClipOp_(astCheckPlot(this)))
#define astGetClipOp(this) \
astINVOKE(V,astGetClipOp_(astCheckPlot(this)))
#define astSetClipOp(this,clipop) \
astINVOKE(V,astSetClipOp_(astCheckPlot(this),clipop))
#define astTestClipOp(this) \
astINVOKE(V,astTestClipOp_(astCheckPlot(this)))

#define astClearInvisible(this) \
astINVOKE(V,astClearInvisible_(astCheckPlot(this)))
#define astGetInvisible(this) \
astINVOKE(V,astGetInvisible_(astCheckPlot(this)))
#define astSetInvisible(this,invisible) \
astINVOKE(V,astSetInvisible_(astCheckPlot(this),invisible))
#define astTestInvisible(this) \
astINVOKE(V,astTestInvisible_(astCheckPlot(this)))

#define astClearGrf(this) \
astINVOKE(V,astClearGrf_(astCheckPlot(this)))
#define astGetGrf(this) \
astINVOKE(V,astGetGrf_(astCheckPlot(this)))
#define astSetGrf(this,grf) \
astINVOKE(V,astSetGrf_(astCheckPlot(this),grf))
#define astTestGrf(this) \
astINVOKE(V,astTestGrf_(astCheckPlot(this)))

#define astClearDrawTitle(this) \
astINVOKE(V,astClearDrawTitle_(astCheckPlot(this)))
#define astGetDrawTitle(this) \
astINVOKE(V,astGetDrawTitle_(astCheckPlot(this)))
#define astSetDrawTitle(this,drawtitle) \
astINVOKE(V,astSetDrawTitle_(astCheckPlot(this),drawtitle))
#define astTestDrawTitle(this) \
astINVOKE(V,astTestDrawTitle_(astCheckPlot(this)))

#define astClearDrawAxes(this,axis) \
astINVOKE(V,astClearDrawAxes_(astCheckPlot(this),axis))
#define astGetDrawAxes(this,axis) \
astINVOKE(V,astGetDrawAxes_(astCheckPlot(this),axis))
#define astSetDrawAxes(this,axis,drawaxes) \
astINVOKE(V,astSetDrawAxes_(astCheckPlot(this),axis,drawaxes))
#define astTestDrawAxes(this,axis) \
astINVOKE(V,astTestDrawAxes_(astCheckPlot(this),axis))

#define astClearAbbrev(this,axis) \
astINVOKE(V,astClearAbbrev_(astCheckPlot(this),axis))
#define astGetAbbrev(this,axis) \
astINVOKE(V,astGetAbbrev_(astCheckPlot(this),axis))
#define astSetAbbrev(this,axis,abbrev) \
astINVOKE(V,astSetAbbrev_(astCheckPlot(this),axis,abbrev))
#define astTestAbbrev(this,axis) \
astINVOKE(V,astTestAbbrev_(astCheckPlot(this),axis))

#define astClearEscape(this) \
astINVOKE(V,astClearEscape_(astCheckPlot(this)))
#define astGetEscape(this) \
astINVOKE(V,astGetEscape_(astCheckPlot(this)))
#define astSetEscape(this,escape) \
astINVOKE(V,astSetEscape_(astCheckPlot(this),escape))
#define astTestEscape(this) \
astINVOKE(V,astTestEscape_(astCheckPlot(this)))

#define astClearLabelAt(this,axis) \
astINVOKE(V,astClearLabelAt_(astCheckPlot(this),axis))
#define astGetLabelAt(this,axis) \
astINVOKE(V,astGetLabelAt_(astCheckPlot(this),axis))
#define astSetLabelAt(this,axis,labelat) \
astINVOKE(V,astSetLabelAt_(astCheckPlot(this),axis,labelat))
#define astTestLabelAt(this,axis) \
astINVOKE(V,astTestLabelAt_(astCheckPlot(this),axis))

#define astClearGap(this,axis) \
astINVOKE(V,astClearGap_(astCheckPlot(this),axis))
#define astGetGap(this,axis) \
astINVOKE(V,astGetGap_(astCheckPlot(this),axis))
#define astSetGap(this,axis,gap) \
astINVOKE(V,astSetGap_(astCheckPlot(this),axis,gap))
#define astTestGap(this,axis) \
astINVOKE(V,astTestGap_(astCheckPlot(this),axis))

#define astClearLogGap(this,axis) \
astINVOKE(V,astClearLogGap_(astCheckPlot(this),axis))
#define astGetLogGap(this,axis) \
astINVOKE(V,astGetLogGap_(astCheckPlot(this),axis))
#define astSetLogGap(this,axis,gap) \
astINVOKE(V,astSetLogGap_(astCheckPlot(this),axis,gap))
#define astTestLogGap(this,axis) \
astINVOKE(V,astTestLogGap_(astCheckPlot(this),axis))

#define astClearCentre(this,axis) \
astINVOKE(V,astClearCentre_(astCheckPlot(this),axis))
#define astGetCentre(this,axis) \
astINVOKE(V,astGetCentre_(astCheckPlot(this),axis))
#define astSetCentre(this,axis,centre) \
astINVOKE(V,astSetCentre_(astCheckPlot(this),axis,centre))
#define astTestCentre(this,axis) \
astINVOKE(V,astTestCentre_(astCheckPlot(this),axis))

#define astClearMajTickLen(this,axis) \
astINVOKE(V,astClearMajTickLen_(astCheckPlot(this),axis))
#define astGetMajTickLen(this,axis) \
astINVOKE(V,astGetMajTickLen_(astCheckPlot(this),axis))
#define astSetMajTickLen(this,axis,majticklen) \
astINVOKE(V,astSetMajTickLen_(astCheckPlot(this),axis,majticklen))
#define astTestMajTickLen(this,axis) \
astINVOKE(V,astTestMajTickLen_(astCheckPlot(this),axis))

#define astClearMinTickLen(this,axis) \
astINVOKE(V,astClearMinTickLen_(astCheckPlot(this),axis))
#define astGetMinTickLen(this,axis) \
astINVOKE(V,astGetMinTickLen_(astCheckPlot(this),axis))
#define astSetMinTickLen(this,axis,minticklen) \
astINVOKE(V,astSetMinTickLen_(astCheckPlot(this),axis,minticklen))
#define astTestMinTickLen(this,axis) \
astINVOKE(V,astTestMinTickLen_(astCheckPlot(this),axis))

#define astClearNumLabGap(this,axis) \
astINVOKE(V,astClearNumLabGap_(astCheckPlot(this),axis))
#define astGetNumLabGap(this,axis) \
astINVOKE(V,astGetNumLabGap_(astCheckPlot(this),axis))
#define astSetNumLabGap(this,axis,numlabgap) \
astINVOKE(V,astSetNumLabGap_(astCheckPlot(this),axis,numlabgap))
#define astTestNumLabGap(this,axis) \
astINVOKE(V,astTestNumLabGap_(astCheckPlot(this),axis))

#define astClearTextLabGap(this,axis) \
astINVOKE(V,astClearTextLabGap_(astCheckPlot(this),axis))
#define astGetTextLabGap(this,axis) \
astINVOKE(V,astGetTextLabGap_(astCheckPlot(this),axis))
#define astSetTextLabGap(this,axis,textlabgap) \
astINVOKE(V,astSetTextLabGap_(astCheckPlot(this),axis,textlabgap))
#define astTestTextLabGap(this,axis) \
astINVOKE(V,astTestTextLabGap_(astCheckPlot(this),axis))

#define astClearTitleGap(this) \
astINVOKE(V,astClearTitleGap_(astCheckPlot(this)))
#define astGetTitleGap(this) \
astINVOKE(V,astGetTitleGap_(astCheckPlot(this)))
#define astSetTitleGap(this,titlegap) \
astINVOKE(V,astSetTitleGap_(astCheckPlot(this),titlegap))
#define astTestTitleGap(this) \
astINVOKE(V,astTestTitleGap_(astCheckPlot(this)))

#define astClearLabelling(this) \
astINVOKE(V,astClearLabelling_(astCheckPlot(this)))
#define astGetLabelling(this) \
astINVOKE(V,astGetLabelling_(astCheckPlot(this)))
#define astSetLabelling(this,labelling) \
astINVOKE(V,astSetLabelling_(astCheckPlot(this),labelling))
#define astTestLabelling(this) \
astINVOKE(V,astTestLabelling_(astCheckPlot(this)))

#define astClearEdge(this,axis) \
astINVOKE(V,astClearEdge_(astCheckPlot(this),axis))
#define astGetEdge(this,axis) \
astINVOKE(V,astGetEdge_(astCheckPlot(this),axis))
#define astSetEdge(this,axis,edge) \
astINVOKE(V,astSetEdge_(astCheckPlot(this),axis,edge))
#define astTestEdge(this,axis) \
astINVOKE(V,astTestEdge_(astCheckPlot(this),axis))

#define astClearMinTick(this,axis) \
astINVOKE(V,astClearMinTick_(astCheckPlot(this),axis))
#define astGetMinTick(this,axis) \
astINVOKE(V,astGetMinTick_(astCheckPlot(this),axis))
#define astSetMinTick(this,axis,mintick) \
astINVOKE(V,astSetMinTick_(astCheckPlot(this),axis,mintick))
#define astTestMinTick(this,axis) \
astINVOKE(V,astTestMinTick_(astCheckPlot(this),axis))

#define astClearNumLab(this,axis) \
astINVOKE(V,astClearNumLab_(astCheckPlot(this),axis))
#define astGetNumLab(this,axis) \
astINVOKE(V,astGetNumLab_(astCheckPlot(this),axis))
#define astSetNumLab(this,axis,numlab) \
astINVOKE(V,astSetNumLab_(astCheckPlot(this),axis,numlab))
#define astTestNumLab(this,axis) \
astINVOKE(V,astTestNumLab_(astCheckPlot(this),axis))

#define astClearLabelUp(this,axis) \
astINVOKE(V,astClearLabelUp_(astCheckPlot(this),axis))
#define astGetLabelUp(this,axis) \
astINVOKE(V,astGetLabelUp_(astCheckPlot(this),axis))
#define astSetLabelUp(this,axis,labelup) \
astINVOKE(V,astSetLabelUp_(astCheckPlot(this),axis,labelup))
#define astTestLabelUp(this,axis) \
astINVOKE(V,astTestLabelUp_(astCheckPlot(this),axis))

#define astClearLogPlot(this,axis) \
astINVOKE(V,astClearLogPlot_(astCheckPlot(this),axis))
#define astGetLogPlot(this,axis) \
astINVOKE(V,astGetLogPlot_(astCheckPlot(this),axis))
#define astSetLogPlot(this,axis,logplot) \
astINVOKE(V,astSetLogPlot_(astCheckPlot(this),axis,logplot))
#define astTestLogPlot(this,axis) \
astINVOKE(V,astTestLogPlot_(astCheckPlot(this),axis))

#define astClearLogTicks(this,axis) \
astINVOKE(V,astClearLogTicks_(astCheckPlot(this),axis))
#define astGetLogTicks(this,axis) \
astINVOKE(V,astGetLogTicks_(astCheckPlot(this),axis))
#define astSetLogTicks(this,axis,logticks) \
astINVOKE(V,astSetLogTicks_(astCheckPlot(this),axis,logticks))
#define astTestLogTicks(this,axis) \
astINVOKE(V,astTestLogTicks_(astCheckPlot(this),axis))

#define astClearLogLabel(this,axis) \
astINVOKE(V,astClearLogLabel_(astCheckPlot(this),axis))
#define astGetLogLabel(this,axis) \
astINVOKE(V,astGetLogLabel_(astCheckPlot(this),axis))
#define astSetLogLabel(this,axis,loglabel) \
astINVOKE(V,astSetLogLabel_(astCheckPlot(this),axis,loglabel))
#define astTestLogLabel(this,axis) \
astINVOKE(V,astTestLogLabel_(astCheckPlot(this),axis))

#define astClearTextLab(this,axis) \
astINVOKE(V,astClearTextLab_(astCheckPlot(this),axis))
#define astGetTextLab(this,axis) \
astINVOKE(V,astGetTextLab_(astCheckPlot(this),axis))
#define astSetTextLab(this,axis,textlab) \
astINVOKE(V,astSetTextLab_(astCheckPlot(this),axis,textlab))
#define astTestTextLab(this,axis) \
astINVOKE(V,astTestTextLab_(astCheckPlot(this),axis))

#define astClearLabelUnits(this,axis) \
astINVOKE(V,astClearLabelUnits_(astCheckPlot(this),axis))
#define astGetLabelUnits(this,axis) \
astINVOKE(V,astGetLabelUnits_(astCheckPlot(this),axis))
#define astSetLabelUnits(this,axis,labelunits) \
astINVOKE(V,astSetLabelUnits_(astCheckPlot(this),axis,labelunits))
#define astTestLabelUnits(this,axis) \
astINVOKE(V,astTestLabelUnits_(astCheckPlot(this),axis))

#define astClearStyle(this,axis) \
astINVOKE(V,astClearStyle_(astCheckPlot(this),axis))
#define astGetStyle(this,axis) \
astINVOKE(V,astGetStyle_(astCheckPlot(this),axis))
#define astSetStyle(this,axis,style) \
astINVOKE(V,astSetStyle_(astCheckPlot(this),axis,style))
#define astTestStyle(this,axis) \
astINVOKE(V,astTestStyle_(astCheckPlot(this),axis))

#define astClearFont(this,axis) \
astINVOKE(V,astClearFont_(astCheckPlot(this),axis))
#define astGetFont(this,axis) \
astINVOKE(V,astGetFont_(astCheckPlot(this),axis))
#define astSetFont(this,axis,font) \
astINVOKE(V,astSetFont_(astCheckPlot(this),axis,font))
#define astTestFont(this,axis) \
astINVOKE(V,astTestFont_(astCheckPlot(this),axis))

#define astClearColour(this,axis) \
astINVOKE(V,astClearColour_(astCheckPlot(this),axis))
#define astGetColour(this,axis) \
astINVOKE(V,astGetColour_(astCheckPlot(this),axis))
#define astSetColour(this,axis,colour) \
astINVOKE(V,astSetColour_(astCheckPlot(this),axis,colour))
#define astTestColour(this,axis) \
astINVOKE(V,astTestColour_(astCheckPlot(this),axis))

#define astClearWidth(this,axis) \
astINVOKE(V,astClearWidth_(astCheckPlot(this),axis))
#define astGetWidth(this,axis) \
astINVOKE(V,astGetWidth_(astCheckPlot(this),axis))
#define astSetWidth(this,axis,width) \
astINVOKE(V,astSetWidth_(astCheckPlot(this),axis,width))
#define astTestWidth(this,axis) \
astINVOKE(V,astTestWidth_(astCheckPlot(this),axis))

#define astClearSize(this,axis) \
astINVOKE(V,astClearSize_(astCheckPlot(this),axis))
#define astGetSize(this,axis) \
astINVOKE(V,astGetSize_(astCheckPlot(this),axis))
#define astSetSize(this,axis,size) \
astINVOKE(V,astSetSize_(astCheckPlot(this),axis,size))
#define astTestSize(this,axis) \
astINVOKE(V,astTestSize_(astCheckPlot(this),axis))
#endif
#endif
