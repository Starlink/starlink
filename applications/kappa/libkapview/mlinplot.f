      SUBROUTINE MLINPLOT( STATUS )
*+
*  Name:
*     MLINPLOT

*  Purpose:
*     Draws a multi-line plot of a 2-d NDF's data values against their
*     axis co-ordinates.

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
*     This application takes one dimension of a 2-dimensional NDF as a
*     line index and draws a multi-line plot of the NDF's data values
*     against selected line indices from its other dimension.  Thus one
*     obvious application is the display of 2-dimensional spectra.
*
*     By default, this application selects the first dimension of the
*     NDF as the abscissa of the plot and the second dimension of the
*     NDF as the line index.  However, you can choose the opposite.
*     The vertical axis of the plot is the value of the data lines
*     after offsetting.  The horizontal axis of the plot is the axis
*     co-ordinates of the selected dimension of the NDF.  If the axis
*     co-ordinates are not defined in the NDF, the pixel co-ordinates
*     of that dimension will be used in the plot.  The plot is situated
*     within the current picture on the current graphics device.
*
*     To separate the data lines from each other, there is a choice of
*     three methods by which to offset the lines.  By default, each
*     line is annotated with its line index (the indices of the
*     dimension are selected for this purpose), and the offsets of the
*     lines display in a table.

*  Usage:
*     mlinplot ndf [comp] lnindx ylimit [pltitl] [abslab] [ordlab]
*       [device]

*  ADAM Parameters:
*     ABSLAB = LITERAL (Read)
*        Label for the plot abscissa.  If axis information is present 
*        the suggested default is the NDF's axis label followed by 
*        the units, in parentheses.  If an error occurs obtaining the 
*        label the default is "Pixel co-ordinates". []
*     ABSAXS = _INTEGER (Read)
*        If it is 1, the first significant dimension of the input NDF
*        will be taken as the abscissa of the plot.  If it is 2, the
*        second significant dimension will be taken as the abscissa. [1]
*     CLEAR = _LOGICAL (Read)
*        TRUE if the current picture is to be cleared before the line
*        plot is drawn. [TRUE]
*     COMP = LITERAL (Read)
*        The NDF component to be plotted.  It may be "Data", "Quality",
*        "Variance", or "Error" (where "Error" is the alternative to
*        "Variance" and causes the square root of the variance values
*        to be displayed).  If "Quality" is specified, then the quality
*        values are treated as numerical values (in the range 0 to
*        255). ["Data"]
*     DEVICE = DEVICE (Read)
*        The plotting device. [Current graphics device]
*     FONT = LITERAL (Read)
*        The fount to be used for the line graphics.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the standard
*        GKS san-serif fount.  The former is intended for hardcopy
*        publication-quality plots, since it is relatively slow; the
*        latter is intended for normal interactive graphics requiring
*        rapid plotting, and it is clearer on small plots.  The
*        suggested default is the current value. ["GKS"]
*     KEY = _LOGICAL (Read)
*        When KEY is TRUE a key of the line offsets will be drawn to
*        the right of the line plots.  A maximum of 50 values are shown.
*        When there are more than 50 lines displayed, the frequency of
*        the offsets in the table decreases.  KEY set to TRUE also
*        causes the line numbers to be drawn to the right of the main
*        plot and adjacent to their corresponding lines.  These are
*        present to identify the lines when LINLAB is FALSE or there are
*        more than 26 lines plotted.  They too decrease in frequency
*        when there is insufficient room to accommodate them all.  Their
*        size is controlled by parameter LBSIZE; they are plotted with
*        palette entry 3 when the chosen device supports at least four
*        colours.
*
*        When KEY is FALSE there will be no key or annotations, enabling
*        the plots to be seen at about 40 per cent greater resolution.
*        The value of KEY is ignored when parameter YLOG is TRUE.
*        [TRUE]
*     LBSIZE = _REAL (Read)
*        The text width of the horizontal and vertical axis labels 
*        given as a fraction of the smaller dimension of the display 
*        window.  The value less than or equal to zero means using NCAR 
*        default setting.  The title of the display will have the text 
*        width of 1.2 * LBSIZE, and the numerical label of the axes 
*        will have the text width of 0.8 * LBSIZE for mantissa, and 
*        0.55 * LBSIZE for exponent.  The permitted range is 0.0--0.05. [0.025] 
*     LINLAB = _LOGICAL (Read)
*        If LINLAB is TRUE, the lines in the plot will be interrupted
*        and be labelled by their line indices.  If LINLAB is FALSE
*        the lines will be solid.  There is a maximum of 26 annotated
*        lines.  When there are more lines than 26, all the lines are
*        solid regardless of the value of LINLAB.  The annotations are
*        plotted with palette entry 2 when using a device that supports
*        at least four colours. [TRUE]
*     LNINDX = LITERAL (Read)
*        A comma-separated number string specifies the line-index number
*        to be displayed.  It can take any of the following values:
*
*           "ALL" or "*": All lines 
*
*           "xx,yy,zz": A list of line indices.
*
*           "xx-yy": Line indices between xx and yy inclusively.  When
*                    xx is omitted the range begins from the lower bound
*                    of the line dimension; when yy is omitted the range
*                    ends with the maximum value it can take, that is
*                    the upper bound of the line dimension or the
*                    maximum number of lines this routine can plot.
*
*           Any reasonable combination of above values separated by
*           commas.  A maximum of 100 lines may be selected.  The
*           suggested default is the current value, initially "1-5".
*     MAJTIC( 2 ) = _REAL (Read)
*        The parameter controlling the number of major tick marks for
*        the x and y axes. (Number used is between MAJTIC+2 and
*        5*MAJTIC / 2 + 4 ) [4.0, 4.0]
*     MINTIC( 2 ) = _REAL (Read)
*        The number of minor tick marks between each major tick mark for
*        the x and y axes.  A negative value forces the graphics package
*        to compute appropriate values.  The number of minor tick marks
*        per major tick is fixed ( 8 ) for a logarithmic axis.
*        [-1.0, -1.0]
*     NDF = NDF (Read)
*        NDF structure containing the array to be plotted.
*     OFFSET() = _REAL (Read)
*        When the offset method is specified as "Free", this parameter is
*        obtains the offset values for each locus of data values.
*     ORDLAB = LITERAL (Read)
*        Label for the vertical axis of the plot.  The suggested default
*        is the NDF's label followed by the units, if present, in
*        parentheses.  If an error occurs obtaining the label the
*        default, is the component name followed by " values". []
*     OUTTIC = _LOGICAL (Read)
*        TRUE if the axis tick marks are to appear on the outside of 
*        the axes instead of inside.  By default, the tick marks are
*        drawn inside the plot region. [FALSE]
*     PLTITL = LITERAL (Read)
*        The title of the plot.  Up to about 40 characters can be
*        accommodated, and NCAR fancy founts may be embedded.  The
*        suggested default is the title of the NDF.  If an error occurs
*        obtaining the title, it is defaulted to "Lines plot".  []
*     PXSIZE = _REAL (Read)
*        The horizontal size of the display in metres.  If a value less
*        than the default is requested, the display will appear at
*        the bottom left of the current device.  There is an upper
*        limit given by the x size of the current picture. [Maximum
*        that can fit in the current picture]
*     PYSIZE = _REAL (Read)
*        The vertical size of the display in metres.  If a value less
*        than the default is requested, then the display will appear at
*        the bottom left of the current device.  There is an upper
*        limit given by the y size of the current picture. [Maximum
*        that can fit in the current picture]
*     SPACE = LITERAL (Read)
*        The value of this parameter specifies the method by which
*        the data lines or loci in the plot are offset.  It can be
*        given the values:
*
*        "Free":
*          The offset of each data locus is specified by you.
*
*        "Constant":
*          The base lines of the curves are evenly spaced between upper 
*          and lower limits of the plotting box.  The width of any line-
*          to-line strip is constant, which could result in the loci
*          becoming confused when the biases of some loci from their
*          base lines are so large that these loci lie totally in the 
*          strips of other curves.
*
*        "Average":
*          This method uses an average data value for each locus and
*          produces offsets which ensure that these average data
*          values are equally spaced over the plotting area.  Any line-
*          to-line striping is thus hidden and the amount of overlap of
*          adjacent traces is minimised.
*
*        The input can be abbreviated to an unambiguous length and 
*        is case insensitive. ["Average"]
*     TICLN = _REAL (Read)
*        The length of the major tick marks given in the fraction of the
*        small dimension of the plot box.  Its value should be within
*        range 0.0--0.05.  A value outside this range means using NCAR
*        default.  The minor tick marks will have the length 0.66*TICLN.
*        [0.015]
*     XLOG = _LOGICAL (Read)
*        TRUE if the abscissa is to be logarithmic.  It is unlikely that
*        you would want to do this. [FALSE] 
*     YLIMIT( 2 ) = _REAL (Read)
*        Used to get the lower and upper vertical display limits.  The
*        suggest default lower limit is the minimum value of the bottom
*        line in the display.  The default upper limit is such that
*        no line will ever overlap.
*     YLOG = _LOGICAL (Read)
*        TRUE if the vertical axis is to be logarithmic.  This is useful
*        when the data have a wide dynamic range.  In order to
*        discriminate between the lines, the lines are plotted using
*        the first four pens in a cyclic fashion.  Note that no key is
*        drawn when YLOG is TRUE. [FALSE]

*  Examples:
*     mlinplot rcw3_b1 reset \
*        Plot the first five lines of the 2-dimensional NDF file,
*        rcw3_b1, against its first significant dimension on the
*        current graphics device.  The data co-ordinate will be in
*        pixels if rcw3_b1 does not have an axis component.  The lines
*        are offset such that the averages of the lines are evenly
*        separated in the direction of the vertical axis.
*     mlinplot rcw3_b1 lnindx="1,3,5,7-10" \
*        Plot the lines 1, 3, 5, 7, 8, 9 and 10 of the 2-dimensional
*        NDF file, rcw3_b1, against its first significant dimension on
*        the current graphics device.
*     mlinplot rcw3_b1 lnindx=* \
*        Plot all lines of the 2-dimensional NDF file, rcw3_b1, against
*        its first significant dimension on the current graphics
*        device.
*     mlinplot rcw3_b1 absaxs=2 lnindx="20-25,30,31" \
*        Plot lines 20, 21, 22, 23, 24, 25, 30 and 31 of the
*        2-dimensional NDF file, rcw3_b1, against its second
*        significant dimension on the current graphics device.
*     mlinplot rcw3_b1 pltitl="CRDD rcw3_b1" \
*        Plot the currently selected lines of the 2-dimensional NDF
*        file, rcw3_b1, against its first significant dimension on the
*        current graphics device.  The plot has a title of "CRDD
*        rcw3_b1".
*     mlinplot rcw3_b1(100.0:500.0,) ylimit=[0.0,1.0E-3] \
*        Plot the currently selected lines of the 2-dimensional NDF
*        file, rcw3_b1, against its first significant dimension within
*        co-ordinates 100.0 to 500.0.  The vertical display range is
*        from 0.0 to 1.0E-3.
*     mlinplot rcw3_b1 space=constant device=ps_p \
*        Plot the currently selected lines of the 2-dimensional NDF
*        file, rcw3_b1, against its first significant dimension on the
*        ps_p device.  The base lines of them are evenly distributed in
*        the range of vertical axis.
*     mlinplot rcw3_b1 space=free offset=[0.,2.0E-4,4.0E-4,6.0E-4,0.1] \
*        Plot the currently selected lines of the 2-dimensional NDF
*        file, rcw3_b1, against its first significant dimension.  The
*        base lines are set at 0.0 for the first line, 2.0E-4 for the
*        second, 4.0E-4 for the third, 6.0E-4 for the fourth and 0.1
*        for the fifth.

*  Notes:
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME of the specified size
*     containing the title, annotated axes, and line plot; and a DATA
*     picture, which has world co-ordinates for linear axes measured in
*     pixels along the x axis and data values along y, and their
*     logarithms if a logarithmic axis is selected.  The DATA picture
*     also has data co-ordinates stored; for a linear axis this
*     requires that the NDF's axis units are not pixel co-ordinates;
*     for a logarithmic axis the actual data co-ordinate or value is
*     recorded.  If there is no NDF axis information and a logarithmic
*     abscissa, the DATA co-ordinates are pixel co-ordinates.  The NDF
*     associated with the plot is stored by reference with the DATA
*     picture.  On exit the current database picture for the chosen
*     device reverts to the input picture.
*     -  In a logarithmic plot only positive data along each
*     logarithmic axis can be displayed, therefore non-positive data
*     are excluded.  A logarithmic axis will always increase from left
*     to right, and from bottom to top.
*     -  Bad pixels appear as gaps in the plot, and they do not affect
*     the limits of the ordinate.  The same applies to zero or negative
*     data values if the plot is to have a logarithmic ordinate.
*     -  On colour graphics devices the actual colours used for
*     different portions of the plot may be adjusted using the PAL*
*     commands.

*  Algorithm:
*     -  Find which component to display, obtain an identifier to the
*     NDF and check that the component is present.  Find the data type
*     for processing.  Get the NDF bounds, the number of significant
*     dimensions and inquire the bad-pixel flag.  Determine which
*     co-ordinate system is to be used.
*     -  Ascertain the type of axes requested.
*     -  Get the bounds of the slice in the significant dimension
*     between define bound limits.  Convert from data co-ordinates if
*     necessary.  Allow for the logarithmic x axis by constraining the
*     bounds to be positive. Create and map the slice.
*     -  Obtain the plot title and axis labels from the NDF where
*     that is possible.
*     -  Obtain the ordinate limits, if required, and the remaining
*     plot attributes.
*     -  Get the display device and open the database for it with the
*     appropriate access mode. Get the current SGS zone.
*     -  Create the frame picture and store it in the database.
*     -  For data co-ordinates obtain the axis co-ordinates.  Make a
*     smaller the section if there are negative co-ordinates in a
*     logarithmic plot.  If using world co-ordinates also make a smaller
*     section if there are negative bounds.  Then get some work
*     space which is filled with pixel co-ordinates.
*     -  Flag the data for a logarithmic plot by setting non-positive
*     data to the NCAR bad-datum flag.  Do likewise replacing any bad
*     pixels.
*     -  Draw the line plot.  Record it as the data picture in the
*     database.  Record the NDF reference in the database.  Store
*     the transformations from world to data co-ordinates evaluating
*     a scale and offset except for logarithmic axes when there is
*     no NDF axis structure, or the abscissa is logarithmic and
*     the data co-ordinate system is plotted.
*     -  Tidy any workspace used, the database and the NDF system.

*  Related Applications:
*     KAPPA: INSPECT, LINPLOT; Figaro: ESPLOT, IPLOTS, MSPLOT, SPLOT;
*     SPECDRE: SPECGRID.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     QUALITY, LABEL, TITLE, and UNITS components of the NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Only
*     single-precision floating-point data can be processed directly.
*     Other non-complex data types will undergo a type conversion
*     before the line plot is drawn.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-APR-1991 (WG):
*        Original version.
*     1991 June 20 (MJC):
*        Added world and data co-ordinate systems. Completed the Usage
*        Examples, Notes and Implementation Status in the prologue plus
*        corrected other typo's.  Renamed or replaced some of the
*        routine calls.  Default line numbers constructed here. Fixed a
*        couple of bugs.  Moved error reports to subroutines.
*     1991 June 23 (WG):
*        Added parameters to control line labelling and text height.
*        Cycle the pen numbers for the lines in a logarithmic-ordinate
*        plot.
*     1991 July 7 (MJC and WG):
*        Allowed for a larger number of lines.  Only draw offset lines
*        when there are less than 27 data lines.  Only draw the offset
*        table when there are less than 51 data lines.  Increased margin
*        between plot and table for longer line numbers.  NCAR settings
*        saved and restored.
*     1991 July 31 (MJC):
*        No longer redefines colours of SGS pens to predefined state if
*        workstation has dynamic colour representation, now there is
*        palette control.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 23 (MJC):
*        Removed XLIMIT parameter.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     1993 February 12 (MJC):
*        For large numbers of lines the key line annotations and key
*        offsets are plotted at lower frequency.  Added KEY parameter.
*        Corrected omissions in the documentation.
*     1995 October 19 (MJC):
*        Supports Error component.
*     23-JUN-1998 (DSB):
*        Used KPG1_MAP instead of NDF_MAP, so that NaN and Inf values
*        are converted to Starlink BAD values before being used.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT definitions
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'NDF_ERR'          ! NDF_ error definitions
      INCLUDE 'IRM_COM'          ! IRM constants
                                
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL SNX_AGGUX
      REAL SNX_AGGUX             ! Converts NCAR x grid co-ordinate to
                                 ! world co-ordinate
      EXTERNAL SNX_AGGUY
      REAL SNX_AGGUY             ! Converts NCAR y grid co-ordinate to
                                 ! world co-ordinate
      REAL SNX_AGUGY             ! Convert the user coordinate to
                                 ! NCAR grid coordinate 
      INTEGER CHR_LEN            ! Used length of a character string

*  Local Constants:
      INTEGER NDIM               ! The dimension of NDF this routine can
      PARAMETER ( NDIM = 2 )     ! handle
      INTEGER MAXPAT             ! The maximum number of line patterns
      PARAMETER ( MAXPAT = 26 )  ! the routine can plot at one time
                                 ! (imposed by NCAR)
      REAL TEXTHT                ! The default height of the text in the
                                 ! plot
      PARAMETER ( TEXTHT = 0.025 )

*  Local Variables:
      INTEGER ABSAXS             ! Flag showing which dimension of NDF
                                 ! will be plotted as abscissa
      INTEGER ANNOIN             ! Frequency of annotations and values
                                 ! in the offset table
      INTEGER AXSPEN             ! Pen number used to draw axes
      CHARACTER*( 72 ) ABSLAB    ! Label of abscissa
      INTEGER ACTDIM             ! Actual dimension of the input NDF
      CHARACTER*( NDF__SZTYP ) ATYPE ! Processing type of the axis
                                 ! centres
      LOGICAL BAD                ! Flag showing data contain bad sample
      CHARACTER*( 5 ) CINDX      ! Line indices string
      LOGICAL CLEAR              ! Flag showing clear graphic surface
      REAL CLIP( 1 )             ! Clip value when get average of lines
      CHARACTER*( 5 ) CLONUM     ! String of low-limit of default list
      LOGICAL COLOUR             ! Graphic colour flag
      CHARACTER*( 31 ) COMLIS    ! List of the NDF component
      INTEGER COMLN              ! The used length of COMLIS
      CHARACTER*( 8 ) COMP       ! The component of NDF to plot
      CHARACTER*( 5 ) COSYS      ! Co-ordinate system
      CHARACTER*( 5 ) CUPNUM     ! String of up-limit of default list
      LOGICAL DACOOR             ! Data co-ordinates are to be stored
                                 ! in the database
      LOGICAL DATACO             ! Axes are given in data co-ordinates
      CHARACTER*( 20 ) DEFLIS    ! Default list of the selection
      INTEGER DEFLN              ! Used length of string DEFLIS
      REAL DEFMAJ( 2 )           ! Default number of major tick mark
      REAL DEFMIN( 2 )           ! Default number of minor tick mark
      INTEGER DIMABS             ! Dimension number of absscia
      INTEGER DIMIND             ! Dimension number of line index
      INTEGER DLNIND( MCM__MXCUR ) ! Indices of the lines to plot w.r.t.
                                 ! an origin of 1
      DOUBLE PRECISION DOFSET( 2 ) ! Offsets in the world-to-data
                                 ! transformations
      LOGICAL DPAXIS             ! Axis centres are double precision
      DOUBLE PRECISION DSCALE( 2 ) ! Scale factors in the
                                 ! world-to-data co-ordinate
                                 ! transformations
      CHARACTER*( NDF__SZFTP ) DTYPE
                                 ! Type of image after plot (not used)
      DOUBLE PRECISION DXLMT( 2 )! The lower and upper limits of x axis
      INTEGER EL                 ! Number of element of a mapped NDF
      CHARACTER*( 4 ) FOUNT      ! Fount type
      REAL HEAP( 2700 )          ! Buffer to store NCAR settings
      INTEGER I                  ! Do loop index
      INTEGER IARY               ! Identifier of a temporary array
      INTEGER IEL                ! Number of element of a temporary
                                 ! array 
      REAL INLBPS( MCM__MXCUR )  ! In-line label position
      CHARACTER*( 12 ) INLAB( MCM__MXCUR )
                                 ! In-line label for each line
      LOGICAL INLIN              ! If true, lines will be labelled
      INTEGER INLPEN( MCM__MXCUR ) ! Pen number used for each in-line
                                 ! label
      INTEGER IPLACE             ! Placeholder of a temporary array
      INTEGER IPNTR( 1 )         ! Pointer to a temporary array
      CHARACTER*( NDF__SZTYP ) ITYPE
                                 ! Processing type of the image
      LOGICAL KEY                ! If true a key is plotted
      INTEGER LABPEN             ! Pen number of axis label and title
      INTEGER LBND( NDF__MXDIM ) ! Lower bound of each dimension of NDF
      REAL LBSIZE                ! The size of the axis labels
      INTEGER LINPEN( MCM__MXCUR ) ! Pen number for each line
      INTEGER LNINDX( MCM__MXCUR ) ! Indices of the lines to plot
      LOGICAL LWORK              ! Workspace for line indices or the
                                 ! obtained
      REAL MAJTIC( 2 )           ! Number of major tick marks
      CHARACTER*( 8 ) MCOMP      ! Component to be mapped
      REAL MINTIC( 2 )           ! Number of minor tick marks
      LOGICAL MONOTO             ! Axis monotonic flag
      INTEGER NCHAR              ! Used length of a offset talbe item
      INTEGER NCINDX             ! Used length of string CINDX
      INTEGER NCLIP              ! Number of clip when get average value
      INTEGER NCOLS              ! Number of colours available on device
      INTEGER NDF                ! The identifier of the input NDF
      INTEGER NDFS               ! The identifier of NDF section
      INTEGER NDISP              ! Number of lines to plot
      INTEGER NLBPEN             ! Pen number used to draw numeric label
      INTEGER NLONUM             ! Used length of string CLONUM
      INTEGER NSMP               ! Number of samples in the plot
      INTEGER NUPNUM             ! Used length of string CUPNUM
      INTEGER NVAL( MCM__MXCUR ) ! Number of valid samples in each line
      INTEGER OFMTHD             ! Offset method code
      REAL OFFGY( MCM__MXCUR )   ! Grid ordinate of offsets of lines
      INTEGER OFFIN              ! Frequency of values in the offset
                                 ! table
      REAL OFFLEN                ! Length of offset marks  
      REAL OFFSET( 2 )           ! Offsets in the world-to-data
                                 ! transformations
      REAL LINOFS( MCM__MXCUR )  ! The offset of each line
      CHARACTER*( 15 ) OFFTAB( 3 * MCM__MXCUR + 3 )
                                 ! Offset table
      CHARACTER*( 72 ) ORDLAB    ! Label of ordinate
      LOGICAL OUTTIC             ! Flag showing tick mark outside plot
      INTEGER PEN2               ! The pen number of the second pen
      INTEGER PICID1             ! Identifier of picture on entering AGI
      INTEGER PICID2             ! FRAME picture identifier
      INTEGER PICID3             ! Identifier of DATA picture
      CHARACTER*( 72 ) PLTITL    ! Title of the plot
      INTEGER PNTR( 1 )          ! Pointer to the array
      CHARACTER*( DAT__SZLOC ) PXLOC ! Locator for scratch area for
                                 ! pixel co-ordinates
      INTEGER PXPNTR( 1 )        ! Pointer to axis of NDF
      LOGICAL PWORK              ! Workspace for pixel co-ordinates
                                 ! obtained
      REAL SCALE( 2 )            ! Scale factors in the
                                 ! world-to-data co-ordinate
                                 ! transformations
      REAL SGMA( MCM__MXCUR )    ! Standard deviation of each line
      INTEGER SIGDIM( NDF__MXDIM )
                                 ! The significant dimension of NDF
      LOGICAL SOLID              ! Flag to show whether to draw curves
                                 ! in solid line
      REAL TABBOT                ! The posit. of bottom of offset table
      LOGICAL THERE              ! Flag showing a component exists
      REAL TICLN                 ! The length of major tick marks
      INTEGER TICPEN             ! Pen number used tp draw tick marks
      INTEGER TITLN              ! Used length of PLTITL
      INTEGER TITPEN             ! Pen number used to write the title
      REAL X1, X2, Y1, Y2        ! Extension of plot box
      INTEGER XBOUND( 2 )        ! Pixel indices of abscissa bounds
      REAL XLMT( 2 )             ! The lower and upper limits of x axis
      LOGICAL XLOG               ! Flag showing X axis is logarithmic
      INTEGER XORDER             ! Order of the abscissa: 0 normal, 1
                                 ! flipped
      REAL YLMT( 2 )             ! The lower and upper limits of y axis
      LOGICAL YLOG               ! Flag showing Y axis is logarithmic
      REAL YMEAN( MCM__MXCUR )   ! Average value of each line in plot
      REAL YMN( MCM__MXCUR )     ! Min. value of each line in plot
      REAL YMX( MCM__MXCUR )     ! Max. value of each line in plot
      INTEGER UBND( NDF__MXDIM ) ! Upper bound of each dimension of NDF
      INTEGER UBDTMP( 2 )        ! Upper bound of temporay array
      INTEGER ZONE1              ! Initial SGS zone identifier
      INTEGER ZONEF              ! SGS frame-zone identifier
      INTEGER ZONEI              ! SGS data-zone identifier

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise the flags to say that no workspace has been obtained.
*    They are used later for tidying.

      PWORK = .FALSE.
      LWORK = .FALSE.

*  Obtain the NDF component.
*  =========================

*  Begin an NDF context, and obtain the identifier of the NDF to be
*  ploted.       
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'NDF','READ', NDF, STATUS )

*  Form the component list of the input NDF. Data array component must
*  exist for the file to be an NDF.
      COMLIS = 'Data'
      COMLN = 4
      
*  If the Quality component exists, append it to component list.
      CALL NDF_STATE( NDF, 'Quality', THERE, STATUS )
      IF ( THERE ) THEN
         CALL CHR_APPND( ','//'Quality', COMLIS, COMLN )
      END IF

*  If the Variance component exists, append it to component list.
      CALL NDF_STATE( NDF, 'Variance', THERE, STATUS )
      IF ( THERE ) THEN
         CALL CHR_APPND( ','//'Error,Variance', COMLIS, COMLN )
      END IF

*  Find which component to plot.
      CALL PAR_CHOIC( 'COMP', 'Data', COMLIS( :COMLN ), .FALSE., COMP,
     :                 STATUS )

*  Most NDF routines with a component argument don't recognise 'ERROR',
*  so we need two variables.  Thus convert 'ERROR' into 'VARIANCE' in
*  the variable needed for such routines.  The original value is held
*  in a variable with the prefix M for mapping, as one of the few
*  routines that does support 'ERROR' is NDF_MAP; it is also needed for
*  plot annotations using any NDF units.
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  This application can only process real components directly. For the
*  given type of the image find in which type it should be processed.
*  It may still be possible to handle d.p. data provided the dynamic
*  range is not too small.
      CALL ERR_MARK
      CALL NDF_MTYPE( '_REAL', NDF, NDF, COMP, ITYPE, DTYPE, STATUS )
      IF ( STATUS .EQ. NDF__TYPNI ) THEN
         CALL ERR_FLUSH( STATUS )
         CALL MSG_OUT( 'PRECLOSS', 'The loss of precision may not be '/
     :     /'serious so continuing to process in _REAL.', STATUS )
         ITYPE = '_REAL'
      END IF
      CALL ERR_RLSE

*  Ensure that the number of significant dimensions is correct.
      CALL KPG1_SGDIM( NDF, NDIM, SIGDIM, STATUS )

*  Get the bounds of the NDF
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, ACTDIM, STATUS )

*  Find which significant dimension will be regarded as abscissa
      CALL PAR_GET0I( 'ABSAXS', ABSAXS, STATUS )
      
*  Use shorthands for abscissa dimension and index dimension.
      IF ( ABSAXS .EQ. 1 ) THEN
         DIMABS = SIGDIM( 1 )
         DIMIND = SIGDIM( 2 )
      ELSE
         DIMABS = SIGDIM( 2 )
         DIMIND = SIGDIM( 1 )
      END IF

*  Get the type of co-ordinates to place on axes.
*  ==============================================

*  Is there an axis system?

      CALL NDF_STATE( NDF, 'Axis', DACOOR, STATUS )

*  Obtain the desired co-ordinate system.

      CALL PAR_CHOIC( 'COSYS', 'Data', 'Data,World', .FALSE., COSYS,
     :                STATUS )

*  Find the effective co-ordinate system.

      DATACO = DACOOR .AND. COSYS .EQ. 'DATA'

*  Find the implementation type of the axis structure.
*  ===================================================

*  Integer needs d.p. because it potentially has ten significant digits.
*  The implementation type is only required when there is an axis
*  structure present.

      DPAXIS = .FALSE.
      IF ( DACOOR ) THEN
         CALL NDF_ATYPE( NDF, 'Centre', DIMABS, ATYPE, STATUS )
         IF ( ATYPE .EQ. '_INTEGER' .OR. ATYPE .EQ. '_DOUBLE' ) THEN
            ATYPE = '_DOUBLE'

*          Initialise the flag to indicate the type.

            DPAXIS = .TRUE.

         ELSE
            ATYPE = '_REAL'
            DPAXIS = .FALSE.
         END IF
      END IF

*  If an error happened, report it and exit.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Obtain the indices of lines to plot.
*  ====================================

*  Get a temporary array for use when selecting the lines to display.
      UBDTMP( 1 ) = UBND( DIMIND ) - LBND( DIMIND ) + 1
      CALL ARY_TEMP( IPLACE, STATUS )
      CALL ARY_NEWP( '_INTEGER', 1, UBDTMP, IPLACE, IARY, STATUS )
      CALL ARY_MAP( IARY, '_INTEGER', 'WRITE', IPNTR, IEL, STATUS )
      LWORK = .TRUE.

*  If an error happened, report it and exit.
      IF ( STATUS .NE. SAI__OK ) GOTO 970

*  Construct the default numbers. Set low-limit of selectable 
*  numbers as the low-limit of the default list.
      CALL CHR_ITOC( LBND( DIMIND ), CLONUM, NLONUM )

*  If the total number of selectable numbers is less then 5, set
*  up-limit of selectable number as the upper limit of default list.
      IF ( UBND( DIMIND ) - LBND( DIMIND ) .LE. 5 ) THEN
         CALL CHR_ITOC( UBND( DIMIND ), CUPNUM, NUPNUM )
      
*  If the total number is more than 5, set LBND( DIMIND ) + 4 as upper
*  limit of default list.
      ELSE
         CALL CHR_ITOC( LBND( DIMIND ) + 4, CUPNUM, NUPNUM )
      END IF

*  Set the default value for the parameter.
      DEFLIS = CLONUM( : NLONUM )//'-'//CUPNUM( : NUPNUM )
      DEFLN = NLONUM + NUPNUM + 1
      CALL PAR_DEF0C( 'LNINDX', DEFLIS( : DEFLN ), STATUS )
      
*  Get the indices of the lines to be displayed.
      CALL KPG1_GILST( LBND( DIMIND ), UBND( DIMIND ), MCM__MXCUR,
     :                 'LNINDX', %VAL( IPNTR( 1 ) ), LNINDX, NDISP,
     :                 STATUS )
      
*  Release the temporary work space.
      CALL ARY_ANNUL( IARY, STATUS )
      LWORK = .FALSE.
            
*  If an error occurred, exit.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  If the number of lines to plot is 0, report and exit.
      IF ( NDISP .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'MLINPLOT_NULIN',
     :     'MLINPLOT: There is no line to plot.', STATUS )
         GOTO 980
      END IF

*  Obtain the bounds of the abscissa.
*  ==================================

*  Map the axis centres in double precision as the bounds will also
*  be used to store a more-precise transformation in the graphics
*  database.
      CALL NDF_AMAP( NDF, 'Centre', DIMABS, '_DOUBLE', 'READ', PXPNTR,
     :               EL, STATUS )

*  Is the axis monotonic?  Start a new error context so that the error
*  reports concerning a non-monotonic axis may be annulled.  Instead we
*  issue a warning message so that the application can continue by
*  using world co-ordinates.
      CALL ERR_MARK
      CALL KPG1_MONOD( .TRUE., EL, %VAL( PXPNTR( 1 ) ), MONOTO, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         MONOTO = .FALSE.
      END IF
      CALL ERR_RLSE

*  Issue the warning.  Change the emphasis depending on whether the
*  co-ordinate system is DATA.
      IF ( .NOT. MONOTO ) THEN
         CALL MSG_SETI( 'IAXIS', DIMABS )
         IF ( DATACO ) THEN
            CALL MSG_OUT( 'MLINPLOT_NOTMONO1',
     :        'MLINPLOT: Abscissa axis ^IAXIS is not monotonic.  Will '/
     :        /'use world co-ordinates instead.', STATUS )
         ELSE
            CALL MSG_OUT( 'MLINPLOT_NOTMONO2',
     :        'MLINPLOT: Abscissa axis ^IAXIS is not monotonic.  Will '/
     :        /'not record axis bounds in the graphics database.',
     :        STATUS )
         END IF

*  Reset the co-ordinate system and axis-present flags.
         DATACO = .FALSE.
         DACOOR = .FALSE.
      END IF

*  Get the horizontal limits of the display as distinct pixel-index
*  bounds, and their corresponding axis values (when data co-ordinates
*  are required).  The bounds are positive for a logarithmic plot axis.
*  This routine assumes a monotonic axis.
      CALL KPS1_LIXLM( LBND( DIMABS ), UBND( DIMABS ),
     :                 %VAL( PXPNTR( 1 ) ), DATACO, XLOG, XBOUND,
     :                 DXLMT, STATUS )

*  Single precision is required for GKS co-ordinates.
      XLMT( 1 ) = REAL( DXLMT( 1 ) )
      XLMT( 2 ) = REAL( DXLMT( 2 ) )
      
*  Unmap the original axis centre since it's no use now.
      CALL NDF_AUNMP( NDF, 'Centre', DIMABS, STATUS )

*  Define and create the new section.
*  ==================================

*  Define the new bounds to create the sub-array. 
      LBND( DIMABS ) = XBOUND( 1 )
      UBND( DIMABS ) = XBOUND( 2 )

*  Create the sub-array.   
      CALL NDF_SECT( NDF, ACTDIM, LBND, UBND, NDFS, STATUS ) 

*  Map the array of the NDF section.
      CALL KPG1_MAP( NDFS, MCOMP, ITYPE, 'READ', PNTR, EL, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Obtain the plot title and axis labels.
*  ======================================

*  Get the plot title.
      CALL KPG1_GNTIT( NDF, 'PLTITL', 'Lines Plot', PLTITL, STATUS )

*  Remove the leading blank and get the used length of the title.
      CALL CHR_LDBLK( PLTITL )
      TITLN = CHR_LEN( PLTITL )

      IF ( DATACO ) THEN

*  Get the abscissa label suggesting the value in the NDF axis
*  structure, if present, as the default.

         CALL KPG1_GAXLB( NDF, DIMABS, 'ABSLAB', 'Pixel co-ordinates',
     :                    ABSLAB, STATUS )
      ELSE

*  Get the abscissa label without consulting the NDF's axis structure
*  to prevent the wrong label being associated with the world
*  co-ordinates.

         CALL PAR_DEF0C( 'ABSLAB', 'Pixel co-ordinates', STATUS )
         CALL PAR_GET0C( 'ABSLAB', ABSLAB, STATUS )
      END IF

*  Get the ordinate label suggesting the value in the NDF label
*  and units components, if present, as the default, otherwise
*  the component name followed by values is used.
      CALL KPG1_GNLBU( NDF, 'ORDLAB', MCOMP, ORDLAB, STATUS )

*  Obtain the axis characteristics.
*  ================================

*  Find if the axes are to be plotted logarithmically.
      CALL PAR_GET0L( 'XLOG', XLOG, STATUS )
      CALL PAR_GET0L( 'YLOG', YLOG, STATUS )

*  Get the number of major and minor tick marks if either axis is not
*  logarithmic.
      IF ( .NOT. ( XLOG .AND. YLOG ) ) THEN
         DEFMIN( 1 ) = -1.0
         DEFMIN( 2 ) = -1.0
         CALL PAR_GDR1R( 'MINTIC', 2, DEFMIN, VAL__MINR, VAL__MAXR,
     :                   .FALSE., MINTIC, STATUS )
         DEFMAJ( 1 ) = 4.0
         DEFMAJ( 2 ) = 4.0
         CALL PAR_GDR1R( 'MAJTIC', 2, DEFMAJ, VAL__MINR, VAL__MAXR,
     :                   .FALSE., MAJTIC, STATUS )
      END IF 

*  See whether the tick marks on the outside of the axes.
      CALL PAR_GET0L( 'OUTTIC', OUTTIC, STATUS )

*  Get the fount.  Although NCAR is the default, either must be
*  selected to prevent persistence from earlier invocations.
      CALL PAR_CHOIC( 'FONT', 'GKS', 'GKS,NCAR', .TRUE., FOUNT, STATUS )
      IF ( FOUNT .EQ. 'GKS ' ) THEN
         CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )
      ELSE IF ( FOUNT .EQ. 'NCAR' ) THEN
         CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
      END IF

*  Get the length of the tick marks
      CALL PAR_GDR0R( 'TICLN', 0.015, 0.0, 0.05, .TRUE., TICLN, STATUS )

*  See whether or not a key is required.  By definition there is no key
*  for a logarithmic ordinate.
      IF ( YLOG ) THEN
         KEY = .FALSE.
      ELSE
         CALL PAR_GTD0L( 'KEY', .TRUE., .TRUE., KEY, STATUS )
      END IF

*  If an error happened, exit.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Extract the lines required from the NDF.
*  ========================================

*  Get the temporary work space to hold the data to be plotted.
      NSMP = UBND( DIMABS ) - LBND( DIMABS ) + 1
      UBDTMP( 1 ) = NSMP
      UBDTMP( 2 ) = NDISP
      CALL ARY_TEMP( IPLACE, STATUS )
      CALL ARY_NEWP( ITYPE, 2, UBDTMP, IPLACE, IARY, STATUS )
      CALL ARY_MAP( IARY, ITYPE, 'WRITE', IPNTR, IEL, STATUS )
      LWORK = .TRUE.
      IF ( STATUS .NE. SAI__OK ) GOTO 970

*  Reset the line indices to an origin of 1.
      DO I = 1, NDISP
         DLNIND( I ) = LNINDX( I ) - LBND( DIMIND ) + 1
      END DO

*  Put the data to be displayed in the temporary work space.
      CALL KPS1_MLPUT( UBND( SIGDIM( 1 ) ) - LBND( SIGDIM( 1 ) ) + 1, 
     :                 UBND( SIGDIM( 2 ) ) - LBND( SIGDIM( 2 ) ) + 1, 
     :                 %VAL( PNTR( 1 ) ), NSMP, NDISP, DLNIND, ABSAXS, 
     :                 YLOG, %VAL( IPNTR( 1 ) ), STATUS )
 
*  Unmap the sub-array since it will not be used later.
      CALL NDF_UNMAP( NDFS, COMP, STATUS )

*  Define the ordinate limits.
*  ===========================

*  Derive the statistics of the lines to be plotted with 1 clip and 
*  clip threshold as 3 times of the data variance.
      NCLIP = 1
      CLIP( 1 ) = 3.0
      CALL IRM_STATS( 1, NSMP, 1, NDISP, %VAL( IPNTR( 1 ) ),
     :                NCLIP, CLIP, .FALSE., .TRUE., .FALSE., YMX, YMN, 
     :                YMEAN, SGMA, NVAL, STATUS )

*  If any line has its valid samples less than the number of the
*  samples, the array to be plotted contains bad samples.
      BAD = .FALSE.
      DO I = 1, NDISP
         IF( NVAL( I ) .LT. NSMP ) BAD = .TRUE.
      END DO

*  Get the vertical limits of the display.
      CALL KPS1_MLYLM( NDISP, YMX, YMN, YLOG, 'YLIMIT', YLMT, STATUS )

*  Obtain and apply offsets to lines.
*  ==================================

*  If the vertical axis is linear, offset the lines.
      IF ( .NOT. YLOG ) THEN

*  Get the method to offset the lines.
         CALL KPS1_MLGOF( 'SPACE', OFMTHD, STATUS )

*  Calculate the offset according to the specified method.
         CALL KPS1_MLCOF( OFMTHD, NDISP, YLMT, YMEAN, 'OFFSET', LINOFS, 
     :                    STATUS )

*  Offset the lines.
         CALL KPS1_MLOFL( NSMP, NDISP, LINOFS, %VAL( IPNTR( 1 ) ), 
     :                    STATUS )
      END IF

*  Start the graphics system.
*  ==========================

*  See whether picture is to be refreshed or not.

      CALL PAR_GTD0L( 'CLEAR', .TRUE., .TRUE., CLEAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  The data are now ready for plotting.  Associate a graphics device in
*  the database and obtain the zone that matches the current picture.

      IF ( CLEAR ) THEN
         CALL AGS_ASSOC( 'DEVICE', 'WRITE', ' ', PICID1, ZONE1, STATUS )
      ELSE
         CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONE1,
     :                   STATUS )
      END IF

*  Determine whether the device supports sufficient colours.
*  =========================================================

*  First see if it supports any colour.
      CALL KPG1_QCOL( COLOUR, STATUS )

*  Now check that it has at least five (background plus four) pens.
      IF ( COLOUR ) THEN
         CALL KPG1_QNCOL( NCOLS, STATUS )
         COLOUR = COLOUR .AND. NCOLS .GT. 4
      END IF

*  Create the frame picture.
*  =========================

      CALL KPG1_FRPIC( 'PXSIZE', 'PYSIZE', 'KAPPA_MLINPLOT', .FALSE.,
     :                 ZONEF, PICID2, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*  Reset the input picture as current in case of an accident.

      CALL AGI_SELP( PICID1, STATUS )

*  Get AUTOGRAPH to use the SGS zone.

      CALL SNX_AGWV

*    Obtain the axis co-ordinates.
*    =============================

      IF ( DATACO ) THEN

*       Use data co-ordinates.
*       ======================

*       Map axis centres this time in single precision for the NCAR/GKS
*       co-ordinate system.

         CALL NDF_AMAP( NDFS, 'Centre', DIMABS, '_REAL', 'READ',
     :                  PXPNTR, EL, STATUS )
      ELSE

         XLMT( 1 ) = REAL( LBND( DIMABS ) ) - 0.5
         XLMT( 2 ) = REAL( UBND( DIMABS ) ) - 0.5

*       Create the pixel axis annotations.
*       ==================================

*       Obtain workspace.  Note that the same pointer is used as for
*       data co-ordinates.

         CALL AIF_GETVM( '_REAL', 1, EL, PXPNTR( 1 ), PXLOC, STATUS )
         PWORK = .TRUE.

*       Fill the array with pixel co-ordinates.

         CALL KPG1_SSCOF( EL, 1.0D0, DBLE( LBND( DIMABS ) ) - 0.5D0,
     :                    %VAL( PXPNTR( 1 ) ), STATUS )
      END IF

*  Store the NCAR settings.
*  ========================
      CALL SNX_AGSAV( HEAP )

*  Set the extent of the plot box alias the DATA picture.
*  ======================================================
      X1 = 0.13
      IF ( KEY ) THEN
         X2 = 0.72
      ELSE
         X2 = 0.96
      END IF
      Y1 = 0.14
      Y2 = 0.91

*  Define label attributes.
*  ========================

*  Get the size of the axis labels.
      CALL PAR_GDR0R( 'LBSIZE', TEXTHT, 0.0, 0.05, .TRUE., LBSIZE,
     :                STATUS )

*  See if the in-line labels will appear in the display.
      CALL PAR_GTD0L( 'LINLAB', .TRUE., .TRUE., INLIN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 960

*  Find the frequency of the line annotations and the table offsets.
*  The latter is governed by the maximum number of line patterns.
      IF ( KEY ) OFFIN = NDISP / 51 + 1
      IF ( INLIN ) THEN
         ANNOIN = INT( REAL( NDISP ) / REAL( MAXPAT + 1 ) * LBSIZE /
     :                 TEXTHT ) + 1
      ELSE
         ANNOIN = 1
      END IF

*  Construct the in-line labels and set in-line label position if it
*  will appear in the display.
      DO I = 1, NDISP
         IF ( INLIN ) THEN
            INLBPS( I ) = 0.1

*  If in-line label will not appear, set the in-line label position 
*  outside the display window.
         ELSE
            INLBPS( I ) = -1.0
         END IF
         CALL CHR_ITOC( LNINDX( I ), CINDX, NCINDX )
         INLAB( I ) = '#'//CINDX( : NCINDX )
      END DO

*  Draw the plot, setting the colours of its components.
*  =====================================================

*  If colour is available, draw the curves in the plot in solid line.
      IF ( COLOUR ) THEN
         SOLID = .TRUE.

*  Otherwise let the pen number and graphic device decide the line 
*  type of the curves in the display.
      ELSE
         SOLID = .FALSE.
      END IF

*  Set the colour of the lines as white if the y axis is linear. 
      IF ( .NOT. YLOG ) THEN
         DO I = 1, NDISP
            LINPEN( I ) = 1
         END DO

*  If the y axis is logarithmic, plot the lines using first four pens
*  in a cyclic fashion to distinguish the lines.
      ELSE
         DO I = 1, NDISP
            LINPEN( I ) = MOD( I - 1, 4 ) + 1
         END DO
      END IF

*  Set the pen used to draw axes and in-line label as green if colour 
*  is available on the graphics device.
      IF ( COLOUR ) THEN
         AXSPEN = 3
         DO I = 1, NDISP
            INLPEN( I ) = 3
         END DO

*  Otherwise as white.
      ELSE 
         AXSPEN = 1
         DO I = 1, NDISP
            INLPEN( I ) = 1
         END DO
      END IF

*  Set the pens for tick marks and numeric label as white.
      TICPEN = 1
      NLBPEN = 1

*  Set the pens used to draw the labels and title as white.
      LABPEN = 1
      TITPEN = 1

*  Define the order of the abscissa.  It cannot be flipped in a
*  logarithmic axis due to the limit of one transform in AGI and since
*  world co-ordinates must increase from left to right.  XORDER = 0
*  means not flipped; = 1 means flipped.
      XORDER = 0
      IF ( ( .NOT. XLOG ) .AND. XLMT( 1 ) .GT. XLMT( 2 ) ) XORDER = 1

*  Draw the plot in solid lines when colour is available.
      CALL IRM_MLINE( NSMP, NDISP, .TRUE., %VAL( PXPNTR( 1 ) ), 
     :                %VAL( IPNTR( 1 ) ), XORDER, 0, LINPEN, SOLID,
     :                X1, X2, Y1, Y2, AXSPEN, PLTITL( :TITLN ), TITPEN,
     :                ABSLAB, ORDLAB, LBSIZE, LABPEN, INLAB, INLBPS,
     :                INLPEN, -1, -1, XLMT, YLMT, XLOG, YLOG, .TRUE.,
     :                .TRUE., MAJTIC, MINTIC, TICLN, OUTTIC, TICPEN,
     :                NLBPEN, BAD, STATUS )
         
*  Draw a table of the offsets.
*  ============================

*  If the vertical axis is linear, draw the offset mark and offset
*  table.
      IF ( .NOT. YLOG .AND. KEY ) THEN
 
*  Get the grid y coordinate for each line offset that is to be drawn.
         DO I = 1, NDISP
            IF ( LINOFS( I ) .NE. VAL__BADR .AND.
     :           MOD( I, ANNOIN ) .EQ. 0 ) THEN
               OFFGY( I ) = SNX_AGUGY( LINOFS( I ) )
            ELSE
               OFFGY( I ) = -1.0
            END IF
         END DO

*  If colour is available, set pen 2 as red.
         IF ( COLOUR ) THEN
            PEN2 = 2

*  Otherwise, set both pens as white.
         ELSE
            PEN2 = 1
         END IF

* Draw trace and offset marks.
         OFFLEN = 0.05 * ( X2 - X1 )
         CALL SGS_SPEN( PEN2 )
         CALL SGS_STXJ( 'BL' )
         CALL SGS_SHTX( 0.75 * LBSIZE )
         DO I = 1, NDISP

*  Draw the left and right mark for the Ith line, if the mark is in the
*  plotting box
            IF ( OFFGY( I ) .GE. 0.0 .AND. OFFGY( I ) .LE. 1.0 ) THEN
               CALL SGS_LINE( 0.0, OFFGY( I ), OFFLEN, OFFGY( I ) )
               CALL SGS_LINE( 1.0 - OFFLEN, OFFGY( I ), 1.0, 
     :                        OFFGY( I ) )  

*  Write right-hand edge line index.
               IF ( MOD( I, ANNOIN ) .EQ. 0 ) THEN
                  CALL SGS_TX( 1.0, OFFGY( I ), INLAB( I ) )
               END IF
            END IF
         END DO

*  Write the contents of the offset table.  Watch for undefined rows.
         OFFTAB( 1 ) = 'LINE'
         OFFTAB( NDISP + 2 ) = 'OFFSET'
         DO I = 1, NDISP
            CALL CHR_ITOC( LNINDX( NDISP - I + 1 ), 
     :                     OFFTAB( I + 1 ), NCHAR )
            IF ( LINOFS( NDISP - I + 1 ) .EQ. VAL__BADR ) THEN
               OFFTAB( NDISP + 2 + I ) = 'Undefined'
            ELSE
               CALL CHR_RTOC( LINOFS( NDISP - I + 1 ),
     :                        OFFTAB( NDISP + 2 + I ), NCHAR )
            END IF
         END DO

*  Display the offset table.
         CALL IRM_TABLE( ' ', NDISP + 1, 2, OFFTAB( 1 ), OFFIN, 1.1,
     :                   1.45, -0.1, 1.05, COLOUR, 1, TABBOT, STATUS )
      END IF

*  Map the axis centres with the correct implementation type if not
*  already done so.
*  ================================================================

*  When world co-ordinates have been requested, but the NDF contains
*  axis information a transformation from world to data co-ordinates is
*  possible.  However, there is no axis array mapped at this point,
*  since the x co-ordinates are stored in workspace.  Therefore free
*  this workspace, since it is no longer required, record this fact,
*  and map the axis centres using the implementation data type.
      IF ( PWORK .AND. DACOOR ) THEN
         CALL DAT_ANNUL( PXLOC, STATUS )
         PWORK = .FALSE.
         CALL NDF_AMAP( NDFS, 'Centre', DIMABS, ATYPE, 'READ',
     :                  PXPNTR, EL, STATUS )

*  Real axis centres are already mapped for GKS/NCAR, but if the data
*  co-ordinates require double precision, unmap the real version and
*  remap in '_DOUBLE'.
      ELSE IF ( DATACO .AND. DPAXIS ) THEN
         CALL NDF_AUNMP( NDFS, 'Centre', DIMABS, STATUS )
         CALL NDF_AMAP( NDFS, 'Centre', DIMABS, '_DOUBLE', 'READ',
     :                  PXPNTR, EL, STATUS )
      END IF

*  Define the DATA picture's world co-ordinates if not already set.
*  ================================================================

*  Make the NCAR grid the data zone.
      CALL SGS_ZONE( 0.0, 1.0, 0.0, 1.0, ZONEI, STATUS )
      
*  Set the world co-ordinates that will be stored in the database to
*  pixels along the abscissa to follow the KAPPA convention.  This
*  cannot be done for logarithmic plots, so log( world co-ordinates )
*  are stored.

*  Get the lower and upper abscissa limits at the left and right of the
*  NCAR grid.
      IF ( XLOG ) THEN
         XLMT( 1 ) = LOG10( XLMT( 1 ) )
         XLMT( 2 ) = LOG10( XLMT( 2 ) )
      ELSE
         XLMT( 1 ) = LBND( DIMABS ) - 0.5
         XLMT( 2 ) = UBND( DIMABS ) - 0.5
      END IF

*  Get the lower and upper ordinate limits at the bottom and top of the
*  NCAR grid.
      IF ( YLOG ) THEN
         YLMT( 1 ) = LOG10( SNX_AGGUY( 0.0 ) )
         YLMT( 2 ) = LOG10( SNX_AGGUY( 1.0 ) )
      ELSE
         YLMT( 1 ) = SNX_AGGUY( 0.0 )
         YLMT( 2 ) = SNX_AGGUY( 1.0 )
      END IF

      CALL SGS_SW( XLMT( 1 ), XLMT( 2 ), YLMT( 1 ), YLMT( 2 ), STATUS )

*  Record the data picture in the database.
*  ========================================
      CALL KPG1_SDTRN( 'KAPPA_MLINPLOT', NDF, PICID3, STATUS )

*  World co-ordinates for data are log( data co-ordinates ), therefore
*  taking the anti-log will result in data positions.  When there is no
*  axis information a logarithmic abscissa or ordinate has world
*  co-ordinates which are log( data co-ordinates) or log( data values )
*  respectively.  Therefore, taking the anti-log will produced the
*  desired result.
      IF ( ( XLOG .AND. ( DATACO .OR. .NOT. DACOOR ) ) .OR.
     :     ( YLOG .AND. .NOT. DACOOR ) ) THEN

*  Store the transformation for logarithmic world co-ordinates.
         CALL KPG1_LGTRN( XLOG, YLOG, STATUS )

      ELSE IF ( DACOOR ) THEN

*  Determine the scale and offset of the transformation between
*  world and data positional co-ordinates. 
*  ============================================================
         IF ( DPAXIS ) THEN

*  Find the transformation for the x axis.
            CALL KPG1_DWSOD( LBND( DIMABS ), UBND( DIMABS ),
     :                       %VAL( PXPNTR( 1 ) ), DSCALE( 1 ),
     :                       DOFSET( 1 ), STATUS )

*  Since the ordinate units are not co-ordinates, but data
*  values, the identity transformation is established.
            DSCALE( 2 ) = 1.0D0
            DOFSET( 2 ) = 0.0D0

*  Only store the transformation for data co-ordinates not in pixel
*  co-ordinates.
            IF ( ABS( DSCALE( 1 ) - 1.0D0 ) .GT. VAL__EPSD  .OR.
     :           ABS( DOFSET( 1 ) - 0.0D0 ) .GT. VAL__EPSD ) THEN
               CALL KPG1_LLTRD( XLOG, YLOG, DSCALE, DOFSET, STATUS )

            ELSE IF ( XLOG ) THEN

*  Store the transformation for logarithmic world co-ordinates.  This
*  allows for the case where the axis array contains merely pixel
*  co-ordinates.
               CALL KPG1_LGTRN( XLOG, YLOG, STATUS )
            END IF

*  Single-precision is sufficient.
         ELSE

*  Find the transformation for the x axis.
            CALL KPG1_DWSOR( LBND( DIMABS ), UBND( DIMABS ),
     :                       %VAL( PXPNTR( 1 ) ), SCALE( 1 ),
     :                       OFFSET( 1 ), STATUS )

*  Since the ordinate units are not co-ordinates, but data values, the
*  identity transformation is established.
            SCALE( 2 ) = 1.0
            OFFSET( 2 ) = 0.0

*  Only store the transformation for data co-ordinates not in pixel
*  co-ordinates.
            IF ( ABS( SCALE( 1 ) - 1.0 ) .GT. VAL__EPSR .OR.
     :           ABS( OFFSET( 1 ) - 0.0 ) .GT. VAL__EPSR ) THEN
               CALL KPG1_LLTRR( XLOG, YLOG, SCALE, OFFSET, STATUS )

            ELSE IF ( XLOG ) THEN

*  Store the transformation for logarithmic world co-ordinates.  This
*  allows for the case where the axis array contains merely pixel
*  co-ordinates.
               CALL KPG1_LGTRN( XLOG, YLOG, STATUS )
            END IF
         END IF
      END IF

*  Reinstate the current picture on entry.
      CALL AGI_SELP( PICID1, STATUS )
 
*  Recover the previous NCAR settings.
*  ===================================
      CALL SNX_AGRES( HEAP )

 960  CONTINUE

*  Close SGS and AGI down.
      CALL AGS_DEASS( 'DEVICE', .FALSE., STATUS )

 970  CONTINUE

*  Tidy the workspace.
      IF ( PWORK ) CALL AIF_ANTMP( PXLOC, STATUS )
      IF ( LWORK ) CALL ARY_ANNUL( IARY, STATUS )

 980  CONTINUE

*  Unmap and annul NDF data.
      CALL NDF_END( STATUS )

 990  CONTINUE

      END
