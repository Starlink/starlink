      SUBROUTINE HISTOGRAM( STATUS )
*+
*  Name:
*     HISTOGRAM

*  Purpose:
*     Computes an histogram of an NDF's values.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL HISTOGRAM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application derives histogram information for an NDF array
*     between specified limits, using either a set number of bins
*     (Parameter NUMBIN) or a chosen bin width (Parameter WIDTH).  The
*     histogram is reported, and may optionally be written to a text log
*     file, and/or plotted graphically.
*
*     By default, each data value contributes a value of one to the
*     corresponding histogram bin, but alternative weights may be
*     supplied via Parameter WEIGHTS.

*  Usage:
*     histogram in numbin range [comp] [logfile]

*  ADAM Parameters:
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        plot.  The width of the margins left for the annotation may be
*        controlled using Parameter MARGIN.  The appearance of the axes
*        (colours, fonts, etc.) can be controlled using the Parameter
*        STYLE.  The dynamic default is TRUE if CLEAR is TRUE, and FALSE
*        otherwise. []
*     CLEAR = _LOGICAL (Read)
*        If TRUE the current picture is cleared before the plot is
*        drawn.  If CLEAR is FALSE not only is the existing plot
*        retained, but also an attempt is made to align the new picture
*        with the existing picture.  Thus you can generate a composite
*        plot within a single set of axes, say using different colours
*        or modes to distinguish data from different datasets.  [TRUE]
*     COMP = LITERAL (Read)
*        The name of the NDF array component to have its histogram
*        computed: "Data", "Error", "Quality" or "Variance" (where
*        "Error" is the alternative to "Variance" and causes the square
*        root of the variance values to be taken before computing the
*        statistics).  If "Quality" is specified, then the quality
*        values are treated as numerical values (in the range 0 to
*        255).  ["Data"]
*     CUMUL = _LOGICAL (Read)
*        Should a cumulative histogram be reported?  [FALSE]
*     DEVICE = DEVICE (Read)
*        The graphics workstation on which to produce the plot.  If it
*        is null (!), no plot will be made.  [Current graphics device]
*     IN = NDF (Read)
*        The NDF data structure to be analysed.
*     LOGFILE = FILENAME (Write)
*        A text file into which the results should be logged.  If a null
*        value is supplied (the default), then no logging of results
*        will take place.  [!]
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave for axis annotation, given
*        as fractions of the corresponding dimension of the current
*        picture.  Four values may be given, in the order bottom, right,
*        top, left.  If fewer than four values are given, extra values
*        are used equal to the first supplied value.  If these margins
*        are too narrow any axis annotation may be clipped.  If a null
*        (!) value is supplied, the value used is 0.15 (for all edges)
*        if either annotated axes or a key are produced, and zero
*        otherwise.  [current value]
*     NUMBIN = _INTEGER (Read)
*        The number of histogram bins to be used.  This must lie in the
*        range 2 to 10000.  The suggested default is the current value.
*        It is ignored if WIDTH is not null.
*     OUT = NDF (Read)
*        Name of the NDF structure to save the histogram in its data
*        array.  If null (!) is entered the histogram NDF is not
*        created.  [!]
*     RANGE = LITERAL (Read)
*        RANGE specifies the range of values for which the histogram is
*        to be computed.  The supplied string should consist of up to
*        three sub-strings, separated by commas.  For all but the option
*        where you give explicit numerical limits, the first sub-string
*        must specify the method to use.  If supplied, the other two
*        sub-strings should be numerical values as described below
*        (default values will be used if these sub-strings are not
*        provided).  The following options are available.
*
*        - lower,upper -- You can supply explicit lower and upper
*        limiting values.  For example, "10,200" would set the histogram
*        lower limit to 10 and its upper limit to 200.  No method name
*        prefixes the two values.  If only one value is supplied,
*        the "Range" method is adopted.  The limits must be within the
*        dynamic range for the data type of the NDF array component.
*
*        - "Percentiles" -- The default values for the histogram data
*        range are set to the specified percentiles of the data.  For
*        instance, if the value "Per,10,99" is supplied, then the lowest
*        10% and highest 1% of the data values are excluded from the
*        histogram.  If only one value, p1, is supplied, the second
*        value, p2, defaults to (100 - p1).  If no values are supplied,
*        the values default to "5,95".  Values must be in the range 0 to
*        100.
*
*        - "Range" -- The minimum and maximum array values are used.  No
*        other sub-strings are needed by this option.  Null (!) is a
*        synonym for the "Range" method.
*
*        - "Sigmas" -- The histogram limiting values are set to the
*        specified numbers of standard deviations below and above the
*        mean of the data.  For instance, if the supplied value is
*        "sig,1.5,3.0", then the histogram extends from the mean of the
*        data minus 1.5 standard deviations to the mean plus 3 standard
*        deviations.  If only one value is supplied, the second value
*        defaults to the supplied value.  If no values are supplied,
*        both default to "3.0".
*
*        The "Percentiles" and "Sigmas" methods are useful to generate
*        a first pass at the histogram.  They reduce the likelihood
*        that all but a small number of values lie within a few
*        histogram bins.
*
*        The extreme values are reported unless Parameter RANGE is
*        specified on the command line.  In this case extreme values
*        are only calculated where necessary for the chosen method.
*
*        The method name can be abbreviated to a single character, and
*        is case insensitive.  The initial value is "Range".  The
*        suggested defaults are the current values, or ! if these do
*        not exist.  [current value]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use when drawing the annotated axes and data values.
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
*        The appearance of the histogram curve is controlled by the
*        attributes Colour(Curves), Width(Curves), etc.  (The synonym
*        Line may be used in place of Curves.)  [current value]
*     TITLE = LITERAL (Read)
*        Title for the histogram NDF.  ["KAPPA - Histogram"]
*     WEIGHTS = NDF (Read)
*        An optional NDF holding weights associated with each input pixel
*        value (supplied via Parameter IN). Together with Parameter
*        WEIGHTSTEP, these determine the count added to the corresponding
*        histogram bin for each input pixel value. For instance, weights
*        could be related to the variance of the data values, or to the
*        position of the data values within the input NDF. If a null value
*        (!) is supplied for WEIGHTS, all input values contribute a count of
*        one to the corresponding histogram bin. If an NDF is supplied, the
*        histogram count for a particular input pixel is formed by dividing
*        its weight value (supplied in the WEIGHTS NDF) by the value of
*        Parameter WEIGHTSTEP, and then taking the nearest integer. Input
*        pixels with bad or zero weights are excluded from the histogram. [!]
*     WEIGHTSTEP = _DOUBLE (Read)
*        Only accessed if a value is supplied for Parameter WEIGHTS.
*        WEIGHTSTEP is the increment in weight value that corresponds to
*        a unit increment in histogram count.
*     WIDTH = _DOUBLE (Read)
*        The bin width.  This is the alternative to setting the number of
*        bins.  The bins of the chosen width start from the minimum value
*        and do not exceed the maximum value.  Values are constrained
*        to give between 2 and 10000 bins.  If this parameter is set to
*        null (!), the data range and Parameter NUMBIN are used to specify
*        the bin width. [!]
*     XLEFT = _DOUBLE (Read)
*        The axis value to place at the left hand end of the horizontal
*        axis of the plot.  If a null (!) value is supplied, the minimum
*        data value in the histogram is used.  The value supplied may be
*        greater than or less than the value supplied for XRIGHT.  [!]
*     XLOG = _LOGICAL (Read)
*        TRUE if the plot X axis is to be logarithmic.  Any histogram
*        bins which have negative or zero central data values are
*        omitted from the plot.  [FALSE]
*     XRIGHT = _DOUBLE (Read)
*        The axis value to place at the right hand end of the horizontal
*        axis of the plot.  If a null (!) value is supplied, the maximum
*        data value in the histogram is used.  The value supplied may be
*        greater than or less than the value supplied for XLEFT.  [!]
*     YBOT = _DOUBLE (Read)
*        The axis value to place at the bottom end of the vertical axis
*        of the plot.  If a null (!) value is supplied, the lowest count
*        the histogram is used.  The value supplied may be greater than
*        or less than the value supplied for YTOP.  [!]
*     YLOG = _LOGICAL (Read)
*        TRUE if the plot Y axis is to be logarithmic.  Empty bins are
*        removed from the plot if the Y axis is logarithmic.  [FALSE]
*     YTOP = _DOUBLE (Read)
*        The axis value to place at the top end of the vertical axis of
*        the plot.  If a null (!) value is supplied, the largest count
*        in the histogram is used.  The value supplied may be greater
*        than or less than the value supplied for YBOT.  [!]

*  Examples:
*     histogram image 100 ! device=!
*        Computes and reports the histogram for the data array in the
*        NDF called image.  The histogram has 100 bins and spans the
*        full range of data values.
*     histogram ndf=spectrum comp=variance range="100,200" numbin=20
*        Computes and reports the histogram for the variance array in
*        the NDF called spectrum.  The histogram has 20 bins and spans
*        the values between 100 and 200.  A plot is made to the current
*        graphics device.
*     histogram ndf=spectrum comp=variance range="100,204" width=5
*        This behaves the same as the previous example, even though it
*        specifies a larger maximum, as the same number of width=5 bins
*        are used.
*     histogram cube(3,4,) 10 si out=c3_4_hist device=!
*        Computes and reports the histogram for the z-vector at (x,y)
*        element (3,4) of the data array in the 3-dimensional NDF called
*        cube.  The histogram has 10 bins and spans a range three
*        standard deviations either side of the mean of the data values.
*        The histogram is written to a one-dimensional NDF called
*        c3_4_hist.
*     histogram cube numbin=32 ! device=xwindows style="title=cube"
*        Computes and reports the histogram for the data array in
*        the NDF called cube.  The histogram has 32 bins and spans the
*        full range of data values.  A plot of the histogram is made to
*        the XWINDOWS device, and is titled "cube".
*     histogram cube numbin=32 ! device=xwindows ylog style=^style.dat
*        As in the previous example except the logarithm of the number
*        in each histogram bin is plotted, and the contents of the text
*        file style.dat control the style of the resulting graph.  The
*        plotting style specified in file style.dat becomes the default
*        plotting style for future invocations of HISTOGRAM.
*     histogram cube numbin=32 ! device=xw ylog tempstyle=^style.dat
*        This is the same as the previous example, except that the style
*        specified in file style.dat does not become the default style
*        for future invocations of HISTOGRAM.
*     histogram halley(~200,~300) "pe,10,90" logfile=hist.dat \
*        Computes the histogram for the central 200 by 300 elements of
*        the data array in the NDF called halley, and writes the
*        results to a logfile called hist.dat.  The histogram uses the
*        current number of bins, and includes data values between the 10
*        and 90 percentiles.  A plot appears on the current graphics
*        device.

*  Related Applications:
*     KAPPA: HISTAT, MSTATS, NUMB, STATS; Figaro: HIST, ISTAT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     QUALITY, LABEL, TITLE, UNITS, and HISTORY components of the input
*     NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998-2000, 2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
*     Council.
*     Copyright (C) 2008-2012, 2014 Science and Technology Facilities
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
*     DSB: David S. Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 March 9 (MJC):
*        Original NDF version.
*     1995 May 1 (MJC):
*        Made examples and usage lowercase.  Moved position of COMP
*        parameter.  Added Related Applications.  Shortened the section
*        getting the range values.  Used PSX for workspace.  Fixed a
*        bug in RANGE reporting.  Allowed for Error as a COMP option.
*        Added PXSIZE and PYSIZE parameters.
*     27-FEB-1998 (DSB):
*        Corrected the reporting of the upper limit of _DOUBLE data by
*        replacing call to CHR_PUTI by CHR_PUTD.
*     12-JUL-1999 (TDCA):
*        Converted graphics to AST/PGPLOT
*     17-SEP-1999 (DSB):
*        Tidied up. NDF calls changed to LPG to use auto-looping.
*        Dynamic default parameters changed to use null default.
*     26-OCT-1999 (DSB):
*        Made MARGIN a fraction of the current picture, not the DATA
*        picture.
*     2000 February 16 (MJC):
*        Made the RANGE parameter literal and allowed additional methods
*        by which to specify the data-value limits of the histogram.
*        Moved the reporting the data range and obtaining the RANGE
*        parameter to a subroutine.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     01-OCT-2004 (PWD):
*        Moved CNF_PAR into declarations.
*     15-APR-2005 (PWD):
*        Parameterise use of backslash to improve portability.
*     23-FEB-2006 (DSB):
*        Added Parameter CUMUL.
*     2006 April 12 (MJC):
*        Remove unused variable and wrapped long lines.
*     2008 June 17 (MJC):
*        Trim trailing blanks from output NDF character components.
*     15-OCT-2009 (DSB):
*        Make use of KPG1_SAXAT and KPG1_ASTTL.
*     2010 August 15 (MJC):
*        Add Parameter TEMPSTYLE.
*     2010 October 14 (MJC):
*        Permit temporary style attributes.  Withdraw TEMPSTYLE.
*     16-JUN-2011 (DSB):
*        Changed to allow display of data values outside range of _REAL.
*     19-AUG-2011 (DSB):
*        Added Parameters WEIGHTS and WEIGHTSTEP.
*     2012-05-08 (TIMJ):
*        Add _INT64 support.
*     2014 April 26 (MJC):
*        Add Parameter WIDTH.
*     20-MAR-2020 (DSB):
*        Changed KPG1_GRAPH API.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR constants
      INCLUDE 'PAR_ERR'          ! PAR error codes
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function
                                 ! declarations
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      CHARACTER BCKSLH*1         ! A single backslash
*  Some compilers need '\\' to get '\', which isn't a problem as Fortran
*  will truncate the string '\\' to '\' on the occasions when that isn't
*  needed.
      PARAMETER( BCKSLH = '\\' )

      INTEGER MAXBIN             ! Maximum number of histogram bins
      PARAMETER( MAXBIN = 10000 )!

      INTEGER SZBUF              ! Size of text buffer
      PARAMETER ( SZBUF = 132 )

*  Local Variables:
      INTEGER AXPNTR( 1 )        ! Pointer to the histogram axis centres
      LOGICAL BAD                ! There may be bad values in the array
      REAL BINWID                ! Histogram bin width
      DOUBLE PRECISION BSCALE( 2 ) ! Scale factors for labels
      CHARACTER * ( SZBUF ) BUFFER ! Text buffer
      CHARACTER * ( 8 ) COMP     ! Name of array component to analyse
      LOGICAL CUMUL              ! Produce cumulative histogram?
      DOUBLE PRECISION DDUMMY    ! Dummy for swapping the data range
      DOUBLE PRECISION DRANGE( 2 ) ! Data range of the histogram
      DOUBLE PRECISION DRAT      ! Max ratio of data value to _REAL
      INTEGER EL                 ! Number of array elements mapped
      INTEGER FRAME              ! Frame used to pass default attributes
      INTEGER HPNTR              ! Pointer to the histogram
      INTEGER HPPTR1             ! Pointer to the histogram x locus
      INTEGER HPPTR2             ! Pointer to the histogram y locus
      INTEGER IERR               ! Position of first conversion error
      INTEGER IFIL               ! File descriptor for logfile
      INTEGER IWCS               ! Pointer to NDFs WCS FrameSet
      CHARACTER * ( 256 ) LABEL  ! Label of the histogram NDF
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel bounds of input NDF
      LOGICAL LOGFIL             ! Log file is required
      INTEGER MAXH               ! Maximum number in an histogram bin
      REAL MAXIM                 ! Maximum value of pixels in array
                                 ! standardised for locus
      INTEGER MAXPOS             ! Index of maximum-valued pixel
      DOUBLE PRECISION MAXWID    ! Maximum histogram bin width
      CHARACTER * ( 8 ) MCOMP    ! Component name for mapping arrays
      INTEGER MINH               ! Minimum number in an histogram bin
      REAL MINIM                 ! Minimum value of pixels in array
                                 ! standardised for locus
      INTEGER MINPOS             ! Index of minimum-valued pixel
      DOUBLE PRECISION MINWID    ! Minimum histogram bin width
      INTEGER NC                 ! No. characters in text buffer
      INTEGER NDFI               ! Identifier for input NDF
      INTEGER NDFO               ! NDF identifier of output histogram
      INTEGER NDFW               ! Identifier for weights base NDF
      INTEGER NDFW2              ! Identifier for weights section NDF
      INTEGER NDIM               ! Number of pixel axes in input NDF
      INTEGER NERR               ! Number of conversion errors
      INTEGER NINVAL             ! Number of invalid pixels in array
      INTEGER NUMBIN             ! Number of histogram bins
      INTEGER OUTPTR( 1 )        ! Pointer to output NDF histogram
      INTEGER PNTR( 1 )          ! Pointer to mapped NDF array
      CHARACTER * ( 255 ) TEXT   ! Temporary text variable
      CHARACTER *( NDF__SZTYP ) TYPE ! Numeric type for processing
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel bounds of input NDF
      CHARACTER * ( 256 ) UNITS  ! Units of the histogram NDF
      DOUBLE PRECISION WEIGHT    ! Value of WEIGHTSTEP parameter
      DOUBLE PRECISION WIDTH     ! Bin width
      INTEGER WPNTR              ! Pointer to weights array
      CHARACTER * ( 255 ) XLAB   ! Default X axis label
      LOGICAL XLOG               ! X axis of plot is logarithmic
      CHARACTER * ( 255 ) YLAB   ! Default Y axis label
      LOGICAL YLOG               ! Y axis of plot is logarithmic
      REAL XL                    ! Left X limit
      REAL XR                    ! Right X limit
      REAL YB                    ! Bottom Y limit
      REAL YT                    ! Top Y limit

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise whether or not the logfile is required.
      LOGFIL = .FALSE.

*  Obtain the NDF to be analysed.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Get its WCS FrameSet.
      CALL KPG1_GTWCS( NDFI, IWCS, STATUS )

*  Determine which array component is to be analysed.
*  Find which components to plot.
      CALL KPG1_ARCOG( 'COMP', NDFI, MCOMP, COMP, STATUS )

*  Obtain the numeric type of the NDF array component to be analysed.
      CALL NDF_TYPE( NDFI, COMP, TYPE, STATUS )

*  Map the array using this numeric type and see whether there may be
*  bad pixels present.
      CALL NDF_MAP( NDFI, MCOMP, TYPE, 'READ', PNTR, EL, STATUS )
      IF ( COMP .EQ. 'QUALITY' ) THEN
         BAD = .FALSE.
      ELSE
         CALL NDF_BAD( NDFI, COMP, .FALSE., BAD, STATUS )
      END IF

*  Exit if something has gone wrong.  Good status is required before
*  obtaining the range.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the data limits.
*  =======================

*  Select appropriate routine for the data type chosen.  Note
*  the data-value scaling limits are double precision for all
*  data types.
      IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPG1_DARAR( 'RANGE', EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    'Limit,Percentiles,Range,Sigmas', BAD,
     :                    DRANGE( 1 ), DRANGE( 2 ), STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_DARAD( 'RANGE', EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    'Limit,Percentiles,Range,Sigmas', BAD,
     :                    DRANGE( 1 ), DRANGE( 2 ), STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_DARAI( 'RANGE', EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    'Limit,Percentiles,Range,Sigmas', BAD,
     :                    DRANGE( 1 ), DRANGE( 2 ), STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL KPG1_DARAW( 'RANGE', EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    'Limit,Percentiles,Range,Sigmas', BAD,
     :                    DRANGE( 1 ), DRANGE( 2 ), STATUS )

      ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_DARAB( 'RANGE', EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    'Limit,Percentiles,Range,Sigmas', BAD,
     :                    DRANGE( 1 ), DRANGE( 2 ), STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_DARAUB( 'RANGE', EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                     'Limit,Percentiles,Range,Sigmas', BAD,
     :                     DRANGE( 1 ), DRANGE( 2 ), STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_DARAUW( 'RANGE', EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                     'Limit,Percentiles,Range,Sigmas', BAD,
     :                     DRANGE( 1 ), DRANGE( 2 ), STATUS )

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL KPG1_DARAK( 'RANGE', EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    'Limit,Percentiles,Range,Sigmas', BAD,
     :                    DRANGE( 1 ), DRANGE( 2 ), STATUS )

      END IF

*  Sort if necessary.
      IF ( DRANGE( 1 ) .GT. DRANGE( 2 ) ) THEN
         DDUMMY = DRANGE( 1 )
         DRANGE( 1 ) = DRANGE( 2 )
         DRANGE( 2 ) = DDUMMY
      END IF

*  Obtain the other parameters of the histogram.
*  =============================================

*  See if a cumulative histogram is required.
      CALL PAR_GET0L( 'CUMUL', CUMUL, STATUS )

*  Optionally obtain the bin width.
      CALL ERR_MARK
      MAXWID = 0.5D0 * ( DRANGE(2) - DRANGE( 1 ) )
      MINWID = ( DRANGE(2) - DRANGE( 1 ) ) / DBLE( MAXBIN )
      CALL PAR_GDR0D( 'WIDTH', -1.0D0, MINWID, MAXWID, .FALSE.,
     :                WIDTH, STATUS )

*  Null means specify the number of bins instead.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Get the number of histogram bins to be used, within a sensible
*  range.
         CALL PAR_GDR0I( 'NUMBIN', 20, 2, MAXBIN, .TRUE., NUMBIN,
     :                   STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         NUMBIN = INT( ( DRANGE( 2 ) - DRANGE( 1 ) ) / WIDTH )
         DRANGE( 2 ) = DRANGE( 1 ) + DBLE( NUMBIN ) * WIDTH
      END IF
      CALL ERR_RLSE

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain weights for each data value.
*  ===================================

*  Assume all values will contribute a count of 1 to the corresponding
*  histogram bin. This is indicated by a WEIGHT value of zero.
      WEIGHT = 0.0D0
      WPNTR = PNTR( 1 )

*  Attempt to get a weights NDF. If none is supplied, annul the error.
      CALL LPG_ASSOC( 'WEIGHTS', 'READ', NDFW, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Otherwise, get the weight corresponding to a histogram bin count of 1.
      ELSE
         CALL PAR_GET0D( 'WEIGHTSTEP', WEIGHT, STATUS )

*  Report an error if a value of zero is given.
         IF( WEIGHT .EQ. 0.0D0 .AND. STATUS .EQ. SAI__OK ) THEN
            CALL ERR_REP( ' ', 'Illegal value of zero supplied for '//
     :                    'parameter WEIGHT.', STATUS )

*  Otherwise get a section of the weights array that matches the bounds
*  of the input NDF, and map it.
         ELSE
            CALL NDF_BOUND( NDFI, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
            CALL NDF_SECT( NDFW, NDIM, LBND, UBND, NDFW2, STATUS )
            CALL NDF_MAP( NDFW2, 'Data', '_DOUBLE', 'READ', WPNTR,
     :                    EL, STATUS )
         END IF
      END IF

*  Obtain an optional file for logging the results.
*  ================================================
      CALL ERR_MARK
      LOGFIL = .FALSE.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 132, IFIL, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGFIL = .TRUE.
      END IF
      CALL ERR_RLSE

*  Write the heading of the histogram report.
*  ==========================================

*  Display the NDF name, also sending it to the logfile if necessary.
      CALL MSG_BLANK( STATUS )
      IF ( LOGFIL ) CALL FIO_WRITE( IFIL, ' ', STATUS )
      CALL NDF_MSG( 'NDF', NDFI )
      CALL MSG_LOAD( 'NDFNAME',
     :               '   Histogram for the NDF structure ^NDF', BUFFER,
     :               NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUFFER( : NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUFFER( : NC ), STATUS )
      END IF

*  Display (and log) the NDF's title.
      CALL MSG_BLANK( STATUS )
      IF ( LOGFIL ) CALL FIO_WRITE( IFIL, ' ', STATUS )
      CALL NDF_CMSG( 'TITLE', NDFI, 'Title', STATUS )
      CALL MSG_LOAD( 'NDFTITLE',
     :               '      Title                     : ^TITLE',
     :               BUFFER, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUFFER( : NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUFFER( : NC ), STATUS )
      END IF

*  Display (and log) the name of the component being analysed.
      CALL MSG_SETC( 'COMP', MCOMP )
      CALL MSG_LOAD( 'NDFCOMP',
     :               '      NDF array analysed        : ^COMP',
     :               BUFFER, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUFFER( : NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUFFER( : NC ), STATUS )
      END IF

*  If a logfile is in use, display its name.
      IF ( LOGFIL ) CALL MSG_OUT( 'LOG',
     :              '      Logging to file           : $LOGFILE',
     :                            STATUS )

*    Obtain workspace for the histogram.
      CALL PSX_CALLOC( NUMBIN, '_INTEGER', HPNTR, STATUS )

*  Compute and report the histogram.
*  =================================

*  Assume the data limits are within the range of a REAL, so no scaling
*  is required to produce the labels.
      BSCALE( 1 ) = 1.0D0
      BSCALE( 2 ) = 1.0D0

*  Call the appropriate routines to compute the histogram and then
*  report the results.  The double-precision range values must be
*  converted to the appropriate type for the routines.
      IF ( TYPE .EQ. '_BYTE' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTB( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    %VAL( CNF_PVAL( WPNTR ) ), WEIGHT,
     :                    NUMBIN, CUMUL,
     :                    NUM_DTOB( DRANGE( 2 ) ),
     :                    NUM_DTOB( DRANGE( 1 ) ),
     :                    %VAL( CNF_PVAL( HPNTR ) ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                    REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTD( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    %VAL( CNF_PVAL( WPNTR ) ), WEIGHT,
     :                    NUMBIN, CUMUL,
     :                    DRANGE( 2 ), DRANGE( 1 ),
     :                    %VAL( CNF_PVAL( HPNTR ) ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSD( NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                    DRANGE( 1 ),
     :                    DRANGE( 2 ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLD( IFIL, NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                       DRANGE( 1 ),
     :                       DRANGE( 2 ), STATUS )
         END IF

*  We will need to convert the data range to single precision later on.
*  But this could cause problems if the data range is close to or
*  exceeds the range of single precision numbers. So if either limit is
*  more than one millionth of the max or min value that can be stored
*  in a REAL, choose a scale factor for the data so that the scaled data
*  does not exceed this limit. This scaling is removed when the axis
*  labels are drawn.
         DRAT = 1.0D6*MAX ( ABS( DRANGE( 1 )/DBLE( VAL__MINR ) ),
     :                      ABS( DRANGE( 2 )/DBLE( VAL__MAXR ) ) )
         IF( DRAT .GT. 1.0 ) THEN
            BSCALE( 1 ) = DRAT
            DRANGE( 1 ) = DRANGE( 1 )/DRAT
            DRANGE( 2 ) = DRANGE( 2 )/DRAT
         END IF

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTI( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    %VAL( CNF_PVAL( WPNTR ) ), WEIGHT,
     :                    NUMBIN, CUMUL,
     :                    NUM_DTOI( DRANGE( 2 ) ),
     :                    NUM_DTOI( DRANGE( 1 ) ),
     :                    %VAL( CNF_PVAL( HPNTR ) ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                    REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTR( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    %VAL( CNF_PVAL( WPNTR ) ), WEIGHT,
     :                    NUMBIN, CUMUL,
     :                    REAL( DRANGE( 2 ) ), REAL( DRANGE( 1 ) ),
     :                    %VAL( CNF_PVAL( HPNTR ) ), STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                    REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTUB( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                     %VAL( CNF_PVAL( WPNTR ) ), WEIGHT,
     :                     NUMBIN, CUMUL,
     :                     NUM_DTOUB( DRANGE( 2 ) ),
     :                     NUM_DTOUB( DRANGE( 1 ) ),
     :                     %VAL( CNF_PVAL( HPNTR ) ),
     :                     STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                    REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTUW( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                     %VAL( CNF_PVAL( WPNTR ) ), WEIGHT,
     :                     NUMBIN, CUMUL,
     :                     NUM_DTOUW( DRANGE( 2 ) ),
     :                     NUM_DTOUW( DRANGE( 1 ) ),
     :                     %VAL( CNF_PVAL( HPNTR ) ),
     :                     STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                    REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTW( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    %VAL( CNF_PVAL( WPNTR ) ), WEIGHT,
     :                    NUMBIN, CUMUL,
     :                    NUM_DTOW( DRANGE( 2 ) ),
     :                    NUM_DTOW( DRANGE( 1 ) ),
     :                    %VAL( CNF_PVAL( HPNTR ) ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                    REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTK( BAD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    %VAL( CNF_PVAL( WPNTR ) ), WEIGHT,
     :                    NUMBIN, CUMUL,
     :                    NUM_DTOK( DRANGE( 2 ) ),
     :                    NUM_DTOK( DRANGE( 1 ) ),
     :                    %VAL( CNF_PVAL( HPNTR ) ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                    REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      END IF

*  Obtain the axis and plot styles.
*  ================================

*  Are the axes logarithmic?
      CALL PAR_GTD0L( 'XLOG', .FALSE., .TRUE., XLOG, STATUS )
      CALL PAR_GTD0L( 'YLOG', .FALSE., .TRUE., YLOG, STATUS )

*  Create a Frame that will be used to communicate default attribute
*  values for the Plot.
      FRAME = AST_FRAME( 2, ' ', STATUS )

*  Ensure the Title attribute of the Frame has a useful value.
      CALL KPG1_ASTTL( FRAME, IWCS, NDFI, STATUS )

*  Set the Label, Units and Symbol attributes for the X axis so that
*  they describe the required NDF array component.
      CALL KPG1_SAXAT( NDFI, MCOMP, 1, XLOG, FRAME, STATUS )

*  Store the default Y-axis Label and Symbol in the Frame, and clear the
*  Units attribute.
      IF ( YLOG ) THEN
         TEXT = 'Log'//BCKSLH//'d10'//BCKSLH//'u(Count)'
      ELSE
         TEXT =  'Count'
      END IF

      CALL AST_SETC( FRAME, 'Label(2)', TEXT, STATUS )
      CALL AST_SETC( FRAME, 'Symbol(2)', TEXT, STATUS )
      CALL AST_CLEAR( FRAME, 'Unit(2)', STATUS )

*  Derive the data ranges.
*  =======================

*  Find the range of the numbers in the histogram bins.  Empty bins
*  will be filled with zero, so no need to check for bad values.
      BAD = .FALSE.
      CALL KPG1_MXMNI( BAD, NUMBIN, %VAL( CNF_PVAL( HPNTR ) ), NINVAL,
     :                 MAXH, MINH, MAXPOS, MINPOS, STATUS )

*  Obtain a range for the X axis, but since graphics is involved
*  single-precision floating-point limits are required.
      MAXIM = REAL( DRANGE( 2 ) )
      MINIM = REAL( DRANGE( 1 ) )

*  Generate the histogram locus.
*  =============================

*  Create work space for storing the histogram locus.
      CALL PSX_CALLOC( NUMBIN, '_REAL', HPPTR1, STATUS )
      CALL PSX_CALLOC( NUMBIN, '_REAL', HPPTR2, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the x-y points at the centre of each bin in the histogram.
         CALL KPG1_HSTLO( NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :                    MINIM, MAXIM,
     :                    XLOG, YLOG, %VAL( CNF_PVAL( HPPTR1 ) ),
     :                    %VAL( CNF_PVAL( HPPTR2 ) ),
     :                    STATUS )

*  Plot the histogram.
*  ===================

*  Plot the locus just computed within annotated axes.  Both axes'
*  limits are defined. Use a default value of 0.0 for the bottom of the
*  vertical axis.  This accesses Parameters AXES, CLEAR, DEVICE,
*  MARGIN, STYLE, XLEFT, XRIGHT, YBOT, and YLEFT.
         XL = VAL__BADR
         XR = VAL__BADR
         YB = 0.0
         YT = VAL__BADR
         CALL KPG1_GRAPH( NUMBIN, %VAL( CNF_PVAL( HPPTR1 ) ),
     :                    %VAL( CNF_PVAL( HPPTR2 ) ),
     :                    0.0, 0.0,  XLAB, YLAB, ' ', ' ',
     :                    ' ', 1, .TRUE., 'KAPPA_HISTOGRAM', .TRUE.,
     :                    .FALSE., BSCALE, FRAME, XL, XR, YB, YT,
     :                    STATUS )

*  If anything was plotted, shut down the graphics workstation and
*  database.
         IF ( FRAME .NE. AST__NULL ) THEN
            CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )
         END IF

*  End of check for no error getting workspace to plot histogram.
      END IF

*  Tidy work-space structures.
      CALL PSX_FREE( HPPTR1, STATUS )
      CALL PSX_FREE( HPPTR2, STATUS )

*  Create an output NDF.
*  =====================

*  Start a new error context.
      CALL ERR_MARK

*  Start a new NDF context.  Note that KPG1_CPNTI is inadequate, as it
*  does return the NDF identifier.
      CALL NDF_BEGIN

*  Create a new NDF.
      CALL LPG_CREAT( 'OUT', '_INTEGER', 1, 1, NUMBIN, NDFO, STATUS )

*  Map the data array.
      CALL NDF_MAP( NDFO, 'Data', '_INTEGER', 'WRITE', OUTPTR,
     :              NUMBIN, STATUS )

*  Write the slice to the NDF.
      CALL VEC_ITOI( .FALSE., NUMBIN, %VAL( CNF_PVAL( HPNTR ) ),
     :               %VAL( CNF_PVAL( OUTPTR( 1 ) ) ),
     :               IERR, NERR, STATUS )

*  Unmap the histogram.
      CALL NDF_UNMAP( NDFO, 'Data', STATUS )

*  There are no bad pixels in the histogram, so record this fact in
*  the NDF, for greater efficiency.
      CALL NDF_SBAD( .FALSE., NDFO, 'DATA', STATUS )

*  Get the title for the NDF.
      CALL NDF_CINP( 'TITLE', NDFO, 'TITLE', STATUS )

*  Write a label for the NDF.  The histogram is unitless.
      CALL NDF_CPUT( 'Number', NDFO, 'Label', STATUS )

*  Obtain the label and units from the input NDF.
      LABEL = ' '
      UNITS = ' '
      CALL NDF_CGET( NDFI, 'Label', LABEL, STATUS )
      CALL NDF_CGET( NDFI, 'Units', UNITS, STATUS )

*  Only create AXIS values if the data range fits into the range of a
*  REAL.
      IF( BSCALE( 1 ) .EQ. 1.0D0 ) THEN

*  Put the label and units in the axis structure, which is
*  also created at this point.  There is only one axis.  Note that
*  NDF_ACPUT does not truncate trailing blanks.
         IF ( LABEL .NE. ' ' ) THEN
            NC = CHR_LEN( LABEL )
            CALL NDF_ACPUT( LABEL( :NC ), NDFO, 'Label', 1, STATUS )
         END IF

         IF ( UNITS .NE. ' ' ) THEN
            NC = CHR_LEN( UNITS )
            CALL NDF_ACPUT( UNITS( :NC ), NDFO, 'Units', 1, STATUS )
         END IF

*  Map the axis centres.
         CALL NDF_AMAP( NDFO, 'CENTRE', 1, '_REAL', 'WRITE', AXPNTR,
     :                  NUMBIN, STATUS )

*  Fill the axis array with the centres of the histogram bins by first
*  finding the bin width.  This assumes an even distribution of points
*  within the bin which is probably not the case, but the difference is
*  only likely to be serious when the number of non-empty bins is low.
         BINWID = ( MAXIM - MINIM ) / REAL( NUMBIN )
         CALL KPG1_SSCOF( NUMBIN, DBLE( BINWID ),
     :                    DBLE( MINIM + 0.5 * BINWID ),
     :                    %VAL( CNF_PVAL( AXPNTR( 1 ) ) ), STATUS )

*  Warn the user if the AXIS structures cannot be created.
      ELSE
         CALL NDF_MSG( 'N', NDFO )
         CALL MSG_OUT( ' ', 'No AXIS structures will be stored in ^N '//
     :                 'because the data range is too large.', STATUS )
      END IF

*  Handle the null case invisibly.
      IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Tidy work-space structures.
      CALL PSX_FREE( HPNTR, STATUS )

*  Close down the NDF system.
      CALL NDF_END( STATUS )

*  Release the new error context.
      CALL ERR_RLSE

*  Arrive here if an error occurs.
  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Close the logfile, if used.
      IF ( LOGFIL ) CALL FIO_ANNUL( IFIL, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HISTOGRAM_ERR', 'HISTOGRAM: Error computing '//
     :                 'or displaying the histogram of an NDF''s '//
     :                 'pixels.', STATUS )
      END IF

      END
