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
*     between specified limits.  The histogram is reported, and may
*     optionally be written to a text log file, and/or plotted
*     graphically.

*  Usage:
*     histogram in numbin range [comp] [logfile]

*  ADAM Parameters:
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        plot. The width of the margins left for the annotation may be 
*        controlled using parameter MARGIN. The appearance of the axes 
*        (colours, fonts, etc) can be controlled using the parameter
*        STYLE. [TRUE]
*     CLEAR = _LOGICAL (Read)
*        If TRUE the current picture is cleared before the plot is 
*        drawn. If CLEAR is FALSE not only is the existing plot retained, 
*        but also an attempt is made to align the new picture with the
*        existing picture. Thus you can generate a composite plot within 
*        a single set of axes, say using different colours or modes to 
*        distinguish data from different datasets. [TRUE]
*     COMP = LITERAL (Read)
*        The name of the NDF array component to have its histogram
*        computed: "Data", "Error", "Quality" or "Variance" (where
*        "Error" is the alternative to "Variance" and causes the square
*        root of the variance values to be taken before computing the
*        statistics).  If "Quality" is specified, then the quality
*        values are treated as numerical values (in the range 0 to
*        255).  ["Data"]
*     DEVICE = DEVICE (Read)
*        The graphics workstation on which to produce the plot.  If it
*        is null (!), no plot will be made. [Current graphics device]
*     IN = NDF (Read)
*        The NDF data structure to be analysed.
*     LOGFILE = FILENAME (Write)
*        A text file into which the results should be logged. If a null
*        value is supplied (the default), then no logging of results
*        will take place. [!]
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave for axis annotation, given 
*        as fractions of the corresponding dimension of the DATA picture. 
*        Four values may be given, in the order - bottom, right, top, left. 
*        If less than four values are given, extra values are used equal to 
*        the first supplied value. If these margins are too narrow any axis 
*        annotation may be clipped. If a null (!) value is supplied, the
*        value used is 0.18 (for all edges) if either annotated axes or 
*        a key are produced, and zero otherwise. [current value]
*     NUMBIN = _INTEGER (Read)
*        The number of histogram bins to be used. This must lie in the
*        range 2 to 10000.  The suggested default is the current value.
*     OUT = NDF (Read)
*        Name of the NDF structure to save the histogram in its data
*        array.  If null (!) is entered the histogram NDF is not
*        created. [!]
*     RANGE( 2 ) = _DOUBLE (Write)
*        The range of values for which the histogram is to be computed.
*        A null value (!) selects the minimum and maximum array values.
*        If RANGE is specified on the command line, the extreme values
*        are not calculated and reported.  The suggested defaults are
*        the current values, or ! if these do not exist.
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to use 
*        when drawing the annotated axes and data values.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text file
*        preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and 
*        interpreted in the same manner. Attribute settings are applied in 
*        the order in which they occur within the list, with later settings
*        over-riding any earlier settings given for the same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*        
*        where <name> is the name of a plotting attribute, and <value> is
*        the value to assign to the attribute. Default values will be
*        used for any unspecified attributes. All attributes will be
*        defaulted if a null value (!) is supplied. See section "Plotting
*        Attributes" in SUN/95 for a description of the available
*        attributes. Any unrecognised attributes are ignored (no error is
*        reported). 
*
*        The appearance of the histogram curve is controlled by the
*        attributes Colour(Curves), Width(Curves), etc (the synonym Line
*        may be used in place of Curves). [current value] 
*     TITLE = LITERAL (Read)
*        Title for the histogram NDF.  ["KAPPA - Histogram"]
*     XLEFT = _REAL (Read)
*        The axis value to place at the left hand end of the horizontal
*        axis of the plot. If a null (!) value is supplied, the minimum data 
*        value in the histogram is used. The value supplied may be greater 
*        than or less than the value supplied for XRIGHT. [!]
*     XLOG = _LOGICAL (Read)
*        TRUE if the plot X axis is to be logarithmic. Any histogram bins 
*        which have negative or zero central data values are omitted from
*        the plot. [FALSE]
*     XRIGHT = _REAL (Read)
*        The axis value to place at the right hand end of the horizontal
*        axis of the plot. If a null (!) value is supplied, the maximum data 
*        value in the histogram is used. The value supplied may be greater 
*        than or less than the value supplied for XLEFT. [!]
*     YBOT = _REAL (Read)
*        The axis value to place at the bottom end of the vertical axis of 
*        the plot. If a null (!) value is supplied, the lowest count in 
*        the histogram is used. The value supplied may be greater than or 
*        less than the value supplied for YTOP. [!]
*     YLOG = _LOGICAL (Read)
*        TRUE if the plot Y axis is to be logarithmic. Empty bins are 
*        removed from the plot if the Y axis is logarithmic. [FALSE]
*     YTOP = _REAL (Read)
*        The axis value to place at the top end of the vertical axis of
*        the plot. If a null (!) value is supplied, the largest count in the 
*        histogram is used. The value supplied may be greater than or less 
*        than the value supplied for YBOT. [!]

*  Examples:
*     histogram image 100 ! device=!
*        Computes and reports the histogram for the data array in the
*        NDF called image.  The histogram has 100 bins and spans the
*        full range of data values.
*     histogram ndf=spectrum comp=variance range=[100,200] numbin=20
*        Computes and reports the histogram for the variance array in
*        the NDF called spectrum.  The histogram has 20 bins and spans
*        the values between 100 and 200.  A plot is made to the current
*        graphics device.
*     histogram cube(3,4,) 10 ! out=c3_4_hist device=!
*        Computes and reports the histogram for the z-vector at (x,y)
*        element (3,4) of the data array in the 3-dimensional NDF called
*        cube.  The histogram has 10 bins and spans the full range of
*        data values.  The histogram is written to a one-dimensional
*        NDF called c3_4_hist.
*     histogram cube numbin=32 ! device=xwindows style="'title=cube'"
*        Computes and reports the histogram for the data array in
*        the NDF called cube.  The histogram has 32 bins and spans the
*        full range of data values.  A plot of the histogram is made to
*        the XWINDOWS device, and is titled "cube".
*     histogram cube numbin=32 ! device=xwindows ylog style=^style.dat  
*        As in the previous example except the logarithm of the number
*        in each histogram bin is plotted, and the contents of the text
*        file style.dat control the style of the resulting graph.
*     histogram halley(~200,~300) range=[-1000,1000] logfile=hist.dat \
*        Computes the histogram for the central 200 by 300 elements of
*        the data array in the NDF called halley, and writes the
*        results to a logfile called hist.dat.  The histogram uses the
*        current number of bins, and includes data values between -1000
*        and 1000.  A plot appears on the current graphics device.

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

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
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
*        Tidied up. NDF calls changed to LPG to use auto-looping. Dynamic
*        default parameters changed to use null default.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR constants
      INCLUDE 'PAR_ERR'          ! PAR error codes
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXBIN             ! Maximum number of histogram bins
      PARAMETER( MAXBIN = 10000 )! 

      INTEGER SZBUF              ! Size of text buffer
      PARAMETER ( SZBUF = 132 )

*  Local Variables:
      BYTE BMAXV                 ! Max. value of pixels in array
      BYTE BMINV                 ! Min. value of pixels in array
      CHARACTER BUFFER*( SZBUF ) ! Text buffer
      CHARACTER COMP*8           ! Name of array component to analyse
      CHARACTER LABEL*256        ! Label of the histogram NDF
      CHARACTER MCOMP*8          ! Component name for mapping arrays
      CHARACTER NDFNAM*255       ! Base name of NDF (+ poss. an HDS path)
      CHARACTER TEXT*255         ! Temporary text variable
      CHARACTER TYPE*( NDF__SZTYP )! Numeric type for processing
      CHARACTER UNITS*256        ! Units of the histogram NDF
      CHARACTER XL*255           ! Default X axis label
      CHARACTER YL*255           ! Default Y axis label
      DOUBLE PRECISION DDUMMY    ! Dummy for swapping the data range
      DOUBLE PRECISION DMAXV     ! Max. value of pixels in array
      DOUBLE PRECISION DMINV     ! Min. value of pixels in array
      DOUBLE PRECISION DRANGE( 2 ) ! Data range of the histogram
      DOUBLE PRECISION DRDEF( 2 )! Defaults for data range
      DOUBLE PRECISION DRMAX     ! Maximum value for the range
      DOUBLE PRECISION DRMIN     ! Minimum value for the range
      INTEGER * 2 WMAXV          ! Max. value of pixels in array
      INTEGER * 2 WMINV          ! Min. value of pixels in array
      INTEGER ACTRNG             ! State of the RANGE parameter
      INTEGER AXPNTR( 1 )        ! Pointer to the histogram axis centres
      INTEGER EL                 ! Number of array elements mapped
      INTEGER HPNTR              ! Pointer to the histogram
      INTEGER HPPTR1             ! Pointer to the histogram x locus
      INTEGER HPPTR2             ! Pointer to the histogram y locus
      INTEGER IAT                ! Position with TEXT
      INTEGER IERR               ! Position of first conversion error
      INTEGER IFIL               ! File descriptor for logfile
      INTEGER IMAXV              ! Max. value of pixels in array
      INTEGER IMINV              ! Min. value of pixels in array
      INTEGER IPLOT              ! AST pointer to plot to use for plotting
      INTEGER LENXL              ! Used length of XL
      INTEGER LENYL              ! Used length of YL
      INTEGER MAXH               ! Maximum number in an histogram bin
      INTEGER MAXPOS             ! Index of maximum-valued pixel
      INTEGER MINH               ! Minimum number in an histogram bin
      INTEGER MINPOS             ! Index of minimum-valued pixel
      INTEGER NC                 ! No. characters in text buffer
      INTEGER NMLEN              ! Used length of NDFNAM
      INTEGER INDF1              ! Identifier for input NDF
      INTEGER INDF2              ! NDF identifier of output histogram
      INTEGER NERR               ! Number of conversion errors
      INTEGER NINVAL             ! No. invalid pixels in array
      INTEGER NUMBIN             ! Number of histogram bins
      INTEGER OUTPTR( 1 )        ! Pointer to output NDF histogram
      INTEGER PNTR( 1 )          ! Pointer to mapped NDF array
      LOGICAL BAD                ! There may be bad values in the array
      LOGICAL COMEXT             ! RANGE parameter is not active
      LOGICAL LOGFIL             ! Log file is required
      LOGICAL XLOG               ! X axis of plot is logarithmic
      LOGICAL YLOG               ! Y axis of plot is logarithmic
      REAL BINWID                ! Histogram bin width
      REAL MAXIM                 ! Max. value of pixels in array standardised for locus
      REAL MINIM                 ! Min. value of pixels in array standardised for locus 
      REAL RMAXV                 ! Max. value of pixels in array
      REAL RMINV                 ! Min. value of pixels in array

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise whether or not the logfile is required.
      LOGFIL = .FALSE.

*  Obtain the NDF to be analysed.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Determine which array component is to be analysed.
*  Find which components to plot.
      CALL KPG1_ARCOG( 'COMP', INDF1, MCOMP, COMP, STATUS )

*  Obtain the numeric type of the NDF array component to be analysed.
      CALL NDF_TYPE( INDF1, COMP, TYPE, STATUS )

*  Map the array using this numeric type and see whether there may be
*  bad pixels present.
      CALL NDF_MAP( INDF1, MCOMP, TYPE, 'READ', PNTR, EL, STATUS )
      IF( COMP .EQ. 'QUALITY' ) THEN
         BAD = .FALSE.
      ELSE
         CALL NDF_BAD( INDF1, COMP, .FALSE., BAD, STATUS )
      END IF

*  Exit if something has gone wrong.  Good status is required before
*  obtaining the range.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the parameters of the histogram.
*  =======================================
*
*  Obtain the range of values first; later get the number of bins.
*  Only compute the extreme values and report them when parameter RANGE
*  is not specified on the command line.
      CALL LPG_STATE( 'RANGE', ACTRNG, STATUS )
      COMEXT = ACTRNG .NE. PAR__ACTIVE

*  We need to find the minimum and maximum to assist the user.  This
*  requires the mapped array.  Now RANGE parameter is double precision,
*  so the default and limiting values must be converted from the
*  implementation type to real.
*  
*  Use an appropriate type.  Deal with signed-byte data.
      IF( TYPE .EQ. '_BYTE' ) THEN

         IF( COMEXT ) THEN

*  Obtain the maximum and minimum values to define the bounds of the
*  histogram.
            CALL KPG1_MXMNB( BAD, EL, %VAL( PNTR( 1 ) ), NINVAL,
     :                       BMAXV, BMINV, MAXPOS, MINPOS, STATUS )

*  Report the extreme values.
            NC = 0
            CALL CHR_PUTC( 'Minimum value is ', BUFFER, NC )
            CALL CHR_PUTI( NUM_BTOI( BMINV ), BUFFER, NC )
            CALL CHR_PUTC( ' and the maximum is ', BUFFER, NC )
            CALL CHR_PUTI( NUM_BTOI( BMAXV ), BUFFER, NC )
            CALL MSG_OUT( 'EXTREMES', BUFFER( :NC ), STATUS )

*  Set the default values.
            DRDEF( 1 ) = NUM_BTOD( BMINV )
            DRDEF( 2 ) = NUM_BTOD( BMAXV )
         ELSE

*  Extreme values have not been calculated, therefore have no suggested
*  default.
            DRDEF( 1 ) = NUM_ITOD( VAL__BADI )
            DRDEF( 2 ) = NUM_ITOD( VAL__BADI )
         END IF

*  Set the extreme values.  Use the full range of numbers when there are
*  no bad values.
         IF( BAD ) THEN
            DRMIN = NUM_BTOD( VAL__MINB )
            DRMAX = NUM_BTOD( VAL__MAXB )
         ELSE
            DRMIN = NUM_BTOD( NUM__MINB )
            DRMAX = NUM_BTOD( NUM__MAXB )
         END IF

*  Use an appropriate type.  Deal with double-precision data.
      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN

         IF( COMEXT ) THEN

*  Obtain the maximum and minimum values to define the bounds of the
*  histogram.
            CALL KPG1_MXMND( BAD, EL, %VAL( PNTR( 1 ) ), NINVAL,
     :                       DMAXV, DMINV, MAXPOS, MINPOS, STATUS )

*  Report the extreme values.
            NC = 0
            CALL CHR_PUTC( 'Minimum value is ', BUFFER, NC )
            CALL CHR_PUTD( DMINV, BUFFER, NC )
            CALL CHR_PUTC( ' and the maximum is ', BUFFER, NC )
            CALL CHR_PUTD( DMAXV, BUFFER, NC )
            CALL MSG_OUT( 'EXTREMES', BUFFER( :NC ), STATUS )

*  Set the default values.
            DRDEF( 1 ) = DMINV
            DRDEF( 2 ) = DMAXV
         ELSE

*  Extreme values have not been calculated, therefore have no suggested
*  default.
            DRDEF( 1 ) = VAL__BADD
            DRDEF( 2 ) = VAL__BADD
         END IF

*  Set the extreme values.  Use the full range of numbers when there are
*  no bad values.
         IF( BAD ) THEN
            DRMIN = VAL__MIND
            DRMAX = VAL__MAXD
         ELSE
            DRMIN = NUM__MIND
            DRMAX = NUM__MAXD
         END IF

*  Use an appropriate type.  Deal with signed integer data.
      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN

         IF( COMEXT ) THEN

*  Obtain the maximum and minimum values to define the bounds of the
*  histogram.
            CALL KPG1_MXMNI( BAD, EL, %VAL( PNTR( 1 ) ), NINVAL,
     :                       IMAXV, IMINV, MAXPOS, MINPOS, STATUS )

*  Report the extreme values.
            NC = 0
            CALL CHR_PUTC( 'Minimum value is ', BUFFER, NC )
            CALL CHR_PUTI( IMINV, BUFFER, NC )
            CALL CHR_PUTC( ' and the maximum is ', BUFFER, NC )
            CALL CHR_PUTI( IMAXV, BUFFER, NC )
            CALL MSG_OUT( 'EXTREMES', BUFFER( :NC ), STATUS )

*  Set the default values.
            DRDEF( 1 ) = DBLE( IMINV )
            DRDEF( 2 ) = DBLE( IMAXV )
         ELSE

*  Extreme values have not been calculated, therefore have no suggested
*  default.
            DRDEF( 1 ) = DBLE( VAL__BADI )
            DRDEF( 2 ) = DBLE( VAL__BADI )
         END IF

*  Set the extreme values.  Use the full range of numbers when there are
*  no bad values.
         IF( BAD ) THEN
            DRMIN = NUM_ITOD( VAL__MINI )
            DRMAX = NUM_ITOD( VAL__MAXI )
         ELSE
            DRMIN = NUM_ITOD( NUM__MINI )
            DRMAX = NUM_ITOD( NUM__MAXI )
         END IF

*  Use an appropriate type.  Deal with single-precision real data.
      ELSE IF( TYPE .EQ. '_REAL' ) THEN

         IF( COMEXT ) THEN

*  Obtain the maximum and minimum values to define the bounds of the
*  histogram.
            CALL KPG1_MXMNR( BAD, EL, %VAL( PNTR( 1 ) ), NINVAL,
     :                       RMAXV, RMINV, MAXPOS, MINPOS, STATUS )

*  Report the extreme values.
            NC = 0
            CALL CHR_PUTC( 'Minimum value is ', BUFFER, NC )
            CALL CHR_PUTR( RMINV, BUFFER, NC )
            CALL CHR_PUTC( ' and the maximum is ', BUFFER, NC )
            CALL CHR_PUTR( RMAXV, BUFFER, NC )
            CALL MSG_OUT( 'EXTREMES', BUFFER( :NC ), STATUS )

*  Set the default values.
            DRDEF( 1 ) = DBLE( RMINV )
            DRDEF( 2 ) = DBLE( RMAXV )
         ELSE

*  Extreme values have not been calculated, therefore have no suggested
*  default.
            DRDEF( 1 ) = DBLE( VAL__BADR )
            DRDEF( 2 ) = DBLE( VAL__BADR )
         END IF

*  Set the extreme values.  Use the full range of numbers when there are
*  no bad values.
         IF( BAD ) THEN
            DRMIN = DBLE( VAL__MINR )
            DRMAX = DBLE( VAL__MAXR )
         ELSE
            DRMIN = DBLE( NUM__MINR )
            DRMAX = DBLE( NUM__MAXR )
         END IF

*  Use an appropriate type.  Deal with unsigned-byte data.
      ELSE IF( TYPE .EQ. '_UBYTE' ) THEN

         IF( COMEXT ) THEN

*  Obtain the maximum and minimum values to define the bounds of the
*  histogram.
            CALL KPG1_MXMNUB( BAD, EL, %VAL( PNTR( 1 ) ), NINVAL,
     :                        BMAXV, BMINV, MAXPOS, MINPOS, STATUS )

*  Report the extreme values.
            NC = 0
            CALL CHR_PUTC( 'Minimum value is ', BUFFER, NC )
            CALL CHR_PUTI( NUM_UBTOI( BMINV ), BUFFER, NC )
            CALL CHR_PUTC( ' and the maximum is ', BUFFER, NC )
            CALL CHR_PUTI( NUM_UBTOI( BMAXV ), BUFFER, NC )
            CALL MSG_OUT( 'EXTREMES', BUFFER( :NC ), STATUS )

*  Set the default values.
            DRDEF( 1 ) = NUM_UBTOD( BMINV )
            DRDEF( 2 ) = NUM_UBTOD( BMAXV )
         ELSE

*  Extreme values have not been calculated, therefore have no suggested
*  default.
            DRDEF( 1 ) = DBLE( VAL__BADI )
            DRDEF( 2 ) = DBLE( VAL__BADI )
         END IF

*  Set the extreme values.  Use the full range of numbers when there are
*  no bad values.
         IF( BAD ) THEN
            DRMIN = NUM_UBTOD( VAL__MINUB )
            DRMAX = NUM_UBTOD( VAL__MAXUB )
         ELSE
            DRMIN = NUM_UBTOD( NUM__MINUB )
            DRMAX = NUM_UBTOD( NUM__MAXUB )
         END IF

*  Use an appropriate type.  Deal with unsigned-word data.
      ELSE IF( TYPE .EQ. '_UWORD' ) THEN

         IF( COMEXT ) THEN

*  Obtain the maximum and minimum values to define the bounds of the
*  histogram.
            CALL KPG1_MXMNUW( BAD, EL, %VAL( PNTR( 1 ) ), NINVAL,
     :                        WMAXV, WMINV, MAXPOS, MINPOS, STATUS )

*  Report the extreme values.
            NC = 0
            CALL CHR_PUTC( 'Minimum value is ', BUFFER, NC )
            CALL CHR_PUTI( NUM_UWTOI( WMINV ), BUFFER, NC )
            CALL CHR_PUTC( ' and the maximum is ', BUFFER, NC )
            CALL CHR_PUTI( NUM_UWTOI( WMAXV ), BUFFER, NC )
            CALL MSG_OUT( 'EXTREMES', BUFFER( :NC ), STATUS )

*  Set the default values.
            DRDEF( 1 ) = NUM_UWTOD( WMINV )
            DRDEF( 2 ) = NUM_UWTOD( WMAXV )
         ELSE

*  Extreme values have not been calculated, therefore have no suggested
*  default.
            DRDEF( 1 ) = DBLE( VAL__BADI )
            DRDEF( 2 ) = DBLE( VAL__BADI )
         END IF

*  Set the extreme values.  Use the full range of numbers when there are
*  no bad values.
         IF( BAD ) THEN
            DRMIN = NUM_UWTOD( VAL__MINUW )
            DRMAX = NUM_UWTOD( VAL__MAXUW )
         ELSE
            DRMIN = NUM_UWTOD( NUM__MINUW )
            DRMAX = NUM_UWTOD( NUM__MAXUW )
         END IF

*  Use an appropriate type.  Deal with signed-word data.
      ELSE IF( TYPE .EQ. '_WORD' ) THEN

         IF( COMEXT ) THEN

*  Obtain the maximum and minimum values to define the bounds of the
*  histogram.
            CALL KPG1_MXMNW( BAD, EL, %VAL( PNTR( 1 ) ), NINVAL,
     :                       WMAXV, WMINV, MAXPOS, MINPOS, STATUS )

*  Report the extreme values.
            NC = 0
            CALL CHR_PUTC( 'Minimum value is ', BUFFER, NC )
            CALL CHR_PUTI( NUM_WTOI( WMINV ), BUFFER, NC )
            CALL CHR_PUTC( ' and the maximum is ', BUFFER, NC )
            CALL CHR_PUTI( NUM_WTOI( WMAXV ), BUFFER, NC )
            CALL MSG_OUT( 'EXTREMES', BUFFER( :NC ), STATUS )

*  Set the default values.
            DRDEF( 1 ) = NUM_WTOD( WMINV )
            DRDEF( 2 ) = NUM_WTOD( WMAXV )
         ELSE

*  Extreme values have not been calculated, therefore have no suggested
*  default.
            DRDEF( 1 ) = DBLE( VAL__BADI )
            DRDEF( 2 ) = DBLE( VAL__BADI )
         END IF

*  Set the extreme values.  Use the full range of numbers when there are
*  no bad values.
         IF( BAD ) THEN
            DRMIN = NUM_WTOD( VAL__MINW )
            DRMAX = NUM_WTOD( VAL__MAXW )
         ELSE
            DRMIN = NUM_WTOD( NUM__MINW )
            DRMAX = NUM_WTOD( NUM__MAXW )
         END IF

      END IF

*  Obtain the limits of the histogram, not constrained by the extreme
*  values though. (This may be needed for histogram bin boundaries
*  to occur at round values.)
      CALL PAR_GDR1D( 'RANGE', 2, DRDEF, DRMIN, DRMAX, .TRUE., DRANGE,
     :                STATUS )

*  Sort if necessary.
      IF( DRANGE( 1 ) .GT. DRANGE( 2 ) ) THEN
         DDUMMY = DRANGE( 1 )
         DRANGE( 1 ) = DRANGE( 2 )
         DRANGE( 2 ) = DDUMMY
      END IF

*  Get the number of histogram bins to be used, within a sensible
*  range.
      CALL PAR_GDR0I( 'NUMBIN', 20, 2, MAXBIN, .TRUE., NUMBIN, STATUS )
      IF( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain an optional file for logging the results.
*  ================================================
      CALL ERR_MARK
      LOGFIL = .FALSE.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 132, IFIL, STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         LOGFIL = .TRUE.
      END IF
      CALL ERR_RLSE

*  Write the heading of the histogram report.
*  ==========================================

*  Display the NDF name, also sending it to the logfile if necessary.
      CALL MSG_BLANK( STATUS )
      IF( LOGFIL ) CALL FIO_WRITE( IFIL, ' ', STATUS )
      CALL NDF_MSG( 'NDF', INDF1 )
      CALL MSG_LOAD( 'NDFNAME',
     :               '   Histogram for the NDF structure ^NDF', BUFFER,
     :               NC, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUFFER( : NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
         IF( LOGFIL ) CALL FIO_WRITE( IFIL, BUFFER( : NC ), STATUS )
      END IF

*  Display (and log) the NDF's title.
      CALL MSG_BLANK( STATUS )
      IF( LOGFIL ) CALL FIO_WRITE( IFIL, ' ', STATUS )
      CALL NDF_CMSG( 'TITLE', INDF1, 'Title', STATUS )
      CALL MSG_LOAD( 'NDFTITLE',
     :               '      Title                     : ^TITLE',
     :               BUFFER, NC, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUFFER( : NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
         IF( LOGFIL ) CALL FIO_WRITE( IFIL, BUFFER( : NC ), STATUS )
      END IF

*  Display (and log) the name of the component being analysed.
      CALL MSG_SETC( 'COMP', MCOMP )
      CALL MSG_LOAD( 'NDFCOMP',
     :               '      NDF array analysed        : ^COMP',
     :               BUFFER, NC, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUFFER( : NC ) )
         CALL MSG_OUT( ' ', '^MESSAGE', STATUS )
         IF( LOGFIL ) CALL FIO_WRITE( IFIL, BUFFER( : NC ), STATUS )
      END IF

*  If a logfile is in use, display its name.
      IF( LOGFIL ) CALL MSG_OUT( 'LOG',
     :              '      Logging to file           : $LOGFILE',
     :                            STATUS )

*    Obtain workspace for the histogram.
      CALL PSX_CALLOC( NUMBIN, '_INTEGER', HPNTR, STATUS )

*  Compute and report the histogram.
*  =================================

*  Call the appropriate routines to compute the histogram and then
*  report the results.  The double-precision range values must be
*  converted to the appropriate type for the routines.
      IF( TYPE .EQ. '_BYTE' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTB( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                    NUM_DTOB( DRANGE( 2 ) ),
     :                    NUM_DTOB( DRANGE( 1 ) ), %VAL( HPNTR ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
 
*  Compute the histogram.
         CALL KPG1_GHSTD( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                    DRANGE( 2 ), DRANGE( 1 ), %VAL( HPNTR ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSD( NUMBIN, %VAL( HPNTR ), DRANGE( 1 ),
     :                    DRANGE( 2 ), STATUS )

*  Write the histogram to the log file.
         IF( LOGFIL ) THEN
            CALL KPG1_HSFLD( IFIL, NUMBIN, %VAL( HPNTR ), DRANGE( 1 ),
     :                       DRANGE( 2 ), STATUS )
         END IF

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
 
*  Compute the histogram.
         CALL KPG1_GHSTI( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                    NUM_DTOI( DRANGE( 2 ) ),
     :                    NUM_DTOI( DRANGE( 1 ) ), %VAL( HPNTR ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF( TYPE .EQ. '_REAL' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTR( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                    REAL( DRANGE( 2 ) ), REAL( DRANGE( 1 ) ),
     :                    %VAL( HPNTR ), STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
 
*  Compute the histogram.
         CALL KPG1_GHSTUB( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                     NUM_DTOUB( DRANGE( 2 ) ),
     :                     NUM_DTOUB( DRANGE( 1 ) ), %VAL( HPNTR ),
     :                     STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF( TYPE .EQ. '_UWORD' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTUW( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                     NUM_DTOUW( DRANGE( 2 ) ),
     :                     NUM_DTOUW( DRANGE( 1 ) ), %VAL( HPNTR ),
     :                     STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF( TYPE .EQ. '_WORD' ) THEN
 
*  Compute the histogram.
         CALL KPG1_GHSTW( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                    NUM_DTOW( DRANGE( 2 ) ),
     :                    NUM_DTOW( DRANGE( 1 ) ), %VAL( HPNTR ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      END IF

*  Obtain the axis and plot styles.
*  ================================
*  Construct the default label for the X axis.
      CALL KPG1_NDFNM( INDF1, NDFNAM, NMLEN, STATUS )
      CALL MSG_SETC( 'NDF', NDFNAM )
      CALL MSG_LOAD( ' ', 'Data value in ^NDF', XL, LENXL, 
     :               STATUS )

*  Construct the default label for the Y axis.
      YL = 'Count'

*  Are the axes logarithmic?
      CALL PAR_GTD0L( 'XLOG', .FALSE., .TRUE., XLOG, STATUS )
      CALL PAR_GTD0L( 'YLOG', .FALSE., .TRUE., YLOG, STATUS )

*  Allow for case where x-axis is logarithmic, and some data is zero
*  or negative.
      IF( XLOG ) THEN    
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( 'Log\\d10\\u(', TEXT, IAT )
         CALL CHR_APPND( XL, TEXT, IAT )
         CALL CHR_APPND( ')', TEXT, IAT ) 
         XL = TEXT
      ENDIF

      IF( YLOG ) THEN
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( 'Log\\d10\\u(', TEXT, IAT )
         CALL CHR_APPND( YL, TEXT, IAT )
         CALL CHR_APPND( ')', TEXT, IAT ) 
         YL = TEXT
      ENDIF

*  Derive the data ranges.
*  =======================

*  Find the range of the numbers in the histogram bins.  Empty bins
*  will be filled with zero, so no need to check for bad values.
      BAD = .FALSE.
      CALL KPG1_MXMNI( BAD, NUMBIN, %VAL( HPNTR ), NINVAL,
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

      IF( STATUS .EQ. SAI__OK ) THEN

*  Get the x-y points at the centre of each bin in the histogram.
         CALL KPG1_HSTLO( NUMBIN, %VAL( HPNTR ), MINIM, MAXIM, 
     :                    XLOG, YLOG, %VAL( HPPTR1 ), %VAL( HPPTR2 ), 
     :                    STATUS )

*  Plot the histogram.
*  ===================

*  Plot the locus just computed within annotated axes.  Both axes'
*  limits are defined. Use a default value of 0.0 for the bottom of the
*  vertical axis.
         CALL KPG1_GRAPH( NUMBIN, %VAL( HPPTR1 ), %VAL( HPPTR2 ), 
     :                    0.0, 0.0,  XL, YL, 'Histogram plot', 'XDATA',
     :                    'YDATA', 1, .TRUE., VAL__BADR, VAL__BADR, 
     :                    0.0, VAL__BADR, 'KAPPA_HISTOGRAM', .TRUE., 
     :                    IPLOT, STATUS )  

*  If anything was plotted, annul the Plot, and shut down the graphics 
*  workstation and database.
         IF( IPLOT .NE. AST__NULL ) THEN
            CALL AST_ANNUL( IPLOT, STATUS )
            CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )
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
      CALL LPG_CREAT( 'OUT', '_INTEGER', 1, 1, NUMBIN, INDF2, STATUS )

*  Map the data array.
      CALL NDF_MAP( INDF2, 'Data', '_INTEGER', 'WRITE', OUTPTR,
     :              NUMBIN, STATUS )

*  Write the slice to the NDF.
      CALL VEC_ITOI( .FALSE., NUMBIN, %VAL( HPNTR ),
     :               %VAL( OUTPTR( 1 ) ), IERR, NERR, STATUS )

*  Unmap the histogram.
      CALL NDF_UNMAP( INDF2, 'Data', STATUS )

*  There are no bad pixels in the histogram, so record this fact in
*  the NDF, for greater efficiency.
      CALL NDF_SBAD( .FALSE., INDF2, 'DATA', STATUS )

*  Get the title for the NDF.
      CALL NDF_CINP( 'TITLE', INDF2, 'TITLE', STATUS )

*  Write a label for the NDF.  The histogram is unitless.
      CALL NDF_CPUT( 'Number', INDF2, 'Label', STATUS )

*  Obtain the label and units from the input NDF.
      LABEL = ' '
      UNITS = ' '
      CALL NDF_CGET( INDF1, 'Label', LABEL, STATUS )
      CALL NDF_CGET( INDF1, 'Units', UNITS, STATUS )

*  Put the label and units in the axis structure, which is
*  also created at this point.  There is only one axis.
      IF( LABEL .NE. ' ' ) CALL NDF_ACPUT( LABEL, INDF2, 'Label', 1, 
     :                                     STATUS )
      IF( UNITS .NE. ' ' ) CALL NDF_ACPUT( UNITS, INDF2, 'Units', 1, 
     :                                     STATUS )

*  Map the axis centres.
      CALL NDF_AMAP( INDF2, 'CENTRE', 1, '_REAL', 'WRITE', AXPNTR,
     :               NUMBIN, STATUS )

*  Fill the axis array with the centres of the histogram bins by first
*  finding the bin width.  This assumes an even distribution of points
*  within the bin which is probably not the case, but the difference is
*  only likely to be serious when the number of non-empty bins is low.
      BINWID = ( MAXIM - MINIM ) / REAL( NUMBIN )
      CALL KPG1_SSCOF( NUMBIN, DBLE( BINWID ),
     :                 DBLE( MINIM + 0.5 * BINWID ),
     :                 %VAL( AXPNTR( 1 ) ), STATUS )

*  Handle the null case invisibly.
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

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

*  Close the logfile, if used.
      IF( LOGFIL ) CALL FIO_ANNUL( IFIL, STATUS )

*  If an error occurred, then report a contextual message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HISTOGRAM_ERR', 'HISTOGRAM: Error computing '//
     :                 'or displaying the histogram of an NDF''s '//
     :                 'pixels.', STATUS )
      END IF

      END
