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
*     ABSLAB = LITERAL (Read)
*        Label for the plot's abscissa.  NCAR fancy founts may be
*        embedded when FONT = "NCAR".  Only the first 50 characters are
*        used.  If axis information is present in the NDF the suggested
*        default is the NDF's axis label followed by the units, in
*        parentheses.  If an error occurs obtaining the label or there
*        is no axis information, the label takes its current value,
*        which initially is "COMP values", where COMP is the value of
*        parameter COMP.  []
*     CLEAR = _LOGICAL (Read)
*        Determines if the graphics workstation is to be cleared before
*        producing the plot. [TRUE]
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
*        is null, !, there will be no plot made.
*        [Current graphics device]
*     FONT = LITERAL (Read)
*        The fount to be used for the histogram plot.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the standard
*        GKS san-serif fount.  The former is intended for hardcopy
*        publication-quality plots, since it is relatively slow; the
*        latter is intended for normal interactive graphics requiring
*        rapid plotting, and it is clearer on small plots.  The
*        suggested default is the current value. ["GKS"]
*     IN = NDF (Read)
*        The NDF data structure to be analysed.
*     LOGFILE = FILENAME (Write)
*        A text file into which the results should be logged. If a null
*        value is supplied (the default), then no logging of results
*        will take place. [!]
*     MAJTIC( 2 ) = _REAL (Read)
*        The parameter controlling the numbers of major tick marks
*        for the x and y axes.  (Number used is between MAJTIC+2 and
*        5*MAJTIC/2+4.) [4.,4.]
*     MINTIC( 2 ) = _REAL (Read)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values. [-1.,-1.]
*     NUMBIN = _INTEGER (Read)
*        The number of histogram bins to be used. This must lie in the
*        range 2 to 10000.  The suggested default is the current value.
*     ORDLAB = LITERAL (Read)
*        A label for the plot's ordinate axis.  NCAR fancy founts may be
*        embedded when FONT = "NCAR".  Only the first 50 characters are
*        used.  ["Number"]
*     OUT = NDF (Read)
*        Name of the NDF structure to save the histogram in its data
*        array.  If null, !, is entered the histogram NDF is not
*        created. [!]
*     OUTTIC = _LOGICAL (Read)
*        TRUE if the axis tick marks are to appear on the outside of
*        the axes instead of inside. [FALSE]
*     PLTITL = LITERAL (Read)
*        A title for the histogram plot, in which NCAR fancy founts may
*        be embedded.  The default is to use the NDF's title if present
*        and no error occurs, otherwise the current value becomes the
*        suggested default.  This is initially "Histogram".  Only the
*        first 50 characters are used.  []
*     PXSIZE = _REAL (Read)
*        The length (x axis) of the plot in metres. [Maximum that can
*        fit in the current picture whilst preserving square pixels]
*     PYSIZE = _REAL (Read)
*        The length (y axis) of the plot in metres. [Maximum that can
*        fit in the current picture whilst preserving square pixels]
*     RANGE( 2 ) = _DOUBLE (Write)
*        The range of values for which the histogram is to be computed.
*        A null value (!) selects the minimum and maximum array values.
*        If RANGE is specified on the command line, the extreme values
*        are not calculated and reported.  The suggested defaults are
*        the current values, or ! if these do not exist.
*     THICK = _REAL (Read)
*        The thickness of the axes and annotations in the histogram
*        plots, where 1.0 is the normal thickness.  Currently,
*        this is only available on a few devices.  It must take a value
*        in the range 0.5--5.0. [1.0]
*     TITLE = LITERAL (Read)
*        Title for the histogram NDF.  ["KAPPA - Histogram"]
*     XLOG = _LOGICAL (Read)
*        TRUE if the plot abscissa is to be logarithmic.  It is
*        unlikely that you would want to do this.  By default, the
*        abscissa is linear. [FALSE]
*     YLOG = _LOGICAL (Read)
*        TRUE if the plot ordinate is to be logarithmic.  By default,
*        the ordinate is linear.  [FALSE]

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
*     histogram cube numbin=32 ! device=xwindows
*        Computes and reports the histogram for the data array in
*        the NDF called cube.  The histogram has 32 bins and spans the
*        full range of data values.  A plot of the histogram is made to
*        the XWINDOWS device.
*     histogram cube numbin=32 ! device=xwindows ylog pltitl="Taurus 2"
*        As in the previous example except the logarithm of the number
*        in each histogram bin is plotted, and the plot title is
*        "Taurus 2".
*     histogram halley(~200,~300) range=[-1000,1000] logfile=hist.dat \
*        Computes the histogram for the central 200 by 300 elements of
*        the data array in the NDF called halley, and writes the
*        results to a logfile called hist.dat.  The histogram uses the
*        current number of bins, and includes data values between -1000
*        and 1000.  A plot appears on the current graphics device.

*  Related Applications:
*     KAPPA: HISTAT, INSPECT, MSTATS, NUMB, STATS; Figaro: HIST, ISTAT.

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
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error codes
      INCLUDE 'NDF_PAR'          ! NDF__ public constants
      INCLUDE 'PRM_PAR'          ! VAL__ primitive data constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL SNX_AGGUX
      EXTERNAL SNX_AGGUY
      REAL SNX_AGGUX             ! Converts NCAR x grid co-ordinate to
                                 ! world co-ordinate
      REAL SNX_AGGUY             ! Converts NCAR y grid co-ordinate to
                                 ! world co-ordinate

*  Local Constants:
      INTEGER MAXBIN             ! Maximum number of histogram bins
      PARAMETER( MAXBIN = 10000 )! 

      INTEGER SZBUF              ! Size of text buffer
      PARAMETER ( SZBUF = 132 )

      REAL YNULL                 ! Bin height for an empty bin in a
                                 ! histogram with a log ordinate
      PARAMETER ( YNULL = 0.8 )  ! Must be in range 0.5 to 1.0

*  Local Variables:
      BYTE BMAXV                 ! Max. value of pixels in array
      BYTE BMINV                 ! Min. value of pixels in array
      
      CHARACTER * ( 72 ) ABSLAB  ! Label for the plot abscissa
      CHARACTER * ( SZBUF ) BUFFER ! Text buffer
      CHARACTER * ( 8 ) COMP     ! Name of array component to analyse
      CHARACTER * ( 4 ) FOUNT    ! Fount type
      CHARACTER * ( 256 ) LABEL  ! Label of the histogram NDF
      CHARACTER * ( 8 ) MCOMP    ! Component name for mapping arrays
      CHARACTER * ( 72 ) ORDLAB  ! Label for the plot ordinate
      CHARACTER * ( 72 ) PLTITL  ! Title of the plot
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric type for processing
      CHARACTER * ( 256 ) UNITS  ! Units of the histogram NDF

      DOUBLE PRECISION DDUMMY    ! Dummy for swapping the data range
      DOUBLE PRECISION DMAXV     ! Max. value of pixels in array
      DOUBLE PRECISION DMINV     ! Min. value of pixels in array
      DOUBLE PRECISION DRDEF( 2 )! Defaults for data range
      DOUBLE PRECISION DRMAX     ! Maximum value for the range
      DOUBLE PRECISION DRMIN     ! Minimum value for the range
      DOUBLE PRECISION DRANGE( 2 ) ! Data range of the histogram

      INTEGER ACTRNG             ! State of the RANGE parameter
      INTEGER AXPNTR( 1 )        ! Pointer to the histogram axis centres
      INTEGER EL                 ! Number of array elements mapped
      INTEGER HPNTR              ! Pointer to the histogram
      INTEGER HPPTR1             ! Pointer to the histogram x locus
      INTEGER HPPTR2             ! Pointer to the histogram y locus
      INTEGER IERR               ! Position of first conversion error
      INTEGER IFIL               ! File descriptor for logfile
      INTEGER IMAXV              ! Max. value of pixels in array
      INTEGER IMINV              ! Min. value of pixels in array
      INTEGER MAXH               ! Maximum number in an histogram bin
      INTEGER MAXPOS             ! Index of maximum-valued pixel
      INTEGER MINH               ! Minimum number in an histogram bin
      INTEGER MINPOS             ! Index of minimum-valued pixel
      INTEGER NC                 ! No. characters in text buffer
      INTEGER NDF                ! Identifier for input NDF
      INTEGER NDFO               ! NDF identifier of output histogram
      INTEGER NERR               ! Number of conversion errors
      INTEGER NINVAL             ! No. invalid pixels in array
      INTEGER NPOS               ! Number of points in histogram locus
      INTEGER NUMBIN             ! Number of histogram bins
      INTEGER OUTPTR( 1 )        ! Pointer to output NDF histogram
      INTEGER PICID              ! Identifier of input picture
      INTEGER PICIDD             ! Identifier of DATA picture
      INTEGER PICIDF             ! Identifier of FRAME picture
      INTEGER PNTR( 1 )          ! Pointer to mapped NDF array
      INTEGER ZONEF              ! SGS zone of FRAME picture

      INTEGER * 2 WMAXV          ! Max. value of pixels in array
      INTEGER * 2 WMINV          ! Min. value of pixels in array

                                 ! True if:
      LOGICAL BAD                ! There may be bad values in the array
      LOGICAL COMEXT             ! RANGE parameter is not active
      LOGICAL GRAPHS             ! Graphics device is open and a plot
                                 ! is to be produced
      LOGICAL LOGFIL             ! Log file is required
      LOGICAL OUTTIC             ! Axis tick marks are to be placed
                                 ! outside the box instead of inside
      LOGICAL THERE              ! Array component exists
      LOGICAL TCKCTR             ! The number of tick marks cannot be
                                 ! controlled
      LOGICAL XLOG               ! Abscissa of plot is logarithmic
      LOGICAL YLOG               ! Ordinate of plot is logarithmic

      REAL ABSLOW                ! Lower abscissa limit
      REAL ABSUPP                ! Upper abscissa limit
      REAL BINWID                ! Histogram bin width
      REAL MAJTIC( 2 )           ! Parameters controlling the numbers
                                 ! of major tick marks along x and y
                                 ! axes respectively
      REAL MAXIM                 ! Max. value of pixels in array
                                 ! standardised for locus
      REAL MINIM                 ! Min. value of pixels in array
                                 ! standardised for locus
      REAL MINTIC( 2 )           ! Numbers of minor tick marks along x
                                 ! and y axes respectively
      REAL NULL                  ! NCAR null value
      REAL ORDLOW                ! Lower ordinate limit
      REAL ORDUPP                ! Upper ordinate limit
      REAL RMAXV                 ! Max. value of pixels in array
      REAL RMINV                 ! Min. value of pixels in array
      REAL THICK                 ! The line thickness (standard is 1.0)
      REAL TICDEF( 2 )           ! Suggested default axis-tick values
      REAL XLOW                  ! x lower limit of the plot
      REAL XHIGH                 ! x upper limit of the plot
      REAL YLOW                  ! y lower limit of the plot
      REAL YHIGH                 ! y upper limit of the plot

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise whether or not the logfile is required.
      LOGFIL = .FALSE.

*  Obtain the NDF to be analysed.
      CALL NDG_ASSOCL( 'IN', 'READ', NDF, STATUS )

*  Determine which array component is to be analysed.
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Error,Quality,Variance',
     :                .TRUE., COMP, STATUS )

*  Most NDF routines with a component argument don't recognise 'ERROR',
*  so we need two variables.  Thus convert 'ERROR' into 'VARIANCE' in
*  the variable needed for such routines.  The original value is held
*  in a variable with the prefix M for mapping, as one of the few
*  routines that does support 'ERROR' is NDF_MAP, but is also needed for
*  the abscissa label.
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Check that the required component exists and report an error,
*  including the NDF's name, if it does not.
      CALL NDF_STATE( NDF, COMP, THERE, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. THERE ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'COMP', COMP )
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'HISTOGRAM_NOCOMP',
     :     'The ^COMP component is undefined in the NDF structure ^NDF',
     :     STATUS )
      END IF

*  Obtain the numeric type of the NDF array component to be analysed.
      CALL NDF_TYPE( NDF, COMP, TYPE, STATUS )

*  Map the array using this numeric type and see whether there may be
*  bad pixels present.
      CALL KPG1_MAP( NDF, MCOMP, TYPE, 'READ', PNTR, EL, STATUS )
      IF ( COMP .EQ. 'QUALITY' ) THEN
         BAD = .FALSE.
      ELSE
         CALL NDF_BAD( NDF, COMP, .FALSE., BAD, STATUS )
      END IF

*  Exit if something has gone wrong.  Good status is required before
*  obtaining the range.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the parameters of the histogram.
*  =======================================
*
*  Obtain the range of values first; later get the number of bins.
*  Only compute the extreme values and report them when parameter RANGE
*  is not specified on the command line.
      CALL NDG_STATE( 'RANGE', ACTRNG, STATUS )
      COMEXT = ACTRNG .NE. PAR__ACTIVE

*  We need to find the minimum and maximum to assist the user.  This
*  requires the mapped array.  Now RANGE parameter is double precision,
*  so the default and limiting values must be converted from the
*  implementation type to real.
*  
*  Use an appropriate type.  Deal with signed-byte data.
      IF ( TYPE .EQ. '_BYTE' ) THEN

         IF ( COMEXT ) THEN

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
         IF ( BAD ) THEN
            DRMIN = NUM_BTOD( VAL__MINB )
            DRMAX = NUM_BTOD( VAL__MAXB )
         ELSE
            DRMIN = NUM_BTOD( NUM__MINB )
            DRMAX = NUM_BTOD( NUM__MAXB )
         END IF

*  Use an appropriate type.  Deal with double-precision data.
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN

         IF ( COMEXT ) THEN

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
         IF ( BAD ) THEN
            DRMIN = VAL__MIND
            DRMAX = VAL__MAXD
         ELSE
            DRMIN = NUM__MIND
            DRMAX = NUM__MAXD
         END IF

*  Use an appropriate type.  Deal with signed integer data.
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN

         IF ( COMEXT ) THEN

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
         IF ( BAD ) THEN
            DRMIN = NUM_ITOD( VAL__MINI )
            DRMAX = NUM_ITOD( VAL__MAXI )
         ELSE
            DRMIN = NUM_ITOD( NUM__MINI )
            DRMAX = NUM_ITOD( NUM__MAXI )
         END IF

*  Use an appropriate type.  Deal with single-precision real data.
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN

         IF ( COMEXT ) THEN

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
         IF ( BAD ) THEN
            DRMIN = DBLE( VAL__MINR )
            DRMAX = DBLE( VAL__MAXR )
         ELSE
            DRMIN = DBLE( NUM__MINR )
            DRMAX = DBLE( NUM__MAXR )
         END IF

*  Use an appropriate type.  Deal with unsigned-byte data.
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN

         IF ( COMEXT ) THEN

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
         IF ( BAD ) THEN
            DRMIN = NUM_UBTOD( VAL__MINUB )
            DRMAX = NUM_UBTOD( VAL__MAXUB )
         ELSE
            DRMIN = NUM_UBTOD( NUM__MINUB )
            DRMAX = NUM_UBTOD( NUM__MAXUB )
         END IF

*  Use an appropriate type.  Deal with unsigned-word data.
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN

         IF ( COMEXT ) THEN

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
         IF ( BAD ) THEN
            DRMIN = NUM_UWTOD( VAL__MINUW )
            DRMAX = NUM_UWTOD( VAL__MAXUW )
         ELSE
            DRMIN = NUM_UWTOD( NUM__MINUW )
            DRMAX = NUM_UWTOD( NUM__MAXUW )
         END IF

*  Use an appropriate type.  Deal with signed-word data.
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN

         IF ( COMEXT ) THEN

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
         IF ( BAD ) THEN
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
      IF ( DRANGE( 1 ) .GT. DRANGE( 2 ) ) THEN
         DDUMMY = DRANGE( 1 )
         DRANGE( 1 ) = DRANGE( 2 )
         DRANGE( 2 ) = DDUMMY
      END IF

*  Get the number of histogram bins to be used, within a sensible
*  range.
      CALL PAR_GDR0I( 'NUMBIN', 20, 2, MAXBIN, .TRUE., NUMBIN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

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
      CALL NDF_MSG( 'NDF', NDF )
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
      CALL NDF_CMSG( 'TITLE', NDF, 'Title', STATUS )
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

*  Call the appropriate routines to compute the histogram and then
*  report the results.  The double-precision range values must be
*  converted to the appropriate type for the routines.
      IF ( TYPE .EQ. '_BYTE' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTB( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                    NUM_DTOB( DRANGE( 2 ) ),
     :                    NUM_DTOB( DRANGE( 1 ) ), %VAL( HPNTR ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
 
*  Compute the histogram.
         CALL KPG1_GHSTD( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                    DRANGE( 2 ), DRANGE( 1 ), %VAL( HPNTR ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSD( NUMBIN, %VAL( HPNTR ), DRANGE( 1 ),
     :                    DRANGE( 2 ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLD( IFIL, NUMBIN, %VAL( HPNTR ), DRANGE( 1 ),
     :                       DRANGE( 2 ), STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
 
*  Compute the histogram.
         CALL KPG1_GHSTI( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                    NUM_DTOI( DRANGE( 2 ) ),
     :                    NUM_DTOI( DRANGE( 1 ) ), %VAL( HPNTR ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTR( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                    REAL( DRANGE( 2 ) ), REAL( DRANGE( 1 ) ),
     :                    %VAL( HPNTR ), STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
 
*  Compute the histogram.
         CALL KPG1_GHSTUB( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                     NUM_DTOUB( DRANGE( 2 ) ),
     :                     NUM_DTOUB( DRANGE( 1 ) ), %VAL( HPNTR ),
     :                     STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN

*  Compute the histogram.
         CALL KPG1_GHSTUW( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                     NUM_DTOUW( DRANGE( 2 ) ),
     :                     NUM_DTOUW( DRANGE( 1 ) ), %VAL( HPNTR ),
     :                     STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
 
*  Compute the histogram.
         CALL KPG1_GHSTW( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN,
     :                    NUM_DTOW( DRANGE( 2 ) ),
     :                    NUM_DTOW( DRANGE( 1 ) ), %VAL( HPNTR ),
     :                    STATUS )

*  Report the histogram.
         CALL KPG1_HSDSR( NUMBIN, %VAL( HPNTR ), REAL( DRANGE( 1 ) ),
     :                    REAL( DRANGE( 2 ) ), STATUS )

*  Write the histogram to the log file.
         IF ( LOGFIL ) THEN
            CALL KPG1_HSFLR( IFIL, NUMBIN, %VAL( HPNTR ),
     :                       REAL( DRANGE( 1 ) ), REAL( DRANGE( 2 ) ),
     :                       STATUS )
         END IF

      END IF

*  Start the graphics system.
*  ==========================

*  Open the device and create a frame picture in the database, unless
*  a null was entered, in which case no plotting is required.  This
*  is tested via a returned switch.  The routine also gets AUTOGRAPH
*  to use the SGS zone.
      CALL NCROPN( 'CLEAR', 'DEVICE', 'PXSIZE', 'PYSIZE',
     :             'KAPPA_HISTOGRAM', GRAPHS, PICID, PICIDF,
     :             ZONEF, STATUS )

      IF ( GRAPHS ) THEN

*  Obtain the axis and plot styles.
*  ================================

*  Obtain a title for the plot, using the NDF title as the default.
         CALL KPG1_GNTIT( NDF, 'PLTITL', 'Histogram', PLTITL, STATUS )

*  Get the abscissa label suggesting the value in the NDF label
*  and units components, if present, as the default, otherwise
*  the component name followed by values is used.
         CALL KPG1_GNLBU( NDF, 'ABSLAB', MCOMP, ABSLAB, STATUS )

*  Obtain the ordinate axis label.
         CALL PAR_DEF0C( 'ORDLAB', 'Number', STATUS )
         CALL PAR_GET0C( 'ORDLAB', ORDLAB, STATUS )

*  Are the axes logarithmic?
         CALL PAR_GTD0L( 'XLOG', .FALSE., .TRUE., XLOG, STATUS )
         CALL PAR_GTD0L( 'YLOG', .FALSE., .TRUE., YLOG, STATUS )
         TCKCTR = XLOG .AND. YLOG

*  Tick numbers are not altered for a logarithmic axis.
         IF ( .NOT. TCKCTR ) THEN

*  Get the number of minor ticks, assigning the dynamic defaults.
            TICDEF( 1 ) = -1.
            TICDEF( 2 ) = -1.
            CALL PAR_GDR1R( 'MINTIC', 2, TICDEF, -1., VAL__MAXR,
     :                      .FALSE., MINTIC, STATUS )

*  Get the parameter controlling the number of major ticks per
*  axis, assigning the dynamic defaults.
            TICDEF( 1 ) = 3.
            TICDEF( 2 ) = 3.
            CALL PAR_GDR1R( 'MAJTIC', 2, TICDEF, -1., VAL__MAXR,
     :                      .FALSE., MAJTIC, STATUS )
         END IF

*  Are the tick marks on the outside of the axes?
         CALL PAR_GTD0L( 'OUTTIC', .FALSE., .TRUE., OUTTIC, STATUS )

*  Get the line thickness.
         CALL PAR_GDR0R( 'THICK', 1.0, 0.5, 5.0, .TRUE., THICK, STATUS )

*  Get the fount.  Although NCAR is the default, either must be
*  selected to prevent persistence from earlier invocations.
         CALL PAR_CHOIC( 'FONT', 'GKS', 'GKS,NCAR', .TRUE., FOUNT,
     :                   STATUS )
         IF ( FOUNT .EQ. 'GKS ' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )
         ELSE IF ( FOUNT .EQ. 'NCAR' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
         END IF

*  Derive the data ranges.
*  =======================

*  Find the range of the numbers in the histogram bins.  Empty bins
*  will be filled with zero, so no need to check for bad values.
         BAD = .FALSE.
         CALL KPG1_MXMNI( BAD, NUMBIN, %VAL( HPNTR ), NINVAL,
     :                    MAXH, MINH, MAXPOS, MINPOS, STATUS )

*  Obtain a range for the abscissa, but since graphics is involved
*  single-precision floating-point limits are required.
         MAXIM = REAL( DRANGE( 2 ) )
         MINIM = REAL( DRANGE( 1 ) )

*  Generate the histogram locus.
*  =============================

*  Create work space for storing the histogram locus. NPOS gives the
*  number of points in the locus.
         NPOS = 2 * ( NUMBIN + 1 ) + ( NUMBIN - 1 ) * 3
         CALL PSX_CALLOC( NPOS, '_REAL', HPPTR1, STATUS )
         CALL PSX_CALLOC( NPOS, '_REAL', HPPTR2, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Find the NCAR null value.
            CALL AGGETF( 'NULL/1.', NULL )

*  Form the locus of x-y points that is the histogram.
            CALL KPG1_HSTLO( NUMBIN, %VAL( HPNTR ), MINIM, MAXIM, NULL,
     :                       XLOG, YLOG, YNULL, NPOS, %VAL( HPPTR1 ),
     :                       %VAL( HPPTR2 ), STATUS )

 
*  Find the limits of the plot.
*  ============================

*  First x.
            IF ( XLOG ) THEN
               IF ( MINIM .LE. 0.0 ) THEN

*  Note in this case KPG1_HSTLO uses bin numbers.
                  XLOW = 0.5
                  XHIGH = 0.5 + REAL( NUMBIN )
                  CALL MSG_OUT( 'BINNOS', 'Some of the data used to '/
     :              /'form the histogram are zero or negative. Thus '/
     :              /'the abscissa is now bin numbers.', STATUS )
                  ABSLAB = 'Bin number'
               ELSE
                  XLOW = MINIM
                  XHIGH = MAXIM
               END IF
            ELSE
               XLOW = MINIM
               XHIGH = MAXIM
            END IF

*  Now y.  Allow some space above the highest count of the histogram
*  bins.
            IF ( YLOG ) THEN
               YLOW = YNULL
               YHIGH = INT( 1.05 * MAXH ) + 1
            ELSE
               YLOW = 0.0
               YHIGH = INT( 1.05 * MAXH ) + 1
            END IF

*  Plot the histogram.
*  ===================

*  Plot the locus just computed within annotated axes.  Both axes'
*  limits are defined.
            CALL LINPLT( %VAL( HPPTR1 ), %VAL( HPPTR2 ), NPOS, .TRUE.,
     :                   .TRUE., XLOW, YLOW, XHIGH, YHIGH, PLTITL,
     :                   ABSLAB, ORDLAB, MINTIC, MAJTIC, XLOG, YLOG,
     :                   OUTTIC, THICK, STATUS )

            CALL SGS_FLUSH

*  Define the DATA picture's world co-ordinates.
*  =============================================

*  Set the world co-ordinates that will be stored in the database.

            IF ( .NOT. XLOG ) THEN

*  Get the lower and upper abscissa limits at the left and right of the
*  NCAR grid.
               ABSLOW = SNX_AGGUX( 0.0 )
               ABSUPP = SNX_AGGUX( 1.0 )
            ELSE
               ABSLOW = LOG10( SNX_AGGUX( 0.0 ) )
               ABSUPP = LOG10( SNX_AGGUX( 1.0 ) )
            END IF

            IF ( .NOT. YLOG ) THEN

*  Get the lower and upper ordinate limits at the bottom and top of the
*  NCAR grid.
               ORDLOW = SNX_AGGUY( 0.0 )
               ORDUPP = SNX_AGGUY( 1.0 )
            ELSE
               ORDLOW = LOG10( SNX_AGGUY( 0.0 ) )
               ORDUPP = LOG10( SNX_AGGUY( 1.0 ) )
            END IF

            CALL SGS_SW( ABSLOW, ABSUPP, ORDLOW, ORDUPP, STATUS )
      
*  Record the data picture in the database.
*  ========================================
            CALL KPG1_SDTRN( 'KAPPA_HISTOGRAM', NDF, PICIDD, STATUS )

*  End of check for no error getting workspace to plot histogram.
         END IF

*  Tidy work-space structures.
         CALL PSX_FREE( HPPTR1, STATUS )
         CALL PSX_FREE( HPPTR2, STATUS )

*  Close the graphics system.
         CALL AGS_DEASS( 'DEVICE', .FALSE., STATUS )

*  End of graphics-output check.
      END IF

*  Create an output NDF.
*  =====================

*  Start a new error context.
      CALL ERR_MARK

*  Start a new NDF context.  Note that KPG1_CPNTI is inadequate, as it
*  does return the NDF identifier.
      CALL NDF_BEGIN

*  Create a new NDF.
      CALL NDG_CREATL( 'OUT', '_INTEGER', 1, 1, NUMBIN, NDFO, STATUS )

*  Map the data array.
      CALL KPG1_MAP( NDFO, 'Data', '_INTEGER', 'WRITE', OUTPTR,
     :              NUMBIN, STATUS )
      
*  Write the slice to the NDF.
      CALL VEC_ITOI( .FALSE., NUMBIN, %VAL( HPNTR ),
     :               %VAL( OUTPTR( 1 ) ), IERR, NERR, STATUS )

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
      CALL NDF_CGET( NDF, 'Label', LABEL, STATUS )
      CALL NDF_CGET( NDF, 'Units', UNITS, STATUS )

*  Put the label and units in the axis structure, which is
*  also created at this point.  There is only one axis.
      CALL NDF_ACPUT( LABEL, NDFO, 'Label', 1, STATUS )
      CALL NDF_ACPUT( UNITS, NDFO, 'Units', 1, STATUS )

*  Map the axis centres.
      CALL NDF_AMAP( NDFO, 'CENTRE', 1, '_REAL', 'WRITE', AXPNTR,
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

*  Close the logfile, if used.
      IF ( LOGFIL ) CALL FIO_ANNUL( IFIL, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HISTOGRAM_ERR',
     :     'HISTOGRAM: Error computing or displaying the histogram '/
     :     /'of an NDF''s pixels.', STATUS )
      END IF

      END
