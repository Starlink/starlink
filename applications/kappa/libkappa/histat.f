      SUBROUTINE HISTAT( STATUS )
*+
*  Name:
*     HISTAT

*  Purpose:
*     Computes ordered statistics for an NDF's pixels using an
*     histogram.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL HISTAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application computes and displays simple ordered statistics
*     for the pixels in an NDF's data, quality, error, or variance
*     array.  The statistics available are:
*     -  the pixel sum,
*     -  the pixel mean,
*     -  the pixel median,
*     -  the pixel mode,
*     -  the pixel value at selected percentiles,
*     -  the value and position of the minimum- and maximum-valued
*     pixels,
*     -  the total number of pixels in the NDF,
*     -  the number of pixels used in the statistics, and
*     -  the number of pixels omitted.

*  Usage:
*     histat ndf [comp] [percentiles] [logfile] [numbin]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The name of the NDF array component for which statistics are
*        required.  The options are limited to the arrays within the
*        supplied NDF.  In general the value may "Data", "Error",
*        "Quality" or "Variance" (note that "Error" is the alternative
*        to "Variance" and causes the square root of the variance
*        values to be taken before computing the statistics).  If
*        "Quality" is specified, then the quality values are treated as
*        numerical values (in the range 0 to 255).  ["Data"]
*     LOGFILE = FILENAME (Write)
*        A text file into which the results should be logged.  If a null
*        value is supplied (the default), then no logging of results
*        will take place. [!]
*     MAXCOORD( ) = _DOUBLE (Write)
*        A 1-dimensional array of values giving the user co-ordinates of
*        the centre of the (first) maximum-valued pixel found in the
*        NDF array.  The number of co-ordinates is equal to the number
*        of NDF dimensions.
*     MAXIMUM = _DOUBLE (Write)
*        The maximum pixel value found in the NDF array.
*     MAXPOS( ) = _INTEGER (Write)
*        A 1-dimensional array of pixel indices identifying the (first)
*        maximum-valued pixel found in the NDF array.  The number of
*        indices is equal to the number of NDF dimensions.
*     MEAN = _DOUBLE (Write)
*        The mean value of all the valid pixels in the NDF array.
*     MEDIAN = _DOUBLE (Write)
*        The median value of all the valid pixels in the NDF array.
*     MINCOORD( ) = _DOUBLE (Write)
*        A 1-dimensional array of values giving the user co-ordinates of
*        the centre of the (first) minimum-valued pixel found in the
*        NDF array.  The number of co-ordinates is equal to the number
*        of NDF dimensions.
*     MINIMUM = _DOUBLE (Write)
*        The minimum pixel value found in the NDF array.
*     MINPOS( ) = _INTEGER (Write)
*        A 1-dimensional array of pixel indices identifying the (first)
*        minimum-valued pixel found in the NDF array. The number of
*        indices is equal to the number of NDF dimensions.
*     MODE = _DOUBLE (Write)
*        The modal value of all the valid pixels in the NDF array.  It
*        is estimated from 3 * median - 2 * mean.  This is only valid
*        for moderately skew distributions.
*     NDF = NDF (Read)
*        The NDF data structure to be analysed.
*     NUMBAD = _INTEGER (Write)
*        The number of pixels which were either not valid or were
*        rejected from the statistics during iterative K-sigma
*        clipping.
*     NUMBIN = _INTEGER (Read)
*        The number of histogram bins to be used.  There is a tradeoff
*        between accuracy and processing time depending on the value
*        of NUMBIN. Increasing the number of bins yields more accurate
*        results, because it reduces quantisation errors biasing
*        the computed statistics, but takes longer to compute.  The
*        bias is in the same sense as the skewness, so generally it
*        will add positive bias.  The default value of NUMBIN is a
*        compromise and generally will give satisfactory results.
*
*        It is hard to quantify the tradeoff precisely; testing with a
*        typical CCD image of stars and galaxies the increase in time
*        went approximately as log of the number of bins, and the
*        accuracy of the pixel sum for some values of NUMBIN were: 100,
*        2.8%; 1000, 0.1% 10000, 0.03%; and 100000, 0.002%.
*
*        NUMBIN must be in the range 100 to 100000. [2048]
*     NUMGOOD = _INTEGER (Write)
*        The number of NDF pixels which actually contributed to the
*        computed statistics.
*     NUMPIX = _INTEGER (Write)
*        The total number of pixels in the NDF (both good and bad).
*     PERCENTILES( 100 ) = _REAL (Read)
*         A list of percentiles to be found.  None are computed if this
*         parameter is null (!).  The percentiles must be in the range
*         0.0 to 100.0 [!]
*     PERVAL() = _DOUBLE (Write)
*         The values of the percentiles of the good pixels in the NDF
*         array.  This parameter is only written when one or more
*         percentiles have been requested.
*     TOTAL = _DOUBLE (Write)
*        The sum of the pixel values in the NDF array.

*  Examples:
*     histat image
*        Computes and displays simple ordered statistics for the data
*        array in the NDF called image.
*     histat ndf=spectrum variance
*        Computes and displays simple ordered statistics for the
*        variance array in the NDF called spectrum.
*     histat spectrum error
*        Computes and displays ordered statistics for the variance
*        array in the NDF called spectrum, but takes the square root of
*        the variance values before doing so.
*     histat halley logfile=stats.dat
*        Computes ordered statistics for the data array in the NDF
*        called halley, and writes the results to a logfile called
*        stats.dat.
*     histat ngc1333 percentiles=[0.25,0.75] numbin=100000
*        Computes ordered statistics for the data array in the NDF
*        called ngc1333, including the quartile values.  The highest
*        accuracy is used.

*  Notes:
*     -  If the array has a few extreme outliers this can bias the
*     statistics unless the number of bins in the histogram is large,
*     or the outliers are removed (flagged) via application THRESH.
*     -  There is quantisation bias in the statistics.  See parameter
*     NUMBIN.

*  Related Applications:
*     KAPPA: HISTOGRAM, INSPECT, MSTATS, NDFTRACE, NUMB, STATS;
*     Figaro: ISTAT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     QUALITY, TITLE, and HISTORY components of the NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single- or double-precision floating point,
*     as appropriate.
*     -  Any number of NDF dimensions is supported.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 November 13 (MJC):
*        Original NDF version.
*     1994 September 27 (MJC):
*        Replaced AIF calls with PAR, FIO, and PSX.  Made messages
*        conditional and used modern names for subroutines (_$ to 1_).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'MSG_PAR'          ! Message constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXBIN             ! Maximum number of histogram bins
      PARAMETER( MAXBIN = 100000 )! 

      INTEGER NPRCTL             ! Maximum number of percentiles
      PARAMETER( NPRCTL = 100 )

      INTEGER SZBUF              ! Size of text buffer
      PARAMETER ( SZBUF = 132 )

*  Local Variables:
      LOGICAL BAD                ! There may be bad values in the array
      CHARACTER * ( SZBUF ) BUF  ! Text buffer
      CHARACTER * ( 8 ) COMP     ! Name of array component to analyse
      DOUBLE PRECISION DMAX      ! Max. value of pixels in array
      DOUBLE PRECISION DMIN      ! Min. value of pixels in array
      LOGICAL DOPRCT             ! Percentiles have been supplied
      INTEGER EL                 ! Number of array elements mapped
      INTEGER HPNTR              ! Pointer to the histogram
      INTEGER I                  ! Loop counter for percentiles
      INTEGER IFIL               ! File descriptor for logfile
      INTEGER IMAX( 1 )          ! Vector index of max. pixel
      INTEGER IMIN( 1 )          ! Vector index of min. pixel
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      LOGICAL LOGFIL             ! Log file is required
      DOUBLE PRECISION MAXC( NDF__MXDIM ) ! Co-ordinates of max. pixel
      CHARACTER * ( 8 ) MCOMP    ! Component name for mapping arrays
      INTEGER MAXP( NDF__MXDIM ) ! Indices of maximum-valued pixel
      INTEGER MINP( NDF__MXDIM ) ! Indices of minimum-valued pixel
      DOUBLE PRECISION MEAN      ! Mean of pixels in array
      DOUBLE PRECISION MEDIAN    ! Median of pixels in array
      DOUBLE PRECISION MINC( NDF__MXDIM ) ! Co-ordinates of min. pixel
      DOUBLE PRECISION MODE      ! Mode of pixels in array
      INTEGER NC                 ! No. characters in text buffer
      INTEGER NDF                ! NDF identifier
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NGOOD              ! No. valid pixels in array
      INTEGER NUMBIN             ! Number of histogram bins
      INTEGER NUMPER             ! Number of percentiles
      REAL PERCNT( NPRCTL )      ! Percentiles
      DOUBLE PRECISION PERVAL( NPRCTL ) ! Values at the percentiles
      INTEGER PNTR( 1 )          ! Pointer to mapped NDF array
      DOUBLE PRECISION STDEV     ! Standard devn. of pixels in array
      DOUBLE PRECISION SUM       ! Sum of pixels in array
      LOGICAL THERE              ! Array component exists
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric type for processing
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise whether or not the logfile is required.
      LOGFIL = .FALSE.

*  Obtain the NDF to be analysed.
      CALL LPG_ASSOC( 'NDF', 'READ', NDF, STATUS )

*  Determine which array component is to be analysed, converting
*  'ERROR' into 'VARIANCE'.
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Error,Quality,Variance',
     :                .FALSE., COMP, STATUS )
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Check that the required component exists and report an error,
*  including the NDF's name, if it does not.
      CALL NDF_STATE( NDF, COMP, THERE, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. THERE ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'COMP', COMP )
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'HISTAT_NOCOMP',
     :     'The ^COMP component is undefined in the NDF structure ^NDF',
     :     STATUS )
      END IF

*  Exit if something has gone wrong.  Good status is required before
*  obtaining the percentiles.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      
*  Defer error reporting and obtain an array of percentiles to be
*  calculated. Constrain the values to be between the minimum and
*  maximum data values.
      CALL ERR_MARK
      CALL PAR_GDRVR( 'PERCENTILES', NPRCTL, 0.0, 100.0, PERCNT, NUMPER,
     :                STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN

*  Null is a valid response to say do not compute percentiles.  Make
*  the number of percentiles one and flag the value, so that the
*  display routines can handle and recognise there are no percentile
*  values to report.
         DOPRCT = .FALSE.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            NUMPER = 1
            PERCNT( 1 ) = VAL__BADR
         END IF
         
      ELSE
         DOPRCT = .TRUE.
      END IF
      CALL ERR_RLSE

*  Get the number of histogram bins to be used, within a sensible
*  range.
      CALL PAR_GDR0I( 'NUMBIN', 2048, 100, MAXBIN, .TRUE., NUMBIN,
     :                STATUS )

*  Obtain an optional file for logging the results.  Start a new error
*  context as null is a valid value.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 132, IFIL, STATUS )

*  Null is a valid response to say do create a logfile.  Record this
*  fact.  CLose the new error context/.
      IF ( STATUS .NE. SAI__OK ) THEN

         LOGFIL = .FALSE.
         IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      ELSE
         LOGFIL = .TRUE.
      END IF
      CALL ERR_RLSE

*  Display the NDF name, also sending it to the logfile if necessary.
*  Cannot use MSG_BLANK because the message is optional.
      CALL MSG_OUTIF( MSG__NORM, ' ', ' ', STATUS )
      IF ( LOGFIL ) CALL FIO_WRITE( IFIL, ' ', STATUS )
      CALL NDF_MSG( 'NDF', NDF )
      CALL MSG_LOAD( 'NDFNAME',
     :               '   Pixel ordered statistics for the NDF '/
     :               /'structure ^NDF', BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( : NC ) )
         CALL MSG_OUTIF( MSG__NORM, ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
      END IF

*  Display (and log) the NDF's title.  Cannot use MSG_BLANK because
*  the message is optional.
      CALL MSG_OUTIF( MSG__NORM, ' ', ' ', STATUS )
      IF ( LOGFIL ) CALL FIO_WRITE( IFIL, ' ', STATUS )
      CALL NDF_CMSG( 'TITLE', NDF, 'Title', STATUS )
      CALL MSG_LOAD( 'NDFTITLE',
     :               '      Title                     : ^TITLE',
     :               BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( : NC ) )
         CALL MSG_OUTIF( MSG__NORM, ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
      END IF

*  Display (and log) the name of the component being analysed.
      CALL MSG_SETC( 'COMP', MCOMP )
      CALL MSG_LOAD( 'NDFCOMP',
     :               '      NDF array analysed        : ^COMP',
     :               BUF, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'MESSAGE', BUF( : NC ) )
         CALL MSG_OUTIF( MSG__NORM, ' ', '^MESSAGE', STATUS )
         IF ( LOGFIL ) CALL FIO_WRITE( IFIL, BUF( : NC ), STATUS )
      END IF

*  If a logfile is in use, display its name.
      IF ( LOGFIL ) CALL MSG_OUTIF( MSG__NORM, 'LOG',
     :              '      Logging to file           : $LOGFILE',
     :                            STATUS )

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

*    Obtain workspace for the histogram.
      CALL PSX_CALLOC( NUMBIN, '_INTEGER', HPNTR, STATUS )
      
*  Call the appropriate routine to compute the histogram and hence the
*  statistics.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_HSTAB( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN, NUMPER,
     :                    PERCNT, NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                    DMAX, SUM, MEAN, MEDIAN, MODE, PERVAL,
     :                    %VAL( HPNTR ), STATUS )
 
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_HSTAUB( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN, NUMPER,
     :                     PERCNT, NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                     DMAX, SUM, MEAN, MEDIAN, MODE, PERVAL,
     :                     %VAL( HPNTR ), STATUS )
 
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_HSTAD( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN, NUMPER,
     :                    PERCNT, NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                    DMAX, SUM, MEAN, MEDIAN, MODE, PERVAL,
     :                    %VAL( HPNTR ), STATUS )
 
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_HSTAI( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN, NUMPER,
     :                    PERCNT, NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                    DMAX, SUM, MEAN, MEDIAN, MODE, PERVAL,
     :                    %VAL( HPNTR ), STATUS )
 
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPG1_HSTAR( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN, NUMPER,
     :                    PERCNT, NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                    DMAX, SUM, MEAN, MEDIAN, MODE, PERVAL,
     :                    %VAL( HPNTR ), STATUS )
 
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL KPG1_HSTAW( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN, NUMPER,
     :                    PERCNT, NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                    DMAX, SUM, MEAN, MEDIAN, MODE, PERVAL,
     :                    %VAL( HPNTR ), STATUS )
 
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_HSTAUW( BAD, EL, %VAL( PNTR( 1 ) ), NUMBIN, NUMPER,
     :                     PERCNT, NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ),
     :                     DMAX, SUM, MEAN, MEDIAN, MODE, PERVAL,
     :                     %VAL( HPNTR ), STATUS )
      END IF

*  Free the workspace.
      CALL PSX_FREE( HPNTR, STATUS )

*  Obtain the NDF bounds and initialise the indices of the minimum and
*  maximim pixel positions and co-ordinates.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      DO 1 I = 1, NDIM
         MINP( I ) = VAL__BADI
         MAXP( I ) = VAL__BADI
         MINC( I ) = VAL__BADD
         MAXC( I ) = VAL__BADD
    1 CONTINUE

*  If available, convert the minimum and maximum pixel locations into
*  N-dimensional indices and then into co-ordinate values.
      IF ( NGOOD .NE. 0 ) THEN
         CALL KPG1_VEC2N( 1, IMIN, NDIM, LBND, UBND, MINP, STATUS )
         CALL KPG1_VEC2N( 1, IMAX, NDIM, LBND, UBND, MAXP, STATUS )
         CALL KPG1_PX2AX( NDIM, MINP, NDF, MINC, STATUS )
         CALL KPG1_PX2AX( NDIM, MAXP, NDF, MAXC, STATUS )
      END IF

*  The standard deviation is undefined, but needs to be passed to the
*  display routines, so give it the flagged value.
      STDEV = VAL__BADD

*  Display the statistics, using the most appropriate floating-point
*  precision.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_STDSD( NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                    DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                    MEDIAN, MODE, NUMPER, PERCNT, PERVAL,
     :                    STATUS )
      ELSE
         CALL KPG1_STDSR( NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                    DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                    MEDIAN, MODE, NUMPER, PERCNT, PERVAL,
     :                    STATUS )
      END IF

*  Also write the statistics to the logfile, if used.
      IF ( LOGFIL ) THEN
         IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_STFLD( NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                       DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                       MEDIAN, MODE, NUMPER, PERCNT, PERVAL,
     :                       IFIL, STATUS )
         ELSE
            CALL KPG1_STFLR( NDIM, EL, NGOOD, DMIN, MINP, MINC,
     :                       DMAX, MAXP, MAXC, SUM, MEAN, STDEV,
     :                       MEDIAN, MODE, NUMPER, PERCNT, PERVAL,
     :                       IFIL, STATUS )
         END IF
      END IF

*  Write the final results to the output parameters.
      CALL PAR_PUT0D( 'MAXIMUM', DMAX, STATUS )
      CALL PAR_PUT0D( 'MEAN', MEAN, STATUS )
      CALL PAR_PUT0D( 'MEDIAN', MEDIAN, STATUS )
      CALL PAR_PUT0D( 'MODE', MODE, STATUS )
      CALL PAR_PUT0D( 'MINIMUM', DMIN, STATUS )
      CALL PAR_PUT0D( 'TOTAL', SUM, STATUS )
      CALL PAR_PUT0I( 'NUMBAD', EL - NGOOD, STATUS )
      CALL PAR_PUT0I( 'NUMGOOD', NGOOD, STATUS )
      CALL PAR_PUT0I( 'NUMPIX', EL, STATUS )
      CALL PAR_PUT1D( 'MAXCOORD', NDIM, MAXC, STATUS )
      CALL PAR_PUT1D( 'MINCOORD', NDIM, MINC, STATUS )
      CALL PAR_PUT1I( 'MAXPOS', NDIM, MAXP, STATUS )
      CALL PAR_PUT1I( 'MINPOS', NDIM, MINP, STATUS )

*  Only write percentiles values if any percentiles were given.
      IF ( DOPRCT ) CALL PAR_PUT1D( 'PERVAL', NUMPER, PERVAL, STATUS )

*  Arrive here if an error occurs.
  999 CONTINUE     

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Close the logfile, if used.
      IF ( LOGFIL ) CALL FIO_CLOSE( IFIL, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HISTAT_ERR',
     :     'HISTAT: Error computing simple ordered statistics for an '/
     :     /'NDF''s pixels.', STATUS )
      END IF

      END
