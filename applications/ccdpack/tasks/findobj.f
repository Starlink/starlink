      SUBROUTINE FINDOBJ( STATUS )
*+
*  Name:
*     FINDOBJ

*  Purpose:
*     Locates and centroids image features.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FINDOBJ( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine processes a list of NDFs, locating and centroiding
*     image features (such as stars) which have groups of connected
*     pixels above threshold values.
*
*     Connected groups of pixels are accepted as objects if they have
*     more than a minimum number of pixels. Such groups may be rejected
*     if they contact the edges of the data array.
*
*     Threshold estimation is performed using either a percentage data
*     point (i.e. the value for which this percentage of pixels have a
*     lower value) or by using a standard deviation and background
*     value determined by fitting a gaussian to the data histogram.

*  Usage:
*     findobj in minpix outlist

*  ADAM Parameters:
*     AUTOTHRESH = _LOGICAL (Read)
*        If this parameter is TRUE then a threshold determined by
*        this routine for each of the NDFs will be used. If FALSE then
*        you will be prompted for a threshold value for each NDF.
*        [TRUE]
*     BINFRAC = _DOUBLE (Read)
*        The minimum fraction of the image area (expressed as a
*        percentage) which is required in the peak bin when forming the
*        histogram.  Ensuring that at least one bin contains this
*        fraction of counts is intended to make sure that the image
*        histogram is well sampled. This increases the robustness of
*        mode estimates made from the histogram but decreases the
*        accuracy.
*        [2.5]
*     IN = LITERAL (Read)
*        A list of NDF names which contain the data components to be
*        scanned for image features.  The NDF names should be separated
*        by commas and may include wildcards.
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP,
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
*     MINPIX = _INTEGER (Read)
*        The minimum number of non-BAD pixels which must be present in
*        a connected group for acceptance as an image feature.
*        [6]
*     NAMELIST = LITERAL (Read)
*        The name of a file to contain the names of the output
*        position lists. The names written to this file are those
*        generated using the expression given to the OUTLIST parameter.
*        The file may be used in an indirection expression to input
*        all the position lists output from this routine into another
*        routine.
*        [FINDOBJ.LIS]
*     NSIGMA = _DOUBLE (Read)
*        The number of standard deviations above the background that
*        should be used as the threshold. This parameter is only
*        accessed if the USEPER parameter is FALSE and a gaussian is
*        being fitted to the background.
*        [5]
*     OUTLIST = LITERAL (Read)
*        The names of the output lists. 
*
*        These may be specified as list of comma separated names, 
*        using indirection if required, OR, as a single modification 
*        element (of the input NDF names). The simplest modification
*        element is the asterisk "*" which means call each of the
*        output lists the same name as the corresponding input NDFs 
*        (but without the ".sdf" extension).
*        So,
*           IN > *
*           OUTLIST > *
*        signifies that all the NDFs in the current directory should be
*        used and the output lists should have the same names.
*
*        Other types of modification can also occur, such as,
*           OUTLIST > *_objs.dat
*        which means call the position lists the same as the input NDFs
*        but put "_objs.dat" after the names. Replacement of a specified
*        string with another in the output file names can also be used,
*           OUTLIST > *|_debias|_images.dat|
*        this replaces the string "_debias" with "_images.dat" in any
*        of the output names.
*
*        If wildcarded names for the input NDFs are used then is it
*        recommended that wildcards are also used for the position list
*        names (the order of input names is not guaranteed).
*
*        The output files contain a integer index for each image
*        feature followed by the X and Y centroid (formed using all the
*        intensity information) and finally the mean intensity of
*        pixels in the group.
*        [*.DAT]
*     OVERSAMP = _INTEGER (Read)
*        An oversampling factor which is used when forming the initial
*        histogram (greater than 1). The oversample is estimated by
*        making the initial histogram mean count OVERSAMP times
*        smaller than the mean count which would give BINFRAC in every
*        bin. Increasing the oversample will increase the probability
*        that only one bin will meet the BINFRAC criterion.
*        [5]
*     PERCENTILE = _DOUBLE (Read)
*        The percentage point in the data histogram which is to be used
*        as the threshold estimate.  For data which has a significant
*        background count this value should always be much greater than
*        50 (the median) and probably greater than the upper quantile
*        (75). Only used if USEPER is TRUE.
*        [96]
*     THRESH = _DOUBLE (Read)
*        The threshold which is to be used for detecting image
*        features.  Connected pixel groups above this threshold form
*        image features. This parameter is only used if the AUTOTHRESH
*        parameter is set FALSE. In this case a value may be supplied
*        for each NDF which is being processed.
*        [Dynamic default]
*     TOUCH = _LOGICAL (Read)
*        If TRUE then pixel groups may contact the edges of the data
*        array. Contact is defined as any pixel in the connected group
*        of pixels being on the first or last column or row of the
*        actual data array (not including any NDF origin information).
*        Setting this FALSE decreases the probability of incomplete
*        pixel groups being centroided which would result in inaccurate
*        positions.
*        [FALSE]
*     USEPER = _LOGICAL (Read)
*        If TRUE then a percentage point (of the total counts) in the
*        histogram will be used to estimate the threshold. Otherwise a
*        gaussian fit to the data histogram will be used to estimate the
*        background value.
*        [TRUE]

*  Examples:
*     findobj in='*' minpix=10 outlist='*.find'
*        In this example FINDOBJ processes all the NDFs in the current
*        directory locating objects with connected pixel groups which
*        have more than 9 pixels above the threshold.
*
*     findobj '"ndf1,ndf2,ndf10"' 6 '"obj1.dat,obj2.dat,obj3.dat"'
*             useper=false nsigma=3
*        In this example FINDOBJ estimates the threshold using the mode
*        value in the histogram of data values as an estimate of the
*        background and the fit of a gaussian to this to estimate the
*        background standard deviation. The threshold used for each NDF
*        is then 3 times the standard deviation above the estimated
*        background.

*  Notes:
*     - Threshold estimation.

*       FINDOBJ is optimised to determine a reliable detection
*       threshold and is not concerned with the accurate 
*       determination of the background value on a frame (as it 
*       performs no photometric measurements). For this reason the 
*       histogram which it uses to determine the background value is 
*       made in such a way that it is usually very well sampled 
*       (probably oversampled, for most other purposes). FINDOBJ 
*       should not be used in a manner for which it is not suited 
*       without understanding how if differs from other more 
*       specialized routines.
*
*     - Histogram formation and gaussian fitting. 
*
*       The histogram used by FINDOBJ is formed by (if necessary) 
*       re-binning until the BINFRAC criterion is met, it is expected
*       that this will always result in a well sampled histogram. The 
*       background value is the mode of this histogram and is not 
*       refined during the gaussian fitting. The gaussian fitting just
*       estimates the standard deviation of the background and uses a 
*       fixed peak value and position (the mode of the histogram) and 
*       iterates rejecting bins whose counts fall below 20 percent of 
*       the peak value, stopping when either 3 iterations have been 
*       performed or the standard deviation does not change by more
*       than one bin width in data values.
*
*     - NDF extension items. 
*
*       On exit the CURRENT_LIST items in the CCDPACK extensions
*       (.MORE.CCDPACK) of the input NDFs are set to the names of the 
*       appropriate output lists. These items will be used by other
*       CCDPACK position list processing routines to automatically 
*       access the lists.
*
*     - Output position list format.
*
*       CCDPACK format - Position lists in CCDPACK are formatted files
*       whose first three columns are interpreted as the following.
*
*          - Column 1: an integer identifier
*          - Column 2: the X position
*          - Column 3: the Y position
*
*       The column one value must be an integer and is used to identify
*       positions which may have different locations but are to be
*       considered as the same point. Comments may be included in the
*       file using the characters # and !. Columns may be separated by
*       the use of commas or spaces.

*  Behaviour of parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*     The exception to this rule is:
*        - THRESH  -- dynamic value
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when re-using the
*     application after a break of sometime. The intrinsic default
*     behaviour of the application may be restored by using the RESET
*     keyword on the command line.
*
*     Certain parameters (LOGTO and LOGFILE) have global values. These
*     global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands.

*  Implementation Status:
*     - This routine correctly processes the DATA and QUALITY components
*       of an NDF data structure. Bad pixels and all non-complex numeric
*       data types can be handled.

*  Implementation Deficiencies:
*     -  There is no support positions other than in pixel coordinates.
*     -  No use is made of variance information.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-OCT-1992 (PDRAPER):
*        Original version.
*     9-NOV-1992 (PDRAPER):
*        Added threshold estimation.
*     6-OCT-1995 (PDRAPER):
*        Updated for CCDPACK 2.0.
*     25-APR-1996 (PDRAPER):
*        Added trap and error message for situation when no pixels
*        are above the threshold.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator control (foreign data access upgrades).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameterisations
      INCLUDE 'PAR_ERR'          ! Parameter system error codes
      INCLUDE 'NDF_PAR'          ! NDF parameterisations
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'FIO_PAR'          ! FIO system parameterisations

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL FIO_TEST
      LOGICAL FIO_TEST           ! Checks general fio error conditions

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Character buffer for writing
                                      ! results
      CHARACTER * ( FIO__SZFNM ) FNAME ! Position list file name
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Input data type
      DOUBLE PRECISION BINPER    ! Percent of count in one bin (please)
      DOUBLE PRECISION NSIGMA    ! Number of sigmas above background
      DOUBLE PRECISION PERCEN    ! Percentile of threshold
      DOUBLE PRECISION SD        ! Background standard deviation
      DOUBLE PRECISION THRESH    ! Detection threshold
      DOUBLE PRECISION TR( 6 )   ! Linear tranformations coefficients
      DOUBLE PRECISION WIDTH     ! Width of histogram bin
      DOUBLE PRECISION ZERO      ! Zero point of histogram bins
      INTEGER EL                 ! Number of pixels in input NDF
      INTEGER FDOUT              ! FIO system file descriptor
      INTEGER FIOGRP             ! Output file names group
      INTEGER IDIN               ! Input NDF identifier
      INTEGER INDEX              ! Loop counter
      INTEGER IPCON              ! Number of contributions
      INTEGER IPGRP              ! Pointer to pixel groups 
      INTEGER IPHIST             ! Pointer to histogram array
      INTEGER IPIN               ! Pointer to input data array
      INTEGER IPINT              ! Pointer to pixel intensities
      INTEGER IPMIN              ! Pointer to mean intensities
      INTEGER IPSUM1             ! Centroid sums
      INTEGER IPSUM2             ! Centroid sums
      INTEGER IPX                ! Pointer to pixel positions
      INTEGER IPXC               ! X centroid positions
      INTEGER IPY                ! Pointer to pixel positions
      INTEGER IPYC               ! Y centroid positions
      INTEGER LBND( 2 )          ! NDF lower pixel bounds
      INTEGER MINBIN             ! Minimum number of counts in one bin
      INTEGER MINPIX             ! Minimum number of pixels in group
      INTEGER MODE               ! Bin with maximum count
      INTEGER NABOVE             ! Number of pixels above threshold
      INTEGER NBIN               ! Number of histogram bins used.
      INTEGER NDFGRP             ! Input NDF Group identifier
      INTEGER NDIM               ! Dimensionality of NDF
      INTEGER NEED               ! Number of bins required for
                                 ! oversampling
      INTEGER NNDF               ! Number of input NDFs
      INTEGER NOBJ               ! Number of images found
      INTEGER NOUT               ! Number of output positions
      INTEGER NRET               ! Number of returns
      INTEGER OVSAMP             ! Oversampling factor
      INTEGER PEAK               ! Number of counts in peak bin
      INTEGER UBND( 2 )          ! NDF upper bounds
      INTEGER XDIM               ! First dimension of input NDF
      INTEGER YDIM               ! Second dimension of input NDF
      LOGICAL AUTOTH             ! True if user will allow
                                 ! auto-thresholding
      LOGICAL BAD                ! Whether BAD pixels are present 
      LOGICAL TOUCH              ! Whether pixels group can touch the
                                 ! edges of the array or not
      LOGICAL USEPER             ! Whether to use percentiles to
                                 ! estimates threshold

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up CCDPACK.
      CALL CCD1_START( 'FINDOBJ', STATUS )

*  Get a list of NDFs.
      CALL NDF_BEGIN
      CALL CCD1_NDFGR( 'IN', 'UPDATE', NDFGRP, NNDF, STATUS )

*  Get the names of the corresponding lists of positions.
      CALL CCD1_STRGR( 'OUTLIST', NDFGRP, NNDF, NNDF, FIOGRP, NRET,
     :                 STATUS )

*  Find out if the user wants to specify the threshold which is
*  used on each loop.
      CALL PAR_GET0L( 'AUTOTHRESH', AUTOTH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Process each of the NDFs in turn.
      DO 1 INDEX = 1, NNDF

*  Access the NDF.
         CALL IRG_NDFEX( NDFGRP, INDEX, IDIN, STATUS )

*  Write informational message about it.
         CALL CCD1_MSG( ' ',  ' ', STATUS )
         CALL NDF_MSG( 'CURRENT_NDF', IDIN )
         CALL CCD1_MSG( ' ', '  +++ Processing NDF: ^CURRENT_NDF',
     :                  STATUS )

*  Inform user how many NDFs we've processed out of the total number.
         CALL MSG_SETI( 'CURRENT_NUM', INDEX )
         CALL MSG_SETI( 'MAX_NUM', NNDF )
         CALL CCD1_MSG( ' ', '  (Number ^CURRENT_NUM of ^MAX_NUM)',
     :                  STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
 
*  Get the bounds of the NDF.
         CALL NDF_BOUND( IDIN, 2, LBND, UBND, NDIM, STATUS )

*  Work out the array dimensions and number of pixels in array.
         XDIM = UBND( 1 ) - LBND( 1 ) + 1
         YDIM = UBND( 2 ) - LBND( 2 ) + 1

*  Get the numeric type of the Data component.
         CALL NDF_TYPE( IDIN, 'Data', ITYPE, STATUS )

*  Map in Data component and determine if BAD pixels are present.
         CALL NDF_MAP( IDIN, 'Data', ITYPE, 'READ', IPIN, EL, STATUS )
         CALL NDF_BAD( IDIN, 'Data', .FALSE., BAD, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*======================================================================
*  Histogram formation and threshold estimation section.
*======================================================================
*  Start section with title.
         CALL CCD1_MSG( ' ',
     :'    HISTOGRAM formation and THRESHOLD estimation', STATUS )
         CALL CCD1_MSG( ' ',
     :'    --------------------------------------------', STATUS )

*  Get fraction of values which are required in one bin for sampling
*  to be "optimal".
         CALL PAR_GET0D( 'BINFRAC', BINPER, STATUS )
         BINPER = ABS( BINPER )

*  Determine how many counts per bin this equates to.
         MINBIN = INT( DBLE( EL ) * BINPER * 0.01D0 )

*  Get the oversampling factor (over mean count per bin).
         CALL PAR_GET0I( 'OVERSAMP', OVSAMP, STATUS )
         OVSAMP = MAX( 1, OVSAMP )

*  Get workspace for holding histogram. Oversample the mean pixel count
*  per bin by a factor of OVSAMP
         NEED = MAX( 10, MINBIN * OVSAMP )
         CALL CCD1_MALL( NEED, '_INTEGER', IPHIST, STATUS )

*  Report how the histogram is formed.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :'  Initial histogram parameters: ', STATUS )
         CALL MSG_SETD( 'BINFRAC', BINPER )
         CALL CCD1_MSG( ' ',
     :'  Minimum fraction of counts in one bin: ^BINFRAC%%',
     :        STATUS )
         CALL MSG_SETI( 'NEED', NEED )
         CALL CCD1_MSG( ' ',
     :'  Number of bins                       : ^NEED', STATUS )

*  Determine the histogram with this optimal bin fraction. Also returns
*  the mode (bin number which contains the peak count). Note the BAD
*  flag is updated by this routine.
         CALL CCD1_MKHIS( ITYPE, IPIN, EL, BAD, MINBIN, NEED,
     :                    %VAL( IPHIST ), MODE, PEAK, NBIN, ZERO, WIDTH,
     :                    STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Report on final histogram parameters.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :'  Final histogram parameters:', STATUS )
         CALL MSG_SETI( 'NBIN', NBIN )
         CALL CCD1_MSG( ' ',
     :'  Number of bins: ^NBIN', STATUS )
         CALL MSG_SETI( 'PEAK', PEAK )
         CALL CCD1_MSG( ' ',
     :'  Peak count    : ^PEAK', STATUS )
         CALL MSG_SETD( 'WIDTH', WIDTH )
         CALL ccd1_msg( ' ',
     :'  Bin width     : ^WIDTH', STATUS )

*  How the threshold value is to be determined? This may be either a
*  percentile value or a number of standard deviations above the mode.
*  If the latter option is used then the standard deviation of the
*  background needs to be determined. The standard deviation is
*  estimated by a gaussian fit to the background counts.
         CALL PAR_GET0L( 'USEPER', USEPER, STATUS )
         IF ( USEPER ) THEN 

*  Get the number of percentiles that the threshold is to be set at.
            CALL PAR_GET0D( 'PERCENTILE', PERCEN, STATUS )
            PERCEN = ABS( PERCEN * 0.01 )

*  Determine what this value is.
            CALL CCD1_HISP( %VAL( IPHIST ), NBIN, ZERO, WIDTH, PERCEN,
     :                      THRESH, STATUS )

*  Write message abotu actions.
            CALL MSG_SETD( 'PERCEN', PERCEN * 100.0D0 )
            CALL CCD1_MSG( ' ',
     :'  Using ^PERCEN percentile to estimate threshold', STATUS )
         ELSE

*  Fit the background using a gaussian.
            CALL CCD1_GAFIT( %VAL( IPHIST ), NBIN, MODE, SD, STATUS )

*  Inform user about fit parameters.
            CALL MSG_SETD( 'MODE', DBLE( MODE - 1 ) * WIDTH + ZERO )
            CALL CCD1_MSG( ' ',
     :'  Estimated background :^MODE', STATUS )
            CALL MSG_SETD( 'SD', SD * WIDTH )
            CALL CCD1_MSG( ' ',
     :'  Standard deviation   :^SD', STATUS )
            CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get the number of sigmas to set the threshold at.
            CALL PAR_GET0D( 'NSIGMA', NSIGMA, STATUS )

*  Set the threshold.
            THRESH = DBLE( MODE - 1 ) * WIDTH +ZERO +
     :               NSIGMA * SD * WIDTH

*  Tell user what is happening.
            CALL MSG_SETD( 'NSIGMA', NSIGMA )
            CALL CCD1_MSG( ' ',
     :'  Using ^NSIGMA standard deviations above background to'//
     :' estimate threshold', STATUS )
         END IF

*  Report the threshold.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETD( 'THRESH', THRESH )
         CALL CCD1_MSG( ' ',
     :'  Estimated threshold: ^THRESH', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get the Threshold value which will be used. Only allow modification
*  if autothresholding is disabled.
         IF ( .NOT. AUTOTH ) THEN
            CALL PAR_DEF0D( 'THRESH', THRESH, STATUS )
            CALL PAR_GET0D( 'THRESH', THRESH, STATUS )

*  Make sure of a prompt next time round.
            CALL PAR_CANCL( 'THRESH', STATUS )

*  Report new threshold.
            CALL MSG_SETD( 'THRESH', THRESH )
            CALL CCD1_MSG( ' ',
     :'  User defined threshold: ^THRESH', STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Using auto-thresholding', STATUS )
         END IF
         CALL CCD1_MSG( ' ', ' ', STATUS )

*  Release histogram workspace.
         CALL CCD1_MFREE( IPHIST, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*=======================================================================
*  End of threshold estimation section.
*  Start of image feature detection and centroiding section.
*=======================================================================
*  Start section with title.
         CALL CCD1_MSG( ' ',
     :'    Image feature DETECTION and CENTROIDING', STATUS )
         CALL CCD1_MSG( ' ',
     :'    ---------------------------------------', STATUS )

*  Get the minimum number of pixels per image.
         CALL PAR_GET0I( 'MINPIX', MINPIX, STATUS )

*  See if images which touch the edges of the array are to be excluded.
         CALL PAR_GET0L( 'TOUCH', TOUCH, STATUS )

*  Report these parameters.
         CALL MSG_SETI( 'MINPIX', MINPIX )
         CALL CCD1_MSG( ' ',
     :'  Minimum number of pixels per group: ^MINPIX', STATUS )
         IF ( TOUCH ) THEN
            CALL CCD1_MSG( ' ',
     :'  Pixels groups may contact array edges', STATUS )
         ELSE
            CALL CCD1_MSG( ' ',
     :'  Pixels groups will not contact array edges', STATUS )
         END IF

*  Count the number of pixels above the threshold to estimate the
*  workspace requirements.
         CALL CCD1_NABV( ITYPE, IPIN, EL, BAD, THRESH, NABOVE, STATUS )
         IF ( NABOVE .GT. 0 ) THEN 

*  Get workspace for object connectivity information.
            CALL CCD1_MALL( NABOVE, '_INTEGER', IPX, STATUS )
            CALL CCD1_MALL( NABOVE, '_INTEGER', IPY, STATUS )
            CALL CCD1_MALL( NABOVE, '_DOUBLE', IPINT, STATUS )
            CALL CCD1_MALL( NABOVE, '_INTEGER', IPGRP, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Determine the connectivity of images above the threshold.
            CALL CCD1_DCON( ITYPE, IPIN, XDIM, YDIM, BAD, THRESH, TOUCH,
     :                      %VAL( IPX ), %VAL( IPY ), %VAL( IPINT ),
     :                      %VAL( IPGRP ), NOBJ, NABOVE, STATUS )

*  Get workspace for the centroiding results
            IF ( NOBJ .GT. 0 ) THEN
               CALL CCD1_MALL( NOBJ, '_DOUBLE', IPXC, STATUS )
               CALL CCD1_MALL( NOBJ, '_DOUBLE', IPYC, STATUS )
               CALL CCD1_MALL( NOBJ, '_DOUBLE', IPMIN, STATUS )
               CALL CCD1_MALL( NOBJ, '_DOUBLE', IPSUM1, STATUS )
               CALL CCD1_MALL( NOBJ, '_DOUBLE', IPSUM2, STATUS )
               CALL CCD1_MALL( NOBJ, '_INTEGER', IPCON, STATUS )
               IF ( STATUS .NE. SAI__OK ) GO TO 99
               
*  Now form the centroids.
               CALL CCD1_DCEN( NABOVE, %VAL( IPX ), %VAL( IPY ),
     :                         %VAL( IPINT ), %VAL( IPGRP ), NOBJ, 
     :                         MINPIX, %VAL( IPSUM1 ), 
     :                         %VAL( IPSUM2 ), %VAL( IPCON ), 
     :                         %VAL( IPXC ), %VAL( IPYC ),
     :                         %VAL( IPMIN), NOUT, STATUS )
               IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Inform the user about the number of features located.
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL MSG_SETI( 'NOUT', NOUT )
               IF ( NOUT .EQ. 1 ) THEN 
                  CALL CCD1_MSG( ' ',
     :'  ^NOUT image features located.', STATUS )
               ELSE
                  CALL CCD1_MSG( ' ',
     :'  ^NOUT image features located.', STATUS )
               END IF
               CALL CCD1_MSG( ' ', ' ', STATUS )

*  Transform the centroid positions to data coordinates.
               TR( 1 ) = DBLE( LBND( 1 ) ) - 1.5D0
               TR( 2 ) = 1.0D0 
               TR( 3 ) = 0.0D0
               TR( 4 ) = DBLE( LBND( 2 ) ) - 1.5D0
               TR( 5 ) = 0.0D0
               TR( 6 ) = 1.0D0 
               CALL CCD1_LXYT3( %VAL( IPXC ), %VAL( IPYC ), NOUT, TR,
     :                          STATUS )

*  Get the output file which is to contain the results. The name of this
*  file is stored in the FIOGRP group of names.
               CALL IRH_GET( FIOGRP, INDEX, 1, FNAME, STATUS )
               CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FDOUT, 
     :                          STATUS )

*  Report error message if open failed.
               IF ( FIO_TEST( 'OPEN error', STATUS ) ) THEN
                  CALL MSG_SETC( 'FNAME', FNAME )
                  CALL ERR_REP( 'FINDOBJ_PFERR',
     : '  Failed to open results file ^FNAME', STATUS )
               END IF

*  Write the output results.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL CCD1_FIOHD( FDOUT, 'Output from FINDOBJ', 
     :                             STATUS )
                  CALL CCD1_WRXYP( FDOUT, %VAL( IPXC ), %VAL( IPYC ),
     :                             %VAL( IPMIN ), NOUT, LINE, 
     :                             CCD1__BLEN, STATUS )

*  Close the positions list file.
                  CALL FIO_CLOSE( FDOUT, STATUS )

*  Report the results.
                  CALL MSG_SETC( 'FNAME', FNAME )
                  CALL CCD1_MSG( ' ',
     :'  Image centroids written to file: ^FNAME', STATUS )
               END IF

*  Finally enter the name of the file to the extension.
               CALL CCG1_STO0C( IDIN, 'CURRENT_LIST', FNAME, STATUS )
            END IF
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN 
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FINDOBJ_NOPIX', 
     :'  There are no pixels with value greater than the threshold.',
     :                    STATUS )
         
         END IF
*=======================================================================
*  End of image feature detection and centroiding section
*=======================================================================

*  Annul the NDF and close its container file.
         CALL NDF_ANNUL( IDIN, STATUS )

*  Release all memory used on this loop.
         CALL CCD1_MFREE( -1, STATUS )

*  Write terminator for Processing NDF: message.
         CALL CCD1_MSG( ' ', '  ---',STATUS )
 
*  Trap cyclic errors.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
 1    CONTINUE

*  Successful scan for image features. Now write a list of the output
*  positions list names.
      IF ( STATUS .EQ. SAI__OK ) THEN 
         CALL CCD1_LNAM( 'NAMELIST', 1, NNDF,
     :   '# FINDOBJ - output position lists', FIOGRP, .TRUE., STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ', '  No namelist written ', STATUS )
         END IF
      END IF

*  Exit on error label.
 99   CONTINUE

*  Free any dynamic memory still allocated for any reason.
      CALL CCD1_MFREE( -1, STATUS )

*  Release all NDF resources.
      CALL NDF_END( STATUS )

*  Annul all IRH identifiers.
      CALL IRH_ANNUL( NDFGRP, STATUS )
      CALL IRH_ANNUL( FIOGRP, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'FINDOBJ_ERR',
     :'FINDOBJ: Error locating image feature centroids', STATUS )
      END IF

*  Close down CCDPACK.
      CALL CCD1_END( STATUS )

      END
* $Id$
