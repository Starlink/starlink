      SUBROUTINE MAKEFLAT( STATUS )
*+
*  Name:
*     MAKEFLAT

*  Purpose:
*     Produces a flatfield calibration NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MAKEFLAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine combines a set of frames into a flatfield. The
*     input data should be of a photometrically flat source, and
*     should be corrected for any instrumental effects. The output
*     calibration frame is normalised to have an average value of one.
*
*     The input data are filtered in an attempt to remove any small
*     blemishes etc. before combination.  This is achieved by smoothing
*     using a boxfilter and then comparing with the original data. An
*     estimate of the standard deviation of each pixel from its
*     surroundings is made. Pixels deviating by more than GAMMA
*     standard deviations are rejected. This procedure is then
*     iterated ITER times. In this way, all image features with a
*     scale size comparable with, or smaller than, the smoothing area
*     size are rejected.

*  Usage:
*     makeflat in out method { alpha=?
*                            { sigmas=?
*                            { sigmas=? niter=?
*                            { min=? max=?

*  ADAM Parameters:
*     ALPHA = _REAL (Read)
*        The fraction of extreme values to remove before combining
*        the data at any pixel. This fraction is removed from each
*        extreme so can only take a value in the range 0 to 0.5.
*        Only used if METHOD="TRIMMED"
*        [0.2]
*     BOXSIZE(2) = _INTEGER (Read)
*        The X and Y sizes (in pixels) of the rectangular box to be
*        applied to smooth the input images. If only a single value is
*        given, then it will be duplicated so that a square filter is
*        used. The values given will be rounded up to positive odd
*        integers if necessary. The values should be adjusted to be
*        larger than the size of any expected defects.
*        [15,15]
*     CLEAN = _LOGICAL (Read)
*        Whether or not to attempt to clean the input images of any
*        defects. For some data types (i.e. spectra) small scale
*        strutures and sharp edges may be real and can be protected
*        against removal by setting this parameter FALSE.
*        [TRUE]
*     GAMMA = _REAL (Read)
*        The number of standard deviations by which a value has to
*        deviate from the local mean (defined by the mean within a box
*        of BOXSIZE(1) by BOXSIZE(2) pixels) before it is considered to
*        be in error. Aberrant pixels are removed from the data before
*        the next "cleaning" iteration is performed.
*        [3.0]
*     IN = LITERAL (Read)
*        A list NDF names. These contain the flatfield data.  The NDF
*        names should be separated by commas and may include wildcards.
*     ITER = _INTEGER (Read)
*        The number of defect rejecting iterations.
*        [3]
*     KEEPIN = _LOGICAL (Read)
*        Whether to keep (i.e. not delete) the input NDFs or
*        not. Deleting the input NDFs has the advantage of saving disk
*        space, but should probably only be used if this program is part
*        of a sequence of commands and the intermediary data used by
*        it are not important.
*
*        The default for this parameter is TRUE and this cannot be
*        overridden except by assignment on the command line or in
*        reponse to a forced prompt.
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
*     MAX = _REAL (Read)
*        If METHOD = "THRESH" then this value defines the upper limit
*        for values which can be used when combining the data. This
*        limit applies to the output data range.
*     METHOD = LITERAL (Read)
*        The method to be used to combine the data components of
*        the input NDFs. This may be set to any unique abbreviation of
*        the following:
*           -  MEAN      -- Mean of the input data values
*           -  MEDIAN    -- Median of the input data values
*           -  TRIMMED   -- An "alpha trimmed mean" in which a fraction
*                           alpha of the values are removed from
*                           each extreme
*           -  MODE      -- An iteratively "sigma clipped" mean which
*                           approximates to the modal value
*           -  SIGMA     -- A sigma clipped mean
*           -  THRESHOLD -- Mean with values above and below given
*                           limits removed
*           -  MINMAX    -- Mean with the highest and lowest values
*                           removed
*           -  BROADENED -- A broadened median (the mean of a small
*                           number of central values)
*        [MEDIAN]
*     MIN = _REAL (Read)
*        If METHOD = "THRESH" then this value defines the lower limit
*        for values which can be used when combining the data. This
*        limit applies to the output data range.
*     MINPIX = _INTEGER (Read)
*        The minimum number of good (ie. not BAD) pixels required which
*        are required to contribute to the value of an output pixel.
*        Output pixels not meeting this requirement are set BAD.
*        [1]
*     NITER = _INTEGER (Read)
*        The number of refining iterations performed if METHOD = "MODE".
*        [7]
*     OUT = LITERAL (Write)
*        Name of an NDF to contain the output flatfield data. Note this
*        NDF will have a precision of at least _REAL. This name may be
*        specified using indirection through a file.
*     SIGMAS = _REAL (Read)
*        Number of standard deviations to reject data at. Used for
*        "MODE" and "SIGMA" methods. For METHOD = "MODE" the standard
*        deviation is estimated from the population of values.
*        For METHOD = "SIGMA" this value is the pixel variance it one
*        exists, otherwise it is the inverse of the mean data value.
*        [4.0]
*     TITLE = LITERAL (Read)
*        Title for the output NDF.
*        [Output from MAKEFLAT]

*  Examples:
*     makeflat in='"f1,f2,f3,f4,f5"' method=median out=mflat
*        This forms a master flat field from NDFs f1 to f5. The input
*        data are first cleaned using the default values for the GAMMA
*        and ITER parameters. The combination mode chosen is the
*        median.  The output NDF is mflat. Note the quotes when
*        entering a comma separated list on the command line.
*
*     makeflat in=^flat_frames.lis out=master_flat
*        In this example the list of NDFs is read from the file
*        flat_frames.lis. This file may contain indirection to other files
*        up to a depth of 7.
*
*     makeflat in='flatr/*' out='flatr/master_flat' gamma=2.5 iter=5
*        In this example all the NDFs in the subdirectory bias/ are
*        used. The input data are severely cleaned using a noise cut
*        of 2.5 standard deviations (current) and 5 iterations. Such
*        severe cleaning is only recommended when many input frames
*        are given, if this is not the case then BAD areas may be seen
*        in the output NDF.
*
*     makeflat in='ff*' out=master_flat gamma=10 iter=1
*        In this example all the frames "ff*" are combined into a master
*        flatfield. Defect rejection is still performed but with
*        gamma set so high and by performing only one iteration
*        almost no bad data will be detected.

*  Implementation Status:
*     - The routine supports BAD pixels and all data types except
*       COMPLEX.  All combinational arithmetic is performed using
*       floating point.  The AXIS, TITLE and QUALITY components are
*       correctly propagated. The output is a ratio so the units are set
*       to blank. The variances are propagated through the combination
*       processing, assuming that the input data have a normal
*       distribution.

*  Behaviour of parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*     The exceptions to this rule are:
*        - TITLE   -- always "Output from MAKEFLAT"
*        - KEEPIN  -- always TRUE
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets/different devices, or after a break of sometime.
*     The intrinsic default behaviour of the application may be
*     restored by using the RESET keyword on the command line.
*
*     Certain parameters (LOGTO and LOGFILE) have global values. These
*     global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands.

*  Notes:
*     - The data input into this routine should have bias strip
*       regions and any badly vignetted parts removed.

*
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-MAY-1991 (PDRAPER):
*        Original version.
*     5-JUL-1993 (PDRAPER):
*        Made some workspace dynamic.
*     2-FEB-1994 (PDRAPER):
*        Added ability to delete input files.
*     12-SEP-1995 (PDRAPER):
*        Added KEEPIN.
*     6-OCT-1995 (PDRAPER):
*        Updated to CCDPACK 2.0.
*     29-JUL-1996 (PDRAPER):
*        Added CLEAN parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'NDF_PAR'          ! NDF status codes and buffer sizes
      INCLUDE 'CCD1_PAR'         ! CCDPACK internal constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPIX             ! Maximum number of pixel mapped from
                                 ! input data sets.
      PARAMETER ( MAXPIX = 500000 )

*  Local Variables:
      CHARACTER * ( 25 )  CMODE ! Combination mode
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Output data type
      CHARACTER * ( CCD1__NMLEN ) FILTER ! The filter type of the input NDFs
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Lowest common precision type of input NDFs
      CHARACTER * ( NDF__SZTYP ) PTYPE ! Actual processing precision
      DOUBLE PRECISION AVEACC( CCD1__MXNDF ) ! Mean values
      DOUBLE PRECISION CVAL     ! Mean value scaling factor
      DOUBLE PRECISION INVAVE( CCD1__MXNDF ) ! Inverse mean values
      DOUBLE PRECISION NCON( CCD1__MXNDF ) ! Number of contributing pixels.
      DOUBLE PRECISION STATS( CCD1__MXNDF ) ! Image contribution statistics
      DOUBLE PRECISION WRK1( CCD1__MXNDF ) ! Workspace for combination
      DOUBLE PRECISION WRK2( CCD1__MXNDF ) ! routines.
      DOUBLE PRECISION WRK3( CCD1__MXNDF ) !
      INTEGER BEL               ! Number of pixels in mapped NDF section (stack)
      INTEGER EL                ! Number of pixels in mapped NDF section
      INTEGER I                 ! Loop variable
      INTEGER ICHUNK            ! Loop variable for number of chunks
      INTEGER IDIL              ! Smoothing workspace identifier
      INTEGER IDIM( 2 )         ! Dimensions of padded NDFs
      INTEGER IDIST             ! Smoothing workspace identifier
      INTEGER IDNL              ! Smoothing workspace identifier
      INTEGER IDSLI             ! Identifier of slice into w/s stack.
      INTEGER IDWRK4            ! Covariances workspace identifier
      INTEGER IMETH             ! The combination method
      INTEGER INWORK            ! NDF identifier for workspace
      INTEGER IPIECE            ! Identifier for stack chunk
      INTEGER IPIL              ! Pointer to workspace
      INTEGER IPIN              ! Pointer to input NDF
      INTEGER IPIST             ! Pointer to workspace
      INTEGER IPNL              ! Pointer to workspace
      INTEGER IPOINT            ! Pointer to currently mapped NDF section
      INTEGER IPSLI             ! Pointer to slice of NDF
      INTEGER IPSTK             ! Pointer to stack chunk Data
      INTEGER IPVAR             ! Pointer to variance of ""
      INTEGER IPVIN             ! POinter to input NDF variance
      INTEGER IPVSLI            ! Pointer to variance of stack slice.
      INTEGER IPVSTK            ! Pointer to stack chunk Variance
      INTEGER IPWRK4            ! Pointer to covariances workspace
      INTEGER IRET              ! Number of returned values
      INTEGER ITER              ! Number of cleaning rejections
      INTEGER LBND( 3 )         ! Lower bounds of NDF
      INTEGER MINPIX            ! Minimum number of contributing pixels
      INTEGER MMXPIX            ! Maximum value that MXPIX can take
      INTEGER MXPIX             ! Maximum number of pixels in processing stack at one time
      INTEGER NCHUNK            ! Number of chunks an input NDF is to be accessed in.
      INTEGER NDFCUR            ! Identifier of currently mapped input NDF
      INTEGER NDFOUT            ! identifier for output NDF
      INTEGER NDIM              ! Number of dimensions of input NDF
      INTEGER NERR              ! Number of numeric errors
      INTEGER NGOOD             ! Number of pixels left after cleaning
      INTEGER NITER             ! Number of clipping iterations
      INTEGER NNDF              ! The number of input NDFs accessed
      INTEGER NPIX              ! The number of pixels in an NDF
      INTEGER NVAR              ! Number of input NDFs with variances
      INTEGER NWRK4             ! Number of elements of covariance w/s
      INTEGER PLACE             ! Place holder for an NDF
      INTEGER POINT( CCD1__MXNDF ) ! Pointers to original order when stacking images
      INTEGER SIZES( 2 )        ! Sides of box to use for cleaning
      INTEGER STACK( CCD1__MXNDF ) ! Stack of input NDF identifiers
      INTEGER UBND( 3 )         ! Upper bounds of NDF
      LOGICAL BAD               ! Set if BAD pixels are present
      LOGICAL CLEAN             ! Clean the input NDFs of defects
      LOGICAL DELETE            ! Delete input NDFs when processed

      LOGICAL HAVVAR            ! Set if all variances components are present.
      LOGICAL THSVAR            ! This variance - used for testing variance presence.
      LOGICAL USED( CCD1__MXNDF ) ! Workspace for flagging image usage
      REAL ALPHA                ! Trimming fraction
      REAL GAMMA                ! Number of Standard deviations to rejected at.
      REAL NSIGMA               ! Number of sigma to clip at
      REAL RMAX, RMIN           ! Maximum and minimum values (in stack)
      REAL SIGMA                ! Noise estimate in cleaned data.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup log system and write introduction.
      CALL CCD1_START( 'MAKEFLAT', STATUS )

*  See if the user wants to save disk space by deleting the input NDFs
*  when MAKEBIAS is finished with them. This will use the NDF_DELET
*  call which will delete container files if the NDF is associated with
*  the top-level object, otherwise the NDF itself will just be deleted.
*  In the latter case the space used by the NDF in the container file
*  will be released, the size of the file will probably not reduce.
      CALL PAR_GET0L( 'KEEPIN', DELETE, STATUS )
      DELETE = .NOT. DELETE

*  Access the input NDFs
      CALL NDF_BEGIN
      IF ( DELETE ) THEN
         CALL CCD1_NDFAC( 'IN', 'UPDATE', 1, CCD1__MXNDF, NNDF, STACK,
     :                    STATUS )
      ELSE
         CALL CCD1_NDFAC( 'IN', 'READ', 1, CCD1__MXNDF, NNDF, STACK,
     :                    STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Check that the input data have the correct frame types. If they do
*  not then issue a warning. All input data should have a recognised
*  flatfield frame type and have the same filter type.
      FILTER = ' '
      CALL CCD1_CKFLA( STACK, NNDF, FILTER, STATUS )

*  Make sure that all the input data is accessed at a precision
*  sufficient to properly represent a normalised flatfield. This means
*  at least _REAL. The data will be in this precision and will remain
*  in the processing precision on output.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'//
     :                '_DOUBLE', NNDF, STACK, 'Data', ITYPE, DTYPE,
     :                STATUS )

*  Set the processing type, ie. the type in which the data will be
*  stacked etc.
      IF( ITYPE .EQ.'_INTEGER' .OR. ITYPE .EQ. '_DOUBLE' ) THEN
         PTYPE = '_DOUBLE'
      ELSE
         PTYPE = '_REAL'
      END IF

*  A final note on precisions - the variance will be mapped as PTYPE.

*  Find out which input NDFs have variances. Will require all inputs to
*  have variances, otherwise the exposure values will be used as
*  weights. If we do not have all variances then none will be accessed.
      NVAR = 0
      DO 4  I = 1, NNDF
         CALL NDF_STATE( STACK( I ), 'Variance', THSVAR, STATUS )
         IF ( THSVAR ) THEN
            NVAR = NVAR + 1
         END IF
 4    CONTINUE

*  Set HAVVAR flag to show if all variances are present. If not all
*  variances are present and NVAR is greater than zero issue a warning.
      IF ( NVAR .GT. 0 ) THEN
         IF ( NVAR .EQ. NNDF ) THEN
             HAVVAR = .TRUE.
         ELSE
             HAVVAR = .FALSE.
             CALL MSG_OUT( 'MAKECAL_NVAR',
     :       ' Warning - only some input NDFs have a variance'//
     :       ' component, variance analysis not performed.',
     :       STATUS )
         END IF
      ELSE

*  No variances.
         HAVVAR = .FALSE.
      END IF

*  Find out the stacking mode.
      CALL CCD1_GTCPB( IMETH, CMODE, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                 STATUS )

*  Match the pixel-index bounds of the input NDFs (makes them all look
*  the same size), padding out to largest required extent.
      CALL NDF_MBNDN( 'Pad', NNDF, STACK, STATUS )

*  Find out how many pixels a single padded NDF contains.
      CALL NDF_SIZE( STACK( 1 ), NPIX, STATUS )
      CALL NDF_BOUND( STACK( 1 ), 2, LBND, UBND, NDIM, STATUS )
      CALL NDF_DIM( STACK( 1 ), 2, IDIM, NDIM, STATUS )

*  Have we got BAD pixels?
      CALL NDF_MBADN( .TRUE., NNDF, STACK, 'Data,Variance', .FALSE.,
     :                BAD, STATUS )

*  Find out how many pixels of a single NDF on the main stack which we
*  are to access at any one time, the actual amount will be NNDF times
*  this value. Note that the actual stack is the size of all the input
*  NDFs, this is necessary to perform the defect filtering. Hence unlike
*  other stacking routines this only chunks the actual stack. The input
*  NDFs are moved onto the stack one at a time. The stack is sliced up
*  into planes before mapping so the mapped space is around twice that
*  of a single NDF (plus the workspace).
      MMXPIX = MAXPIX / ( NNDF + 4 )
      MXPIX = MAX( 1, MIN( MMXPIX, NPIX ) )

*  Find out how many chunks this corresponds too.
      CALL NDF_NCHNK( STACK( 1 ), MXPIX, NCHUNK, STATUS )

*  Get a temporary NDF big enough to contain all possible input NDF
*  sections in a stack NNDF big.
      CALL NDF_TEMP( PLACE, STATUS )
      LBND( 3 ) = 1
      UBND( 3 ) = NNDF
      CALL NDF_NEW( PTYPE, 3, LBND, UBND, PLACE, INWORK, STATUS )

*  Create the output NDF to contain the result. Propagating axis,
*  quality, label, and history but no units from the first NDF. Do NOT
*  propagate the CCDPACK extension (the information in this only
*  applies to the input NDF).
      CALL CCD1_NDFPR( 'OUT', STACK( 1 ),
     :                 'Axis,Quality,Nounits,Noext(CCDPACK)', NDFOUT,
     :                 STATUS )

*  Set the output types (as explained above) to the processing
*  precision. They will remain at this precision.
      CALL NDF_STYPE( PTYPE, NDFOUT, 'Data,Variance', STATUS )

*  Get the minimum number of contributing pixels per output pixel
      CALL PAR_GET0I( 'MINPIX', MINPIX, STATUS )

*  If we have variances then will process covariances - get the
*  required workspace.
      IF ( HAVVAR ) THEN
         NWRK4 = MAX( 1, ( ( NNDF+ 1 )**3 ) / 2 )
         CALL CCD1_MKTMP( NWRK4, '_DOUBLE', IDWRK4, STATUS )
         NWRK4 = MAX( 1, NWRK4 / NNDF )
         CALL CCD1_MPTMP( IDWRK4, 'WRITE', IPWRK4, STATUS )
      ENDIF

* ----------------------------------------------------------------------
*  Cleaning section.
      CALL PAR_GET0L( 'CLEAN', CLEAN, STATUS )
      IF ( CLEAN ) THEN

*  Get the box sizes for smoothing data.
         CALL PAR_GET1I( 'BOXSIZE', 2, SIZES, IRET, STATUS )
         IF ( IRET .EQ. 1 ) THEN
            SIZES( 2 ) = SIZES( 1 )
         END IF
         SIZES( 1 ) = MAX( SIZES( 1 ) / 2 + 1 ,1 )
         SIZES( 2 ) = MAX( SIZES( 2 ) / 2 + 1 ,1 )

*  Obtain number of iterations for rejection.
         CALL PAR_GET0I( 'ITER', ITER, STATUS )

*  Obtain number of standard deviations for rejection threshold.
         CALL PAR_GET0R( 'GAMMA', GAMMA, STATUS )

*  Get work space used only for smoothing.
         CALL CCD1_MKTMP( IDIM( 1 ), '_INTEGER', IDIL, STATUS )
         CALL CCD1_MPTMP( IDIL, 'WRITE', IPIL, STATUS )
         CALL CCD1_MKTMP( IDIM( 1 ), PTYPE, IDNL, STATUS )
         CALL CCD1_MPTMP( IDNL, 'WRITE', IPNL, STATUS )
      END IF

*  Get other work spaces.
      CALL CCD1_MKTMP( IDIM( 1 ) * IDIM( 2 ), PTYPE, IDIST, STATUS )
      CALL CCD1_MPTMP( IDIST, 'WRITE', IPIST, STATUS )

*  Loop over all the input NDFs cleaning the data and transfering the
*  data and variance onto the stack.
      DO 5 I = 1, NNDF

*  Get a slice of the stack to put the result into.
         CALL NDF_BLOCK( INWORK, 2, IDIM, I, IDSLI, STATUS )

*  Map in the slice. Do not map in variances if not used.
         CALL NDF_MAP( IDSLI, 'Data', PTYPE, 'WRITE', IPSLI, EL,
     :                 STATUS )
         IF ( HAVVAR ) CALL NDF_MAP( IDSLI, 'Variance', PTYPE,
     :                               'WRITE', IPVSLI, EL, STATUS )

*  Map in the input NDF.
         CALL NDF_MAP( STACK( I ), 'Data', PTYPE, 'READ', IPIN, EL,
     :                 STATUS )
         IF ( HAVVAR ) CALL NDF_MAP( STACK( I ), 'Variance', PTYPE,
     :                               'READ', IPVIN, EL, STATUS )

*  Do the cleaning process transfering the result to the stack.
         IF  ( CLEAN ) THEN
            CALL CCD1_FFCL( PTYPE, BAD, IPIN, STACK( I ), IDIM( 1 ),
     :                      IDIM( 2 ), ITER, GAMMA, SIZES( 1 ),
     :                      SIZES( 2 ), SIGMA, IPSLI, NGOOD, IPIST,
     :                      IPIL, IPNL, STATUS )
         ELSE

*  No cleaning requested so just copy the data into the stack.
            CALL CCD1_MSG( ' ','  Data not cleaned', STATUS )
            CALL CCD1_COPY( PTYPE, EL, IPIN, IPSLI, STATUS )
         END IF

*  Copy the variances also (if appropriate).
         IF ( HAVVAR ) CALL CCD1_COPY( PTYPE, EL, IPVIN, IPVSLI,
     :                                 STATUS )

* ----------------------------------------------------------------------
*  Normalisation section.
*  Determine the mean value in each frame and divide by this. Also
*  process any variances similarily.
         CALL CCD1_MEAN( PTYPE, BAD, IPSLI, EL, AVEACC( I ), NGOOD,
     :                   STATUS )

*  Divide the data by this value, check mean of input data is not
*  exactly zero first, data with this characteristic cannot be real
*  a corruption or improper test data must be responsible.
         IF ( AVEACC( I ) .EQ. 0.0D0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', '  Input data component has a mean'//
     :      ' of zero - MAKEFLAT cannot process this', STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CVAL = 1.0D0 / AVEACC( I )
         CALL CCD1_CMULT( BAD, PTYPE, IPSLI, EL, CVAL, IPIST, NERR,
     :                    STATUS )

*  Modify any variances, existing variances are modified by dividing by
*  the signal squared. If no variances exist then use the signal level
*  as a weight (inverted so looks like variance)
         IF ( HAVVAR ) THEN
            CVAL = CVAL * CVAL
            CALL CCD1_CMULT( BAD, PTYPE, IPVSLI, EL, CVAL, IPIST, NERR,
     :                       STATUS )
         ELSE
            INVAVE( I ) = CVAL
         END IF

*  Release the input NDF and the stack slice.
         CALL NDF_UNMAP( STACK( I ), '*', STATUS )
         CALL NDF_UNMAP( IDSLI, '*', STATUS )
         CALL NDF_ANNUL( IDSLI, STATUS )
 5    CONTINUE

*  Release workspace.
      IF ( CLEAN ) THEN
         CALL CCD1_FRTMP( IDNL, STATUS )
         CALL CCD1_FRTMP( IDIL, STATUS )
      END IF
      CALL CCD1_FRTMP( IDIST, STATUS )

* ----------------------------------------------------------------------
*  Stacking section. Cut the data stack up into chunks to aid
*  efficiency (elapsed time-wise)
*  Leave I/O switched off in this application, considerable parts of
*  data seem to be (accidently?) mapped by sub-systems. More robust
*  this way.

*  Zero statistics buffer.
      DO 6 I = 1, NNDF
         NCON( I ) = 0.0D0
 6    CONTINUE
      DO 2 ICHUNK = 1, NCHUNK

*  Get the current chunk of the stack. The way this will work is to
*  chunk the output NDF, get the bounds of this section. Then section
*  the stack so that it uses the same bounds, except in the third
*  dimension which will be NNDF large.
*  Get chunk of output NDF first.
         CALL NDF_CHUNK( NDFOUT, MXPIX, ICHUNK, NDFCUR, STATUS)
         CALL NDF_MAP( NDFCUR, 'Data', PTYPE, 'WRITE', IPOINT, EL,
     :                 STATUS )
         IF ( HAVVAR ) CALL NDF_MAP( NDFCUR, 'Variance', PTYPE,
     :                               'WRITE', IPVAR, EL, STATUS )

*  Get the size of the stack chunk.
         CALL NDF_BOUND( NDFCUR, 2, LBND, UBND, NDIM, STATUS )
         LBND( 3 ) = 1
         UBND( 3 ) = NNDF

*  Section the stack and map it in.
         CALL NDF_SECT( INWORK, 3, LBND, UBND, IPIECE, STATUS )
         CALL NDF_MAP( IPIECE, 'Data', PTYPE, 'READ', IPSTK, BEL,
     :                 STATUS )
         IF ( HAVVAR ) CALL NDF_MAP( IPIECE, 'Variance', PTYPE,
     :                               'READ', IPVSTK, BEL, STATUS )

*  Combine all the NDFs in the stack using the method given.
         IF ( HAVVAR ) THEN

*  Process at real versions.
            IF ( PTYPE .EQ. '_REAL' ) THEN
               CALL CCG1_CM1RR( %VAL( IPSTK ), EL, NNDF, %VAL( IPVSTK),
     :                          IMETH, MINPIX, NITER, NSIGMA, ALPHA,
     :                          RMIN, RMAX, %VAL( IPOINT ),
     :                          %VAL( IPVAR ), WRK1, WRK2, WRK3,
     :                          %VAL( IPWRK4 ), NWRK4, NCON,
     :                          POINT, USED, STATUS )

*  Double precision version.
            ELSE IF ( PTYPE .EQ. '_DOUBLE' ) THEN
               CALL CCG1_CM1DD( %VAL( IPSTK ), EL, NNDF, %VAL( IPVSTK ),
     :                          IMETH, MINPIX, NITER, NSIGMA, ALPHA,
     :                          RMIN, RMAX, %VAL( IPOINT ),
     :                          %VAL( IPVAR ), WRK1, WRK2, WRK3,
     :                          %VAL( IPWRK4 ), NWRK4, NCON,
     :                          POINT, USED, STATUS )
            END IF
         ELSE

*  No variance to propagate, use inverse exposure factors.
*  Process at real versions.
            IF ( PTYPE .EQ. '_REAL' ) THEN
               CALL CCG1_CM3RR( %VAL( IPSTK ), EL, NNDF, INVAVE,
     :                          IMETH, MINPIX, NITER, NSIGMA, ALPHA,
     :                          RMIN, RMAX, %VAL( IPOINT ),
     :                          WRK1, WRK2, NCON, POINT, USED, STATUS )

*  Double precision version.
            ELSE IF ( PTYPE .EQ. '_DOUBLE' ) THEN
               CALL CCG1_CM3DD( %VAL( IPSTK ), EL, NNDF, INVAVE,
     :                          IMETH, MINPIX, NITER, NSIGMA, ALPHA,
     :                          RMIN, RMAX, %VAL( IPOINT ),
     :                          WRK1, WRK2, NCON, POINT, USED, STATUS )
            END IF
         END IF

*  Unmap the output section, ready for next chunks.
         CALL NDF_UNMAP( NDFCUR, '*', STATUS )
         CALL NDF_UNMAP( IPIECE, '*', STATUS )

*  Release the resources associated with this identifier
         CALL NDF_ANNUL( IPIECE, STATUS )
         CALL NDF_ANNUL( NDFCUR, STATUS )
 2    CONTINUE

*  Work out contribution statistics.
      DO 7 I = 1, NNDF
         STATS( I ) = NCON( I ) / DBLE( NPIX ) * 100.0D0
 7    CONTINUE

*  Set bad pixel flag in output NDF
      CALL NDF_SBAD( BAD, NDFOUT, 'Data', STATUS )

*  Set title of output NDF, propagating it if requested.
      CALL NDF_CINP( 'TITLE', NDFOUT, 'TITLE', STATUS )

*  Add the frame type to the output NDF.
      CALL CCG1_STO0C( NDFOUT, 'FTYPE', 'MASTER_FLAT', STATUS )

*  Add the filter type to the output NDF.
      IF ( FILTER .NE. ' '  ) THEN
         CALL CCG1_STO0C( NDFOUT, 'FILTER', FILTER, STATUS )
      END IF

*  Add a time and date stamp to NDF.
      CALL CCD1_TOUCH( NDFOUT, 'MAKEFLAT', STATUS )

*  Report MAKEFLAT parameters, logging if required
      CALL CCD1_RFLT( STACK, NNDF, AVEACC, STATS, CMODE, IMETH, MINPIX,
     :                ALPHA, NSIGMA, NITER, RMIN, RMAX, NDFOUT, PTYPE,
     :                DELETE, STATUS )

*  If requested delete all the input NDFs.
      IF ( DELETE .AND. STATUS .EQ. SAI__OK ) THEN
         DO 10 I = 1, NNDF
            CALL NDF_DELET( STACK( I ), STATUS )
 10      CONTINUE
      END IF

*  End of NDF context, release all identifiers etc.
 99   CONTINUE

*  End NDF context.
      CALL NDF_END( STATUS )

*  Make sure all workspace is released (ok to call these at any time
*  after CCD1_START).
      CALL CCD1_FRTMP( -1, STATUS )
      CALL CCD1_MFREE( -1, STATUS )

*  Close IRH.
      CALL IRH_CLOSE( STATUS )

*  If an error occurred, then report a contextual message. Logging it if
*  required.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'MAKEFLAT_ERR',
     :   'MAKEFLAT: Error making master flat field frame.',
     :   STATUS )
      END IF

*  Close log file and write terminator
      CALL CCD1_END( STATUS )

      END
* $Id$
