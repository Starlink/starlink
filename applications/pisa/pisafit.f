      SUBROUTINE PISAFIT( STATUS )
*+
*  Name:
*     PISAFIT

*  Purpose:
*     Fits a mixed Gaussian - Exponential - Lorentzian profile to
*     STELLAR images.

*  Language:
*     FORTRAN

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISAFIT( STATUS )

*  Usage:
*     PISAFIT IN POSITIONS [DEVICE] [LINEW] [MINMODE]
*        AGAIN=? RADIUS=?

*  Description:
*     PISAFIT fits a radially symmetric mixed Gaussian - Exponential -
*     Lorentzian function to STELLAR type objects. The function is
*     described by three separate parameters, the gaussian sigma, the
*     cross over point, as a fraction of the peak intensity, from the
*     gaussian core to an exponential wing and the fractional mix of a
*     lorentzian to these two functions at each point. The parameters
*     describing the resultant functional fit are stored in the GLOBAL
*     file and can be subsequently accessed by PISAFIND if the profile
*     fitting option is chosen, or by PISAGEN for generating a model
*     data frame of the detected objects. The fit produced by this
*     routine is displayed on a graphics device, together with the
*     residuals (derived from the variance of data contributing to each
*     point), so that the quality of fit can be assessed. The user is
*     prompted as to their satisfaction with the displayed fit, and can
*     re-do the fit, out to a specified radius. This cycle can repeat
*     until the user indicates that they are satisfied with the fit.
*     Experience shows that the first fit is rarely the best.
*
*     The fit parameters can be controlled, within certain limitations,
*     by the user. Three different forms of minimisation parameter
*     bounding are available. Type APM sets up the bounds so that a
*     minimisation as used in the original APM specification of this
*     routine can be performed. Type USER allows the user to directly
*     specify the ranges within which the fitting parameters can vary,
*     and type NONE freely allows the minimisation routine to fit the
*     function with the only restriction that the values are greater
*     than zero. Finally the user can specify that the fit is done
*     using a weighted scheme, using the shown residuals.
*

*  Notes:
*     The potential quality of the fit is very dependent on the objects
*     chosen to produce the radial profile. The best results should be
*     obtained from a range of unsaturated, well separated objects. The
*     list of objects used for the fit can be passed directly from
*     PISAFIND or by a list of object x,y positions produced by the
*     user, say by using the KAPPA routine CENTROID. Note that any
*     input lists of the latter type should be of the form
*     object_number, X_position, Y_position in the first three columns
*     or X_position, Y_position only.

*  ADAM Parameters:
*     AGAIN = _LOGICAL (Read)
*        Controls whether another refinement of the fit takes place or
*        not. [TRUE]
*     BACKGROUND = _REAL (Read)
*        The background (sky) value. Used across whole frame. [Global]
*     COMIX = _REAL (Write)
*        The value found for the mixture ratio between the lorentzian
*        and the other functions, on exit from the program.
*     COMIXRANGE = _REAL (Read)
*        The range of values between which COMIX is allowed to vary
*        during the minimisation. Only used if MINMODE = USER. [0,1]
*     CROSS = _REAL (Write)
*        The value found for the percentage cross over point, from the
*        gaussian core to the exponential wings, on exit from the
*        program.
*     CROSSRANGE = _REAL (Read)
*        The range of values between which CROSS is allowed to vary
*        during the minimisation. Only used if MINMODE = USER. [0,100]
*     DEVICE = DEVICE (Read)
*        A character string specifying a valid device name.
*     GSIGM = _REAL (Write)
*        The value found for the gaussian sigma on exit from the
*        program.
*     GSIGMRANGE = _REAL (Read)
*        The range of values between which GSIGM is allowed to vary
*        during the minimisation. Only used if MINMODE = USER. [0.5,5.5]
*     IN = NDF (Read)
*        The Input NDF containing the objects to be fitted.
*     LINEW = _INTEGER (Read)
*        The relative width of the lines plotted ( greater than equal
*        to 1 ). [1]
*     MINMODE = LITERAL (Read)
*        String defining which type of bounds are to be applied to the
*        minimisation parameters. The allowed returns are any string
*        beginning with the characters 'A','U' or 'N'. These represent
*        'A'PM (default) 'U'SER or 'N'ONE. If return is APM then the
*        bounds for the minimisation are as in the original APM version
*        of this routine ( 0.5 to 5.5 for GSIGM, 0.0 to 1.0 for CROSS
*        and 0.0 to approximately 0.12 for COMIX ). If the return
*        selects USER then the user will be prompted for the limitations
*        to be applied during the minimisation. If the returns selects
*        NONE then the parameters will be allowed to vary to any value
*        greater than 0.0. [APM]
*     POSITIONS = FILENAME (Read)
*        Ascii file containing the positions of the objects to be
*        fitted. The file may be of the type produced by PISAFIND or
*        just of list of either
*        ( object number ) ( x position ) ( y position ) etc.,
*        or
*        ( x position ) ( y position ).
*        [PISAFIND.DAT]
*     PRALL = _LOGICAL (Read)
*        Analyse the whole array ( to derive the background value )
*        or select a subset of the data.
*
*        If this is true the whole data array is analysed. If this is
*        false the pixel coordinates defining a subset of the data to be
*        analysed are requested. [TRUE]
*     RADIUS = _REAL (Read)
*        The radius out to which the fit will be attempted. This should
*        be at least a few stellar radii. [10.0]
*     WEIGHTED = _LOGICAL
*        Set to true if a weighted fit is to be attempted. The weighting
*        is based on the distribution of errors of values contributing
*        to a bin in the mean profile. Note that under certain
*        conditions weighting can seem to bias fit unfairly to the
*        central bins (usually seen as a very poor fit to the outer
*        values). If this situation occurs then an unweighted fit is
*        the best option. [TRUE]
*     XPIXS = _INTEGER (Read)
*        Initial and final pixel coordinates of the subset of the data
*        array to be analysed. [Dynamic]
*     YPIXS = _INTEGER (Read)
*        Initial and final pixel coordinates of the subset of the data
*        array to be analysed. [Dynamic]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Timing:
*     The timing for the initial processing (i.e. radial profile
*     production) is proportional to the number of objects given.
*     The latter processing (the fitting loop) is dependent only on
*     the number of points in the fit .
*

*  Examples:
*     PISAFIT FRAME FRAME.STARS_ACC CANON_PORTRAIT 3 NONE AGAIN=F
*        RADIUS=15
*        Performs the radial fit on the stars contained in the list
*        FRAME.STARS_ACC. It directs the graphical output to a laser
*        printer whose lines are printed at three times normal density.
*        The bounds on the minimisation are freed to be any value
*        greater than 0. The interactive, recursive, fitting loop is
*        disabled by setting AGAIN=F and the fit is done out to a
*        radius of 15 pixels.
*
*     PISAFIT FRAME FRAME.STARS_ACC
*        Performs the production of the radial profile. Enters into an
*        interactive session allowing the user to modify the radius of
*        the data used in the fit.

*  Implementation Status:
*     The present status of the program has 4 main drawbacks.
*
*     1. The data must lie within the range of signed word reasonably
*     approximated by integers.
*
*     2. There is no bad pixel handling.
*
*     3. The input arrays cannot be any larger than 10240 pixels in
*     any axis.
*
*     4. A maximum of 1000 input objects is allowed.

*  Functional Forms:
*     The basic functional forms of the fitting equations are:
*
*     1. exp( -r**2/gsigm**2 )    -- gaussian
*
*     2. 1/( 1 + r**2/gsigm**2 )  -- lorentzian
*
*     3. exp( -( r - rc )/gsigm ) -- exponential
*
*     where rc is the radius at which change over from the gaussian to
*     the exponential occurs ( rc= sigma*sqrt( -log( cross/100 ))).
*     At radii below rc the fitting form goes as
*
*     comix*lorentzian + (1-comix)*gaussian
*
*     and at radii above rc
*
*     comix*lorentzian + (1-comix)*exponential.
*

*  Authors:
*     MIKE: Mike Irwin (APM)
*     PDRAPER: Peter Draper (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-AUG-1990 (MIKE):
*        Original version.
*     5-SEP-1990 (PDRAPER):
*        Converted to ADAM A-Task, major re-coding of most parts
*     12-SEP-1990 (PDRAPER):
*        Changed graphics to access device through parameter system
*     13-SEP-1990 (PDRAPER):
*        Changed to plot residuals
*     15-SEP-1990 (PDRAPER):
*        Changed routine to use NAG minimisation routine
*     21-SEP-1990 (PDRAPER):
*        Changed to allow flexible minimisation parameter limits
*        through routine GTMLIM.
*     24-SEP-1990 (PDRAPER):
*        Changed to use AGI. Also device handling changed to access one
*        device for the whole fitting session. Changed name from
*        FITPRO to PISAFIT. Changed plotting to use routine FITPLO,
*        instead of inline code.
*     25-SEP-1990 (PDRAPER):
*        Changed to use AGI pictures instead of PGPLOT viewports.
*     1-OCT-1990 (PDRAPER):
*        Changed to use accurate estimate of radius in radial bins.
*     2-OCT-1990 (PDRAPER):
*        Changed to allow the user control over the background value.
*     10-OCT_1990(PDRAPER):
*        Used corrections for NDF origin.
*     22-NOV-1990 (PDRAPER):
*        Added weighted fit option, major changes to raprof and funct1.
*     4-SEP-1991 (PDRAPER):
*        Changed to use array mapping.
*     11-AUG-1992 (PDRAPER):
*        Now uses simple NDFs correctly.
*     31-AUG-1996 (PDRAPER):
*        Removed call to NAG routine E04JAF and replaced with PDA_LMDIF1.
*     07-SEP-2004 (PDRAPER):
*        Changed to use CNF_PVAL.
*     14-AUG-2005 (TIMJ):
*        Fix standards uncompliant SQRT
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'FIO_ERR'         ! FIO system error codes
      INCLUDE 'PRM_PAR'         ! Primitive data constants
      INCLUDE 'CNF_PAR'         ! CNF functions

*  Status:
      INTEGER STATUS            ! Global status

*  Local constants:
      INTEGER MAXOBJ            ! Maximum number of objects to fit
      PARAMETER ( MAXOBJ = 1000 )
      INTEGER FITSIZ             ! Number of potential data points
      PARAMETER ( FITSIZ = 256 ) ! in any fit
      INTEGER MAXINT            ! Maximum integer*2
      PARAMETER ( MAXINT = 32767 )
      INTEGER SUBSIZ               ! Size of sub array to hold object
      PARAMETER ( SUBSIZ = 10240 ) ! data
      INTEGER SUBSID            ! Size of one side of sub array
      PARAMETER ( SUBSID = 64 ) ! Sqrt ( subsiz )
      INTEGER INVAL             ! Not valid data flag
      PARAMETER ( INVAL = -999 )
      INTEGER BUFSIZ             ! Size of buffer to hold line read
      PARAMETER ( BUFSIZ = 132 ) ! from file
      INTEGER LIW               ! Size of minimization routine w/s
      PARAMETER ( LIW = FITSIZ * 3 + 15 + FITSIZ )
      INTEGER LW                ! Size of minimization routine w/s
      PARAMETER ( LW = FITSIZ )
      REAL NOTZER               ! Small non zero number
      PARAMETER ( NOTZER = 1.0E-20 )
      REAL CSIZE                 ! Size of plotted pgplot characters
      PARAMETER ( CSIZE = 0.75 ) ! relative to norm
      REAL LOGE
      PARAMETER ( LOGE = 0.43429 ) ! LOG10( E )

*  Global variables:
      REAL XDAT, XCOR           ! Data values to fit
      INTEGER II                ! Number of points in XDAT and XCOR
      DOUBLE PRECISION SD       ! The weights if available
      LOGICAL NOTWEI            ! Whether to use weights or not
      REAL MINMIX, MAXMIX       ! Minimum and maximum values that
                                ! mixture fraction can take
      REAL MINSIG, MAXSIG       ! Maximum and minimum values that
                                ! gaussian sigma can take
      REAL MINTHR, MAXTHR       ! Maximum and minimum values that
                                ! fractional threshold can take
      COMMON /PSA1_FITCM/ SD( FITSIZ ), XDAT( FITSIZ ), XCOR( FITSIZ ),
     :                    II, MINMIX, MAXMIX, MINSIG, MAXSIG, MINTHR,
     :                    MAXTHR, NOTWEI

*  Input array sizes passed solely to and from RDBUF2.
      INTEGER ISTART, ISTOP, NWORD, IXL, IXH
      COMMON /FIL/ ISTART, ISTOP, NWORD, IXL, IXH
*  ISTART,ISTOP - y extent of array, NWORD - number of data values
*  IXL,IXH - x extent of array

*  External references:
      EXTERNAL PSA1_FITFN       ! Subroutine used by PDA_LMDIF1 to
                                ! calculate the difference of the
                                ! current function and the data

*  Local variables:
      CHARACTER*( BUFSIZ ) BUF  ! Buffer to hold line read from file
      DOUBLE PRECISION DEVIA( FITSIZ ) ! Single profile standard
                                       ! deviations buffer
      DOUBLE PRECISION DNUMB    ! Buffer for number counts
      DOUBLE PRECISION MEANSQ( FITSIZ ) ! Workspace for raprof
      INTEGER HALFS             ! Half size of object sub array side
      INTEGER ID0               ! AGI picture identifier for base
                                ! viewport
      INTEGER ID1               ! AGI picture identifier for data
                                ! viewport
      INTEGER ID2               ! AGI picture identifier for
                                ! residuals viewport.
      INTEGER NBOUND            ! Controls how bounds are applied
                                ! to minimisation, nbound=0 for
                                ! user defined bounds, nbound =2
                                ! for values greater than zero.
      INTEGER NTRUE             ! The true number of points have data
                                ! to fit to
      INTEGER ORIGX             ! X origin of data_array
      INTEGER ORIGY             ! Y origin of data array
      INTEGER MAP               ! Pointer to image data
      LOGICAL AGAIN             ! Flag controlling if another fit is
                                ! required
      LOGICAL DEVOPN            ! True if device is opened
      LOGICAL DZERO             ! Flag to control divide by zeros
      LOGICAL NULL              ! Set if a null device is returned
      LOGICAL OPEN              ! Set if input file opened
      REAL DUP, DDOWN, RUP, RDOWN, MUP, MDOWN ! Scaled plot limits
      REAL MAXRES               ! Maximum residual for one fit
      REAL MINRES               ! Minimum residual for one fit
      REAL RADBUF( FITSIZ )     ! Buffer to hold radii
                                ! contributions to a profile bin
      REAL RESIDS( FITSIZ )     ! Buffer to hold the residuals
                                ! between the model and the fit
      REAL RLIM                 ! Radius limit for fit
      REAL V1( 4 )              ! Limits of data plotting viewport-
                                ! normalised device coordinates
      REAL V2( 4 )              ! Limits of residuals plotting
                                ! viewport- normalised device
                                ! coordinates

*  Minimization buffers:
      INTEGER IFAIL
      DOUBLE PRECISION FVEC( FITSIZ )
      DOUBLE PRECISION XC( 3 )
      DOUBLE PRECISION W( LIW )
      DOUBLE PRECISION EPS
      INTEGER IW( LW )

*  Miscellaneous local variables (from old style declarations).
      REAL YCURVE( FITSIZ )
      REAL XLIST( MAXOBJ ),YLIST( MAXOBJ )
      INTEGER IHIST( MAXINT + 100 ),ISBUF( FITSIZ ),IRBUF( FITSIZ )
      REAL XBUF( SUBSIZ ), SBUF( FITSIZ ), RSBUF( FITSIZ),RBUF( FITSIZ )
      INTEGER I, NXOUT, NYOUT, NPIX, MODE, MAXH, IFS, NIMAGE, NNN, K,
     :        IQL, IQH, IYL, IYH, ITR

      REAL XMEAN, XPEAK, SIGMA, GSIGMA, XOUT,YOUT, XX, XS, YS, RSCALE,
     :     TEMP, DLIM, THRESH, Q, COEF1, COEF2, RADTHR, FGAUSS, FLOR,
     :     FEXP, XINT, CHANGE

*  Local Data:
      DATA V1 / 0.1, 0.9, 0.1, 0.65 /
      DATA V2 / 0.1, 0.9, 0.75, 0.9 /

*.

*  Check global status
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*  Clear histogram, and profiling buffers.
      DO 10 I = 1, (MAXINT + 100)
         IHIST( I ) = 0
 10   CONTINUE
      DO 20 I = 1, FITSIZ
         ISBUF( I ) = 0
         SBUF( I ) = 0.0
         RSBUF( I ) = 0.0
         SD( I ) = 0.0D0
 20   CONTINUE

*  Input the image data, get the image size (nxout,nyout).
*  Access data from NDF. Can access sub array of the input data frame.
      CALL NDF_BEGIN
      CALL PSA1_NDFIN( MAP, NXOUT, NYOUT, ORIGX, ORIGY, STATUS )

*  Check the input data array does not exceed the limitations.
      IF ( NYOUT .GT. SUBSIZ ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'LINLEN', SUBSIZ )
         CALL ERR_REP( 'LINE_TO_LONG',
     :   ' Length of first dimension of input data array too long;'//
     :   ' maximum size ^LINLEN. Use sectioning to decrease.', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      NPIX = NXOUT * NYOUT

*   Notes on coordinate systems.
*      APM normal format is:
*
*         -------------> Y-axis
*         |
*         |
*         |
*         V X-axis
*
*      By reversing the axis in RDBUF we are dealing with:
*
*         ^ X-axis
*         |
*         |
*         |
*         -------------> Y-axis
*
*      Any input coordinates from other routines will be as normal.
*
*         ^ Y-axis
*         |
*         |
*         |
*         -------------> X-axis
*
*      Consequently x and y will need to be reversed on input.
*
*  Bin up data into a (integer) histogram, uses offset of 101 (?).
      CALL PSA1_MKHIS( %VAL( CNF_PVAL( MAP ) ), NPIX, IHIST, STATUS )

*  Call histat to estimate the sky background (xpeak) and the noise
*  (sigma) in the background.
      CALL HISTAT( IHIST, MODE, MAXH, XMEAN, XPEAK, SIGMA, GSIGMA,
     :            (MAXINT + 100) )

*  Correct the sky value.
      XPEAK = XPEAK - 100.0

*  Select least sigma, ensure its not zero.
      SIGMA = MAX( NOTZER, MIN( SIGMA, GSIGMA) )

*  Write out the sky value and noise estimate to the user.
      CALL MSG_FMTR( 'XPEAK', 'F7.1', XPEAK )
      CALL MSG_OUT( 'BACK', ' Estimated background level = ^XPEAK',
     :              STATUS )
      CALL MSG_FMTR( 'SIGMA', 'F7.1', SIGMA )
      CALL MSG_OUT( 'SDEV',
     :              ' Background standard deviation = ^SIGMA', STATUS )

*  Offer this value as a default and accept user returned value.
      CALL PAR_DEF0R( 'BACKGROUND', XPEAK, STATUS )
      CALL PAR_GET0R( 'BACKGROUND', XPEAK, STATUS )

*  Open the input file.
      CALL PSA1_ASFIO( 'POSITIONS', 'READ', 'LIST', 0, IFS, OPEN,
     :                 STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999


*  Read in the data from this file until EOF is reached or until the
*  maximum number of objects have been used.
      NIMAGE = 0
 1    CONTINUE
         IF ( NIMAGE .GE. MAXOBJ ) THEN
            CALL MSG_SETI( 'MAXOBJ', MAXOBJ )
            CALL MSG_OUT( 'MAXOBJS', ' The number of input objects has'/
     :           /'been restricted to ^MAXOBJ', STATUS)
            GO TO 2
         END IF
         CALL PXYGET( IFS, BUF, NNN, XOUT, YOUT, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 2

*  Remember swap the order of the x and y positions of input list,
*  correct from starlink convention. Correct for sub-array referencing.
C        YOUT = YOUT - (ORIGY - 1.5) - (ISTART - 1.0)
C        XOUT = XOUT - (ORIGX - 1.5) - (IXL - 1.0)
         XOUT = XOUT - REAL( IXL ) + 1.5
         YOUT = YOUT - REAL( ISTART ) + 1.5

*  Check that this object is within the bounds of the current image.
         IF ( INT( XOUT ) .GE. 1 .AND. INT( XOUT ) .LE. NYOUT .AND.
     :        INT( YOUT ) .GE. 1 .AND. INT( YOUT ) .LE. NXOUT ) THEN

*  Increment number of images
            NIMAGE = NIMAGE + 1

*  Store these positions.
            XLIST( NIMAGE ) = YOUT
            YLIST( NIMAGE ) = XOUT
         END IF
         GO TO 1
 2    CONTINUE

*  May have status other than end-of-file, check for this,
      IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      ENDIF

*  If no objects have been found then exit.
      IF ( NIMAGE .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PISAFIT_ERR',
     :               'PISAFIT: input file contains no valid entries',
     :                STATUS )
         GO TO 999
      ENDIF

*  Get the minimisation limits.
      CALL GTMLIM( NBOUND, MINSIG, MAXSIG, MINTHR, MAXTHR, MINMIX,
     :             MAXMIX, STATUS )

*  Get an intial value for the fit radius.
      CALL PAR_GET0R( 'RADIUS', RLIM, STATUS )

*  Cancel the present parameter associations for RADIUS, to re-enable
*  for next pass.
      CALL PAR_CANCL( 'RADIUS', STATUS )

*  Find if user wants to do a weighted fit.
      NOTWEI = .FALSE.
      CALL PAR_GET0L( 'WEIGHTED', NOTWEI, STATUS )
      NOTWEI = .NOT. NOTWEI
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop for each object in the list.
      HALFS = ( SUBSID-1 ) / 2
      DO 800 K = 1, NIMAGE

*  Zero the object area buffer
         DO 360 I = 1, SUBSIZ
            XBUF( I ) = 0.0
 360     CONTINUE

*  Form the limits of an area (+/- SUBSID/2 ish ) pixels about the centre
*  of the object.
*
*       ------- iql
*       |     |
*       |  *  |
*       |     |
*       ------- iqh
*      iyl   iyh
*
         IQL = MAX( 1, NINT( XLIST( K ) - HALFS ))
         IQH = MIN( NXOUT, NINT( XLIST( K )+ HALFS ))
         IYL = MAX( 1, NINT( YLIST( K )- HALFS ))
         IYH = MIN( NYOUT, NINT( YLIST( K )+ HALFS ))

*  Map the appropriate part of map array into our box and subtract the
*  sky value.
         CALL PSA1_EXSS( %VAL( CNF_PVAL( MAP ) ), IQL, IQH, IYL, IYH,
     :                   NYOUT, SUBSID, XPEAK, XBUF, STATUS )

*  Form the actual position of object in sub-array
         XS = REAL( XLIST( K ) - IQL + 1 )
         YS = REAL( YLIST( K ) - IYL + 1 )

*  Call RAPROF to form the radial profile for this object, assumes
*  radial symmetricity.
*  On return rbuf contains the radial profile and irbuff the numbers
*  of pixels used for each bin in rbuf. suppress the normalization.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL RAPROF( XBUF, SUBSID, FITSIZ, XS, YS, RBUF,
     :                   RADBUF, IRBUF, MEANSQ, DEVIA )
         ELSE
            GO TO 999
         ENDIF

         DO 500 I = 1, FITSIZ

*  Sum number of entries in each bin.
            ISBUF( I ) = ISBUF( I ) + IRBUF( I )

*  Sum radius contributions to this bin.
            RSBUF( I ) = RSBUF( I ) + RADBUF( I ) * IRBUF( I )

*  Sum the intensity values in this bin.
            SBUF( I ) = SBUF( I ) + RBUF( I ) * IRBUF( I )

*  Calculate sum,s to evaluate the standard error in this bin.
            IF( IRBUF( I ) .GT. 1 ) THEN
               DNUMB = DBLE( IRBUF( I ) )
               SD ( I ) = SD( I ) +
     :                 SQRT ( DNUMB * DNUMB * DEVIA( I ) * DEVIA( I ) )
            ELSE

*  Ignore this point !
            ENDIF
 500     CONTINUE
 800  CONTINUE

*  Obtain the normalisation factor.
*  If get divide by zero here, experience suggests that the objects are
*  not centered properly will inform users about this.
      DZERO = .FALSE.
      IF ( ISBUF ( 1 ) .NE. 0 ) THEN
         RSCALE = SBUF( 1 )/ REAL( ISBUF( 1 ))
         IF ( RSCALE .NE. 0.0 ) THEN
            RSCALE = 1.0 / RSCALE
         ELSE
            DZERO = .TRUE.
         ENDIF
      ELSE
         DZERO = .TRUE.
      ENDIF
      IF ( DZERO ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PISAFIT_ERR',
     :        'PISAFIT: Divide by zero; objects not centered correctly',
     :        STATUS )
         GO TO 999
      ENDIF

*  If the first entry is negative, this means that the stars are
*  seriously misaligned with the given positions, QUIT.
      IF ( .NOT. ( SBUF ( 1 ) .GT. 0.0 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'BAD_POSITIONS',
     :        'PISAFIT : Negative first profile value ;'//
     :        ' check star alignment', STATUS )
         GO TO 999
      ENDIF

*  Otherwise normalise the overall (all objects) profile and take LOG.
*  Only record good results.
      NTRUE = 0
      DO 850 I = 1, FITSIZ
         IF ( SBUF( I ) .GT. 0.0 ) THEN
            TEMP = RSCALE * SBUF( I ) / REAL( ISBUF( I ))
            IF ( TEMP .GT. 0.0 ) THEN
               NTRUE = NTRUE + 1
               SBUF( NTRUE ) = LOG10( TEMP )

*  Find the mean radius of points contributing to this bin.
               IF ( ISBUF( I ) .GT. 0 ) THEN
                  RBUF( NTRUE ) = RSBUF( I ) / ISBUF( I )
               END IF

*  Scale the standard error in this bin.
               SD( NTRUE ) = SD( NTRUE ) * RSCALE / REAL( ISBUF ( I ) )

*  Account for LOG10.
               SD( NTRUE ) = SD( NTRUE ) / TEMP * LOGE
            END IF
         END IF
 850  CONTINUE

*  Access device initializing AGI and PGPLOT. Record successful device
*  open.
      CALL PGINTR( V1, V2, NULL, ID0, ID1, ID2, STATUS )
      IF( .NOT. NULL .AND. STATUS .EQ. SAI__OK ) THEN
         DEVOPN = .TRUE.
      ELSE
         DEVOPN = .FALSE.
      END IF

*  LOOPS from here if fit is redone, rlim specifies radius limit
*  for fit, dlim the intensity limit.
855   CONTINUE
         II = 0
         DLIM = -1.0E-10

*  Select data for fit, uses half available data for some reason!
         DO 860 I = 1 , NTRUE

*  Do not go past radius limit.
            IF ( RBUF( I ) .GT. RLIM ) GO TO 865
            II = II + 1

*  Record data in minimisation buffer and record distance estimate.
            XDAT( II ) = SBUF( I )
            XCOR( II ) = RBUF( I )

*  Record the minimum intensity
            DLIM = MIN( SBUF( I ), DLIM )
 860     CONTINUE
 865     CONTINUE

*  Inform user of number of points used in fit.
*  Check that we have enough points to do a fit (more than 1).
         IF ( II .GT. 1 ) THEN
            CALL MSG_SETI( 'NFIT', II )
            CALL MSG_OUT( 'NUMFIT',
     :           ' Number of points in curve fit = ^NFIT', STATUS )
            CALL MSG_OUT( ' ', ' ', STATUS )
         ELSE
            CALL MSG_OUT( 'NOT_ENOUGH',
     :           ' The attempted fit does not contain enough points ',
     :           STATUS)

*  Try again.
            GO TO 3
         END IF

*  Find the best fit to the data.
         XC( 1 ) = 2.0D0
         XC( 2 ) = 0.5D0
         XC( 3 ) = 0.1D0
         EPS = DBLE( VAL__EPSR )
         CALL PDA_LMDIF1( PSA1_FITFN, II, 3, XC, FVEC, EPS, IFAIL,
     :                    IW, W, LIW )
         IF ( IFAIL .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PISAFIT_BADARGS',
     :           'Minization routine reports bad input arguments'//
     :           ' - possible programming error', STATUS )
            GO TO 999
         ELSE IF ( IFAIL .GT. 4 ) THEN
            CALL MSG_OUT( ' ', '  Problems with the fit, check plot',
     :                   STATUS )
         END IF

*  Write out the final RMS value.
         EPS = 0.0D0
         DO 132 I = 1, II
            EPS = EPS + FVEC( I )
 132     CONTINUE
         EPS = ABS ( EPS )
         CALL MSG_SETR( 'RMS', REAL( SQRT( EPS ) ) )
         CALL MSG_OUT( 'RMS_MESS',
     :        ' RMS of fit                 = ^RMS ', STATUS )

*  Set the model parameters.
         SIGMA = REAL( XC( 1 ))
         THRESH = REAL( XC( 2 )) * 100.0
         Q = REAL( XC( 3 ))
         CHANGE = LOG( MAX( NOTZER, 0.01 * THRESH ))
         COEF1 = -1.0 / ( MAX( NOTZER, SIGMA**2 ))

*  Note that " * - " is not standards compliant fortran so parentheses
*  must be used around -COEF1
         COEF2 = SQRT( MAX ( 0.0, ( -4.0 * CHANGE * ( -COEF1 ) ) ) )
         RADTHR = SIGMA * SQRT( MAX( 0.0, -CHANGE ) )
         ITR = NINT( THRESH )

*  Write out the best results.
         CALL MSG_FMTR( 'SIGMA', 'F5.2', SIGMA )
         CALL MSG_OUT( 'REP_SIGMA',
     :        ' Gaussian Sigma      (GSIGM)= ^SIGMA', STATUS)
         CALL MSG_SETI( 'CROSS', ITR )
         CALL MSG_OUT( 'REP_CROSS',
     :        ' % Cross Over        (CROSS)= ^CROSS', STATUS)
         CALL MSG_FMTR( 'COMIX', 'F5.3', Q )
         CALL MSG_OUT( 'REP_COMIX',
     :        ' Mixture Coefficient (COMIX)= ^COMIX', STATUS )

*  Produce the model fit data to plot, and calculate the residuals.
         DO 890 I = 1, NTRUE

*  Radius value.
            XX = RBUF( I )

*  Gaussian argument.
            XINT = COEF1 * XX**2

*  Use functions appropriate to whether we're above or below the
*  threshold.
            IF ( XINT .GE. CHANGE ) THEN
               FGAUSS = ( 1.0 - Q ) * EXP( XINT )
               FLOR = Q / ( 1.0 - XINT / LOG( 2.0 ) )
               YCURVE( I ) = LOG10( MAX( FGAUSS + FLOR, NOTZER ) )
            ELSE
               FEXP= ( 1.0 - Q ) *
     :               EXP( CHANGE + ( RADTHR - XX ) * COEF2 )
               FLOR= Q / ( 1.0 - XINT / LOG( 2.0 ) )
               YCURVE( I ) = LOG10( MAX( FEXP + FLOR, NOTZER ) )
            END IF

*  Form the residuals
            RESIDS( I ) = SBUF( I ) - YCURVE ( I )
 890     CONTINUE

*  Find the limits of the residuals up to the number of points used.
         MAXRES = -1.0E20
         MINRES = 1.0E20
         DO 5 I = 1, II
            MAXRES = MAX( RESIDS( I ) + REAL( SD( I ) ), MAXRES )
            MINRES = MIN( RESIDS( I ) - REAL( SD( I ) ), MINRES )
 5       CONTINUE
         IF ( .NOT. NULL ) THEN

*  Plot the current fit.
            CALL FITPLO( ID0, ID1, ID2, RBUF, SBUF, YCURVE, RESIDS, SD,
     :                   RLIM, DLIM, CSIZE, II, NTRUE, MINRES, MAXRES,
     :                   RDOWN, RUP, DDOWN, DUP, MDOWN, MUP, STATUS )
         END IF

*  Try for a better fit if required.
 3    CONTINUE
      CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS )
      IF ( AGAIN .AND. STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0R( 'RADIUS', RLIM, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Cancel the present parameter associations for AGAIN and RADIUS, and
*  return for next fit.
            CALL PAR_CANCL( 'AGAIN', STATUS )
            CALL PAR_CANCL( 'RADIUS', STATUS )
            GO TO 855
         END IF
      END IF
*  End of fitting block (started at label 855).

*  Write out derived parameters to the environment
      CALL PAR_PUT0R( 'GSIGM', SIGMA, STATUS )
      CALL PAR_PUT0R( 'CROSS', REAL( ITR ) , STATUS )
      CALL PAR_PUT0R( 'COMIX', Q, STATUS )

*  End now in exit block
 999  CONTINUE

*  Close the device, and save the viewports in AGI
      IF ( .NOT. NULL  .AND. DEVOPN ) THEN
         CALL PGEXTR( ID0, ID1, ID2, RDOWN, RUP, DDOWN, DUP, MDOWN,
     :                MUP, STATUS )
      END IF

*  Close the input positions file.
      IF ( OPEN ) THEN
         CALL FIO_CLOSE( IFS, STATUS )
         CALL PAR_CANCL( 'POSITIONS', STATUS )
      END IF

*  Issue closing error message, if required.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISAFIT_ERR',
     :        'PISAFIT: error producing fit',STATUS )
      ENDIF

*  Release the NDF, unmapping etc.
      CALL NDF_END( STATUS )
      END
* $Id$
