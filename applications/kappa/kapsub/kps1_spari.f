      SUBROUTINE KPS1_SPARI( CFRM, MAP,  DIM1, DIM2, ARRAY, LBND, 
     :                        ISIZE, RANGE,
     :                        GAUSS, NXY, POS, LOGF, FD,
     :                        PNMIN, PAXISR, PORIEN, PFWHM, PGAMMA, IDS, 
     :                        GOTIDS, AXISR, THETA, FWHM, 
     :                        GAMMA, WIDTH, SIG, STATUS )
*+
*  Name:
*     KPS1_SPARI

*  Purpose:
*     Finds a set of parameters describing a model star image
*     fitted to a set of star images and to display the results.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SPARI( CFRM, MAP, DIM1, DIM2, ARRAY, LBND, ISIZE, RANGE,
*                        GAUSS, NXY, POS, LOGF, FD,
*                        PNMIN, PAXISR, PORIEN, PFWHM, PGAMMA, IDS, 
*                        GOTIDS, AXISR, THETA, FWHM, GAMMA, 
*                        WIDTH, SIG, STATUS )

*  Description:
*     This routine calls a number of subroutines to find a set of 
*     parameters to describe a model Gaussian star image fitted to a
*     set of star images, to display the results graphically, report
*     the fit parameters to the user, and report the analysis for each
*     star to a log file.

*     The main stages are to find the mean ellipticity, orientation
*     and sigma of the stars.  These are then reported to the optional
*     log file.  Finally, find the form of mean radial profile, and
*     optionally plot it.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The number of pixels per line of the array.
*     DIM2 = INTEGER (Given)
*        The number of lines in the array.
*     ARRAY( DIM1, DIM2 ) = INTEGER (Given)
*        The input array containing the stars to be fitted.
*     LBND( 2 ) = INTEGER (Given)
*        The lower bounds of the input array.
*     ISIZE = INTEGER (Given)
*        The length of the search square side used in finding stars
*        and calculating their ellipticity.
*     RANGE = REAL (Given)
*        The radius in units of the star 'sigma' out to which the
*        radial profile is fitted.
*     GAUSS = LOGICAL (Given)
*        If .TRUE., the radial-fall-off parameter (gamma) is fixed to be
*        2; in other words the best-fitting two-dimensional Gaussian is
*        evaluated.  If .FALSE., gamma is a free parameter of the fit,
*        and the derived value is returned in argument GAMMA.
*     NXY = INTEGER (Given)
*        The number of stars to be fitted.
*     POS( NXY, 2 ) = DOUBLE PRECISION (Given)
*        Each line comprises the approximate x then y positions of a 
*        star centre.
*     LOGF = LOGICAL (Given)
*        Logging switch.  If true the results of the analysis, including
*        a table of parameters for each star, and indiciating omitted
*        stars will be written to the file specified by the file
*        descriptor.
*     FD = INTEGER (Given)
*        The file descriptor of the log file.  It will be ignored if
*        the logging switch is off.  The file must be already open.
*     PNMIN = CHARACTER * ( * ) (Given)
*        The name of a parameter which will be used to decide
*        whether to plot the profile along the minor or major axis.
*     PAXISR = CHARACTER * ( * ) (Given)
*        The name of an output parameter which will be used to store
*        the mean axis ratio.
*     PORIEN = CHARACTER * ( * ) (Given)
*        The name of an output parameter which will be used to store
*        the mean major axis orientation.
*     PFWHM = CHARACTER * ( * ) (Given)
*        The name of an output parameter which will be used to store
*        the mean minor axis width.
*     PGAMMA = CHARACTER * ( * ) (Given)
*        The name of an output parameter which will be used to store
*        the exponent in the radial star profile.
*     IDS( NXY ) = INTEGER (Given)
*        An array of integer identifiers for the supplied positions. Only
*        accessed if GOTIDS is .TRUE.
*     GOTIDS = LOGICAL (Given)
*        Does the IDS array contain usable position identifiers? If not,
*        position identifiers 1, 2, 3, ... NXY will be used.
*     AXISR = REAL (Returned)
*        The axis ratio of the star images.
*     THETA = REAL (Returned)
*        The orientation of the major axis of the star images to the
*        x axis in radians (x through y positive).
*     FWHM = REAL (Returned)
*        The full width at half maximum of the star images in the
*        minor-axis direction, measured in pixels.
*     GAMMA = REAL (Returned)
*        The exponent in the radial star profile.
*     WIDTH = INTEGER (Returned)
*        The dimension of the square in pixels that was used to
*        determine the point-spread function.
*     SIG( NXY, 5 ) = REAL (Returned)
*        Intermediate storage for the widths of the star marginal
*        profiles.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (Durham Univ.)
*     MJC: Malcolm J. Currie (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1981 (RFWS):
*        Original version.
*     1990 September 21 (MJC):
*        Made generic; renamed from SPARAM; removed INVAL and ILEVEL;
*        passed extra arguments for the plotting, graphics database, and
*        a log file; combined x and y positions into a single array;
*        commented the variables, and converted the prologue.
*     1991 July 6 (MJC):
*        Obtained workspace for the arrays needed for the routine that
*        calculates the mean point spread function; thus calculating
*        the number of bins in a star's and the mean profile, and the
*        number of pixels in the largest bin.  Make an additional
*        call for the plotting.
*     1991 July 9 (MJC):
*        Added WIDTH argument.
*     1991 August 20 (MJC):
*        Added PNFONT argument passed onto plotting routine.
*     1992 April 2 (MJC):
*        Reordered the ARRAY argument to its normal location after the
*        dimensions.  Added LBND argument.
*     1993 August 27 (MJC):
*        Added additional plotting parameters, SCALE, and RUNITS; and
*        swapped NXY and POS arguments.
*     1995 January 13 (MJC):
*        Used PSX to obtain workspace.
*     1997 December 20 (MJC):
*        Used PSX_MALLOC where arbitrary data type is required.
*     1998 May 26 (MJC):
*        Added GAUSS argument.  Used modern style of variable ordering.
*     15-JUL-1999 (TDCA):
*        Converted graphics to AST/PGPLOT and removed PNMINT, PNMAJT,
*        PNOUTT, and PNFONT.
*     20-SEP-1999 (DSB):
*        Big changes for AST version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER CFRM
      INTEGER MAP
      INTEGER DIM1
      INTEGER DIM2
      INTEGER ARRAY( DIM1, DIM2 )
      INTEGER LBND( 2 )
      INTEGER ISIZE
      LOGICAL GAUSS
      REAL RANGE
      INTEGER NXY
      DOUBLE PRECISION POS( NXY, 2 )
      LOGICAL LOGF
      INTEGER FD
      CHARACTER * ( * ) PNMIN
      CHARACTER * ( * ) PAXISR
      CHARACTER * ( * ) PORIEN
      CHARACTER * ( * ) PFWHM
      CHARACTER * ( * ) PGAMMA
      INTEGER IDS( NXY )
      LOGICAL GOTIDS

*  Arguments Returned:
      REAL AXISR
      REAL THETA
      REAL FWHM
      REAL GAMMA
      INTEGER WIDTH
      REAL SIG( NXY, 5 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string less
                                 ! any trailing blanks

*  Local Constants:
      INTEGER MAXRAD             ! Maximum radius in pixels of the radial 
      PARAMETER ( MAXRAD = 100 ) ! profile

      REAL RESOL1                ! Resolution (no. of bins per pixel
      PARAMETER ( RESOL1 = 2.5 ) ! spacing) for binning the radial
                                 ! profiles of each star
      
      REAL RESOL2                ! Resolution (no. of bins per pixel
      PARAMETER ( RESOL2 = 10.0 )! spacing) for binning the mean radial
                                 ! profile
      
      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.1415926535898 )

*  Local Variables:
      CHARACTER BUFFER*80        ! Buffer to write results
      CHARACTER FMTVAL*(AST__SZCHR) ! Formatted value
      CHARACTER FVAL*10          ! Formatted FWHM value
      CHARACTER FWHMUN*20        ! Units of FWHM
      CHARACTER UNIT*20          ! Value of Unit attribute
      CHARACTER XVAL*13          ! Formatted axis 1 value
      CHARACTER YVAL*13          ! Formatted axis 2 value
      DOUBLE PRECISION SEEING    ! FWHM seeing disk
      DOUBLE PRECISION SIGOUT    ! Sigma in Current Frame 
      DOUBLE PRECISION XOUT      ! Current Frame axis 1 centre
      DOUBLE PRECISION YOUT      ! Current Frame axis 2 centre
      INTEGER BINPTS             ! Number of pixels in to be considered
      INTEGER DAPTR              ! Pointer to the workspace for the radial-bin pixels
      INTEGER FRM2               ! FWHM reporting Frame
      INTEGER FSET               ! Current Frame to default SkyFrame Mapping
      INTEGER I                  ! Loop counter
      INTEGER IAPTR              ! Pointer to the workspace for the linked list
      INTEGER IAT                ! Used length of a string
      INTEGER ID                 ! Position identifier
      INTEGER LLPTR              ! Pointer to the workspace for the linked-list starts
      INTEGER MOPTR              ! Pointer to the workspace for the radial-bin modes
      INTEGER MRPTR              ! Pointer to the workspace for the radial-bin mean radii
      INTEGER NAPTR              ! Pointer to the workspace for the next address in  the linked list
      INTEGER NBIN1              ! Number of bins for each star
      INTEGER NBIN2              ! Number of bins for the mean star
      INTEGER NC                 ! Number of characters
      INTEGER NGOOD              ! Number of stars in the fit
      INTEGER NPPTR              ! Pointer to the workspace for the number of pixels in each radial bin
      INTEGER PRPTR              ! Pointer to the workspace for the mean radial-profile radii
      INTEGER PVPTR              ! Pointer to the workspace for the mean radial-profile values
      INTEGER PWPTR              ! Pointer to the workspace for the mean radial-profile weights
      INTEGER PDW                ! Pointer to the double precision workspace 
      LOGICAL GOTSKY             ! Is the current Frame a SkyFrame?
      LOGICAL SWAP               ! SkyFrame axes swapped?
      REAL AMP                   ! Gaussian amplitude
      REAL ANGLE                 ! Star orientation in degrees
      REAL ANGOUT                ! Axis orientation in current Frame      
      REAL AXROUT                ! Axis ratio in current Frame      
      REAL BACK                  ! Background level
      REAL LIMIT                 ! Radius limit
      REAL PRF( 4 )              ! Profile parameters
      REAL RSCALE                ! Scale factor for converting radial distance into bins (per star)
      REAL RSCL2                 ! Scale factor for converting radial distance into bins (mean star)
      REAL SCALE                 ! Factor to convert pixels into "arc" distance
      REAL SIG0                  ! Star sigma across minor axis
      REAL SIGMA                 ! Mean profile sigma across minor axis
      REAL STAAXS                ! Star axis ratio
      REAL STASIG                ! Star sigma
      REAL STATHE                ! Star orientation
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context
      CALL AST_BEGIN( STATUS )

*  Determine the mean ellipticity of the star images.
      CALL KPS1_STPAI( DIM1, DIM2, ARRAY, LBND, NXY, ISIZE, POS,
     :                   SIG0, AXISR, THETA, NGOOD, SIG, STATUS )
 
*  If no stars could be found to determine the ellipticity, abort.
       IF ( STATUS .NE. SAI__OK ) GO TO 999
 
*  Print a table of the axis ratios of each star used.
*  ===================================================

*  Is the Current Frame a SkyFrame? If so, set an appropriate Format 
      GOTSKY = AST_ISASKYFRAME( CFRM, STATUS ) 
      IF( GOTSKY ) THEN

*  We need to know if the axes are swapped so that we can tell which axis
*  is latitude and which is longitude. This is far more complex than it
*  should be! The only way I can see to do this is at the moment is to
*  compare the SkyFrame with a newly created default SkyFrame. SkyFrames
*  are created with axis 1 as longitude and axis 2 as latitude, but the
*  order of the axes can be swapped later.
         FSET = AST_FINDFRAME( CFRM, AST_SKYFRAME( ' ', STATUS ), ' ',
     :                         STATUS )

*  The FrameSet returned by AST_FINDFRAME converts from the supplied CFRM
*  to a similar SkyFrame with default axis order. If this Mapping is a 
*  UnitMap, the axes must be in the default order in the supplied SKyFrame.
         SWAP = .NOT. AST_ISAUNITMAP( AST_GETMAPPING( FSET, AST__BASE,
     :                                           AST__CURRENT, STATUS ), 
     :                                STATUS )

*  Take a copy of the current Frame so that we can set its attributes
*  without changing the way the star positions are formated.
         FRM2 = AST_COPY( CFRM, STATUS )

*  Set the Format attribute of Axis 1 of the SkyFrame so that
*  the FWHM values are displayed as arc-seconds, with 2 decimal places.
         FWHMUN = 'arc-seconds'
         CALL AST_SETC( FRM2, 'FORMAT(1)', 's.2', STATUS )

*  If the Current Frame is not a SkyFrame, arbitrarily use axis 1. Get its
*  units.
      ELSE
         FRM2 = AST_CLONE( CFRM, STATUS )
         FWHMUN = AST_GETC( FRM2, 'UNIT(1)', STATUS )
      END IF

*  Log the results to a text file if required.
      IF ( LOGF ) THEN
 
*  Print description of Current Frame axes:
         BUFFER = ' '
         IAT = 0
         CALL CHR_APPND( '   Axis 1:', BUFFER, IAT )
         IAT = IAT + 2
         CALL CHR_APPND( AST_GETC( CFRM, 'LABEL(1)', STATUS ), BUFFER,
     :                  IAT )
         UNIT = AST_GETC( CFRM, 'UNIT(1)', STATUS )
         IF( UNIT .NE. ' ' ) THEN
            CALL CHR_APPND( ' ( units: ''', BUFFER, IAT )
            CALL CHR_APPND( UNIT, BUFFER, IAT )
            CALL CHR_APPND( ''' )', BUFFER, IAT )
         END IF
         CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )

         BUFFER = ' '
         IAT = 0
         CALL CHR_APPND( '   Axis 2:', BUFFER, IAT )
         IAT = IAT + 2
         CALL CHR_APPND( AST_GETC( CFRM, 'LABEL(2)', STATUS ), BUFFER,
     :                  IAT )
         UNIT = AST_GETC( CFRM, 'UNIT(2)', STATUS )
         IF( UNIT .NE. ' ' ) THEN
            CALL CHR_APPND( ' ( units: ''', BUFFER, IAT )
            CALL CHR_APPND( UNIT, BUFFER, IAT )
            CALL CHR_APPND( ''' )', BUFFER, IAT )
         END IF
         CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )

*  Print description of FWHM units
         IF( FWHMUN .NE. ' ' ) THEN
            BUFFER = ' '
            IAT = 0
            CALL CHR_APPND( '   FWHM units: ''', BUFFER, IAT )
            CALL CHR_APPND( FWHMUN, BUFFER, IAT )
            CALL CHR_APPND( '''', BUFFER, IAT )
            CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
         END IF

*  Print description of angles.
         IF( GOTSKY ) THEN
            CALL FIO_WRITE( FD, '   Angles measured in degrees, from '//
     :                      'north through east.', STATUS )
         ELSE
            CALL FIO_WRITE( FD, '   Angles measured in degrees, from '//
     :                      'Axis 1 through Axis 2.', STATUS )
         END IF

*  Print the headings.
         BUFFER = ' '
         CALL FIO_WRITE( FD, BUFFER( 1:1 ), STATUS )

         WRITE ( BUFFER, '( T52, ''Gaussian'', T68, ''Axis ratio'' )' )
         CALL FIO_WRITE( FD, BUFFER, STATUS )

         WRITE ( BUFFER, '( 5X, ''Identifier'', T22, ''  Axis 1  '','/
     :           /'5X, ''  Axis 2  '', 4X, ''FWHM seeing'', 5X,'/
     :           /'''/ Angle (deg)'' )' )
         CALL FIO_WRITE( FD, BUFFER, STATUS )

         WRITE ( BUFFER, '( 5X, ''----------'', T22, '' ---------'','/
     :           /'5X, '' ---------'', 4X, ''-----------'', 5X,'/
     :           /'''-------------'' )' )
         CALL FIO_WRITE( FD, BUFFER, STATUS )

         BUFFER = ' '
         CALL FIO_WRITE( FD, BUFFER( 1:1 ), STATUS )
 
*  Consider each star.
         DO I = 1, NXY

*  Get its identifier.
            IF( GOTIDS ) THEN
               ID = IDS( I )
            ELSE
               ID = I
            END IF
 
*  If the widths of the marginal profiles were found, find the
*  parameters specifying the star shape.
            IF ( SIG( I, 5 ) .GT. 1.0E-10 ) THEN
               PRF( 1 ) = SIG( I, 1 )
               PRF( 2 ) = SIG( I, 2 )
               PRF( 3 ) = SIG( I, 3 )
               PRF( 4 ) = SIG( I, 4 )
               CALL KPS1_ELGAU( PRF, STASIG, STAAXS, STATHE, STATUS )

*  Find the parameters of the ellipe after mapping it into the current Frame. 
*  Note the half- pixel shifts because the co-ordinates are pixel indices and 
*  true co-ordinates that adhere to the Starlink convention.
               CALL KPS1_ELMAP( SWAP, CFRM, POS( I, 1 ) - 0.5D0, 
     :                          POS( I, 2 ) - 0.5D0,
     :                          STAAXS, STASIG, STATHE, MAP,
     :                          XOUT, YOUT, AXROUT, SIGOUT, ANGOUT, 
     :                          STATUS )

*  Format the centre axis values, and the seeing, converting from
*  a sigma value to an FWHM value (assuming a Gaussian profile).
               FMTVAL = AST_FORMAT( CFRM, 1, XOUT, STATUS )
               CALL CHR_LDBLK( FMTVAL )
               XVAL = FMTVAL

               FMTVAL = AST_FORMAT( CFRM, 2, YOUT, STATUS )
               CALL CHR_LDBLK( FMTVAL )
               YVAL = FMTVAL

               FMTVAL = AST_FORMAT( FRM2, 1, SIGOUT*2.35482, STATUS )
               CALL CHR_LDBLK( FMTVAL )
               FVAL = FMTVAL

*  Print the star parameters.  
               WRITE ( BUFFER, '( I11, T22, 2( SS, A13, 2X ), SS,'/
     :                /'A10, 6X, SS, G10.3 )' ) ID, XVAL,
     :                YVAL, FVAL, AXROUT
               CALL FIO_WRITE( FD, BUFFER( :78 ), STATUS )

               WRITE ( BUFFER, '( 66X, SS, G10.3 )' ) ANGOUT*57.29578
               CALL FIO_WRITE( FD, BUFFER( :76 ), STATUS )

            ELSE

*  If the marginal profile widths were not found, find the Current Frame
*  centre position.
               CALL KPS1_ELMAP( SWAP, CFRM, POS( I, 1 ) - 0.5D0, 
     :                          POS( I, 2 ) - 0.5D0,
     :                          VAL__BADR, VAL__BADR, VAL__BADR, MAP,
     :                          XOUT, YOUT, AXROUT, SIGOUT, ANGOUT, 
     :                          STATUS )

*  Format the centre axis values.
               FMTVAL = AST_FORMAT( CFRM, 1, XOUT, STATUS )
               CALL CHR_LDBLK( FMTVAL )
               XVAL = FMTVAL

               FMTVAL = AST_FORMAT( CFRM, 2, YOUT, STATUS )
               CALL CHR_LDBLK( FMTVAL )
               YVAL = FMTVAL

               WRITE ( BUFFER, '( I10, T22, 2( SS, G13.6, 2X ), 2X,'/
     :                 /'''Cannot fit this star'' )' ) ID, XVAL, YVAL
               CALL FIO_WRITE( FD, BUFFER( :74 ), STATUS )

            END IF

         END DO

*  Now print the number of stars found ok.
         BUFFER = ' '
         CALL FIO_WRITE( FD, BUFFER( 1:1 ), STATUS )

         NC = 3
         IF ( NGOOD .EQ. 1 ) THEN
            CALL CHR_PUTC( ' One star fitted successfully', BUFFER, NC )
         ELSE
            CALL CHR_PUTI( NGOOD, BUFFER, NC )
            CALL CHR_PUTC( ' stars fitted successfully', BUFFER, NC )
         END IF

         CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
         BUFFER = ' '
         CALL FIO_WRITE( FD, BUFFER( 1:1 ), STATUS )

*  End of (most) logging.
      END IF

*  Report the mean results.
*  ========================
*  Find the parameters of the mean ellipe after mapping it into the current 
*  Frame. We arbitrarily use the position of th efirst valid star as teh
*  centre position. This will be OK wince we are assuming that the pixel
*  size and PSF do not vary across the image. Also, use a sigma of SIG0 
*  so that we can get a scale factor for converting pixels into current
*  frame distance.
      AXROUT = VAL__BADR
      ANGOUT = VAL__BADR
      DO I = 1, NXY
         IF ( SIG( I, 5 ) .GT. 1.0E-10 ) THEN
            CALL KPS1_ELMAP( SWAP, CFRM, POS( I, 1 ) - 0.5D0, 
     :                       POS( I, 2 ) - 0.5D0,
     :                       AXISR, 1.0, THETA, MAP, XOUT, YOUT, 
     :                       AXROUT, SIGOUT, ANGOUT, STATUS )
            GO TO 10
         END IF
      END DO
 10   CONTINUE

*  Write to log file if required.
      IF( LOGF ) THEN

*  Print the mean results.
         IF( AXROUT .NE. VAL__BADR ) THEN
            WRITE ( BUFFER, '( ''   Mean axis ratio ='', SS, G11.4 )' ) 
     :              AXROUT
            CALL FIO_WRITE( FD, BUFFER( :31 ), STATUS )
         ELSE
            BUFFER = '   Mean axis ratio = (undefined)'
            CALL FIO_WRITE( FD, BUFFER( :32 ), STATUS )
         END IF         

         BUFFER = ' '
         CALL FIO_WRITE( FD, BUFFER( 1:1 ), STATUS )

         IF( ANGOUT  .NE. VAL__BADR ) THEN
            WRITE ( BUFFER, '( ''   Mean orientation of major axis ='','
     :              //'SS, G11.4, '' degrees'' )' ) ANGOUT * 57.29578
            CALL FIO_WRITE( FD, BUFFER( :54 ), STATUS )
         ELSE
            BUFFER = '   Mean orientation of major axis = (undefined)'
            CALL FIO_WRITE( FD, BUFFER( :48 ), STATUS )
         END IF

         BUFFER = ' '
         CALL FIO_WRITE( FD, BUFFER( 1:1 ), STATUS )

      END IF

*  Now display them on the screen.
      IF( AXROUT  .NE. VAL__BADR ) THEN
         CALL MSG_FMTR( 'A', 'G11.4', AXROUT )
      ELSE
         CALL MSG_SETC( 'A', '(undefined)' )
      END IF

      CALL MSG_OUT( ' ', '   Mean axis ratio = ^A', STATUS )
      CALL MSG_BLANK( STATUS )

      IF( ANGOUT  .NE. VAL__BADR ) THEN
         CALL MSG_FMTR( 'A', 'G11.4', ANGOUT*57.29578 )
         CALL MSG_SETC( 'A', ' degrees' )
      ELSE
         CALL MSG_SETC( 'A', '(undefined)' )
      END IF

      CALL MSG_OUT( ' ', '   Mean orientation of major axis = ^A', 
     :              STATUS )

      IF( ANGOUT  .NE. VAL__BADR ) THEN
         IF( GOTSKY ) THEN
            CALL MSG_OUT( ' ', '     (measured from north through '//
     :                    'east)', STATUS )
         ELSE
            CALL MSG_OUT( ' ', '     (measured from X through Y)', 
     :                    STATUS )
         END IF
      END IF

      CALL MSG_BLANK( STATUS )

*  Now write them to the output parameters.
      CALL PAR_PUT0R( PAXISR, AXROUT, STATUS )
      CALL PAR_PUT0R( PORIEN, ANGOUT*57.29578, STATUS )

*  Find scale factors and the numbers of radial bins.
*  ==================================================
      IF ( NGOOD .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Determine the scale factor for converting minor axis distances expressed 
*  in pixels into equivalent distances in the Current Frame.
         IF( SIG0 .NE. 0.0 ) THEN
            SCALE = SIGOUT/SIG0
         ELSE
            SCALE = 1.0
         END IF

*  If the current Frame is a SkyFrame, radial distances are displayed in
*  arc-seconds. but SIGOUT will be in radians. Add a factor to convert
*  radians into arc-seconds.
         IF( GOTSKY ) SCALE = SCALE*(180.0*3600.0/PI) 

*  Calculate the scale factors for converting radial distance to bins.
         RSCALE = RESOL1 * 2.0 / MIN( 2.0, SIG0 )
         RSCL2 = RESOL2 * 2.0 / MIN( 2.0, SIG0 )
         LIMIT = RANGE * SIG0

*  Find the number of bins to be used in binning the radial profiles of
*  each star and of the mean profile.
         NBIN1 = INT( LIMIT * RSCALE ) + 1
         NBIN2 = INT( LIMIT * RSCL2 ) + 1

*  Find the size of square to be scanned around each star to
*  accommodate the radial fitting range.
         WIDTH = 2 * MIN( MAX( 1, NINT( LIMIT * AXISR ) ), MAXRAD ) + 1
         BINPTS = WIDTH * WIDTH

*  Obtain the various workspace arrays.
*  ====================================
*
*  Workspace for the radial-profile modes.
         CALL PSX_CALLOC( NBIN1, '_REAL', MOPTR, STATUS )

*  Workspace for the radial-profile radii.
         CALL PSX_CALLOC( NBIN1, '_REAL', MRPTR, STATUS )
 
*  Workspace for the linked-list starts in each radial bin.
         CALL PSX_CALLOC( NBIN1, '_INTEGER', LLPTR, STATUS )

*  Workspace for the number of pixels in each radial bin.
         CALL PSX_CALLOC( NBIN1, '_INTEGER', NPPTR, STATUS )

*  Workspace for the pixels in a radial-profile bin.  Allow sufficient
*  room for the expansion of the token.
         CALL PSX_MALLOC( BINPTS * VAL__NBI, DAPTR, STATUS )

*  Workspace for the linked list of pixels in a radial-profile bin.
         CALL PSX_CALLOC( BINPTS, '_INTEGER', IAPTR, STATUS )

*  Workspace for the next address in the linked list of pixels of a
*  radial-profile bin.
         CALL PSX_CALLOC( BINPTS, '_INTEGER', NAPTR, STATUS )

*  Workspace for the mean radial-profile radii.
         CALL PSX_CALLOC( NBIN2, '_REAL', PRPTR, STATUS )

*  Workspace for the mean radial-profile values.
         CALL PSX_CALLOC( NBIN2, '_REAL', PVPTR, STATUS )

*  Workspace for the mean radial-profile weights.
         CALL PSX_CALLOC( NBIN2, '_REAL', PWPTR, STATUS )

*  Workspace for the fitted profile 
         CALL PSX_CALLOC( 2*NBIN2, '_DOUBLE', PDW, STATUS )

*  Report the error context if something has failed whilst trying to
*  obtain workspace.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'KPS1_SPARI_WSP',
     :        'Error obtaining workspace to determine the mean radial '/
     :        /'profile.', STATUS )
         END IF

*  Determine the form of the radial profile.
*  =========================================

         CALL KPS1_RPRFI( DIM1, DIM2, ARRAY, LBND, SIG0, AXISR, THETA,
     :                     RANGE, GAUSS, NXY, POS, SIG( 1, 5 ), RSCALE,
     :                     RSCL2, NBIN1, NBIN2, BINPTS, %VAL( MOPTR ),
     :                     %VAL( MRPTR ), %VAL( LLPTR ), %VAL( NPPTR ),
     :                     %VAL( DAPTR ), %VAL( IAPTR ), %VAL( NAPTR ),
     :                     %VAL( PVPTR ), %VAL( PRPTR ), %VAL( PWPTR ),
     :                     FWHM, AMP, BACK, SIGMA, GAMMA, STATUS )

*  Optionally plot the mean radial profile. 
*  ========================================

         CALL KPS1_PSPLT( NBIN2, SIGMA, AXISR, AMP, GAMMA, BACK, SCALE,
     :                    FWHMUN, PNMIN, %VAL( PVPTR ), %VAL( PRPTR ), 
     :                    %VAL( PWPTR ), %VAL( PDW ), STATUS )

*  Tidy up the workspace.
         CALL PSX_FREE( MOPTR, STATUS )
         CALL PSX_FREE( MRPTR, STATUS )
         CALL PSX_FREE( LLPTR, STATUS )
         CALL PSX_FREE( NPPTR, STATUS )
         CALL PSX_FREE( DAPTR, STATUS )
         CALL PSX_FREE( IAPTR, STATUS )
         CALL PSX_FREE( NAPTR, STATUS )
         CALL PSX_FREE( PRPTR, STATUS )
         CALL PSX_FREE( PVPTR, STATUS )
         CALL PSX_FREE( PWPTR, STATUS )
         CALL PSX_FREE( PDW, STATUS )

*  Report results to the user and to the optional log file. These have
*  only been calculated if FWHM is not undefined.
         IF ( FWHM .NE. VAL__BADR .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            WRITE ( BUFFER, '(''   FWHM seeing ='', SS, G11.4, A )' )
     :              FWHM * SCALE, FWHMUN
            CALL MSG_OUT( 'FWHMREP', BUFFER, STATUS )
            IF ( LOGF )
     :        CALL FIO_WRITE( FD, BUFFER( :CHR_LEN( BUFFER ) ), STATUS )

            WRITE ( BUFFER, '( ''   Gamma ='', SS, G11.4 )' ) GAMMA
            CALL MSG_OUT( 'GAMMAREP', BUFFER, STATUS )
            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            IF ( LOGF ) THEN
               CALL FIO_WRITE( FD, ' ', STATUS )
               CALL FIO_WRITE( FD, BUFFER( :21 ), STATUS )
            END IF
         END IF

*  Now write them to the output parameters.
         CALL PAR_PUT0R( PFWHM, FWHM*SCALE, STATUS )
         CALL PAR_PUT0R( PGAMMA, GAMMA, STATUS )

      ELSE
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_SPARI_NOFITS',
     :     'No stars could be fitted to a Gaussian profile.', STATUS )
      END IF

*  End the AST context
      CALL AST_END( STATUS )

  999 CONTINUE

      END
