      SUBROUTINE KPS1_SPARUB( DIM1, DIM2, ARRAY, LBND, ISIZE, RANGE,
     :                        NXY, POS, SCALE, RUNITS, LOGF, FD, PNCLER,
     :                        PNDEV, PNMIN, PNXSIZ, PNYSIZ, PNTIT,
     :                        PNABSL, PNORDL, PNMINT, PNMAJT, PNOUTT,
     :                        PNFONT, COMMNT, AXISR, THETA, FWHM, GAMMA,
     :                        WIDTH, SIG, STATUS )
*+
*  Name:
*     KPS1_SPARx
 
*  Purpose:
*     Finds a set of parameters describing a model star image
*     fitted to a set of star images and to display the results.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPS1_SPARx( DIM1, DIM2, ARRAY, LBND, ISIZE, RANGE, NXY, POS,
*                      SCALE, RUNITS, LOGF, FD, PNCLER, PNDEV, PNMIN,
*                      PNXSIZ, PNYSIZ, PNTIT, PNABSL, PNORDL, PNMINT,
*                      PNMAJT, PNOUTT, PNFONT, COMMNT, AXISR, THETA,
*                      FWHM, GAMMA, WIDTH, SIG, STATUS )
 
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
*     ARRAY( DIM1, DIM2 ) = ? (Given)
*        The input array containing the stars to be fitted.
*     LBND( 2 ) = ? (Given)
*        The lower bounds of the input array.
*     ISIZE = INTEGER (Given)
*        The length of the search square side used in finding stars
*        and calculating their ellipticity.
*     RANGE = REAL (Given)
*        The radius in units of the star 'sigma' out to which the
*        radial profile is fitted.
*     NXY = INTEGER (Given)
*        The number of stars to be fitted.
*     POS( 2, NXY ) = REAL (Given)
*        Each line comprises the approximate x then y positions of a
*        star centre.
*     SCALE = REAL (Given)
*        The scale factor to convert pixels to the physical units given
*        by argument RUNITS.  This factor is applied to the reported
*        and logged seeing size, and to the radial distances in the
*        plotted profile.
*     RUNITS = CHARACTER * ( * ) (Read)
*        The units of the radial profile after applying argument SCALE
*        to the pixel steps.  It gets used to make the default abscissa
*        label in the plot.
*     LOGF = LOGICAL (Given)
*        Logging switch.  If true the results of the analysis, including
*        a table of parameters for each star, and indiciating omitted
*        stars will be written to the file specified by the file
*        descriptor.
*     FD = INTEGER (Given)
*        The file descriptor of the log file.  It will be ignored if
*        the logging switch is off.  The file must be already open.
*     PNCLER = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to see if the
*        user wishes the display to be cleared.
*     PNDEV = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be associated with
*        a graphics workstation.
*     PNMIN = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to decide
*        whether to plot the profile along the minor or major axis.
*     PNXSIZ = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        required x extent of the FRAME picture in metres.
*     PNYSIZ = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        required y extent of the FRAME picture in metres.
*     PNTIT = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        plot title.
*     PNABSL = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        plot abscissa axis label.
*     PNORDL = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        plot ordinate axis label.
*     PNMINT = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        numbers of minor tick marks per major tick.
*     PNMAJT = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        numbers of major tick marks.
*     PNOUTT = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to determine
*        whether the ticks should appear inside or outside the grid
*        region of the plot.
*     PNFONT = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to determine
*        the fount to be used in the plot.
*     COMMNT = CHARACTER * ( * ) (Given)
*        A comment to store in the AGI database with the FRAME and
*        DATA pictures.
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
 
*  [optional_subroutine_items]...
*  Algorithm:
*     -  Determine the ellipticity of the star images
*     -  Print headings and a table of the ellipse parameters for
*     each star used. Print the mean ellipse parameters.
*     -  Determine the model radial star profile and print the results.
 
*  Notes:
*     -  This is a server routine for PSF.
*     -  %PNCLER, %PNDEV, %PNMIN, %PNXSIZ, %PNYSIZ, %PNTIT, %PNABSL,
*     %PNORDL, %PNMINT, %PNMAJT, %PNOUTT, %PNFONT, and %COMMNT are
*     passed directly to KPS1_PSPLT.  It has been coded this way to
*     prevent a much-longer PSF routine, and numerous other parameters
*     being passed between PSF and the various subroutines for the
*     fitting.  This way the fitting is self contained. (of note is
*     that the dimensions of the work arrays are known here but not
*     from the PSF level.)  One way to remove the PN arguments (with
*     some efficiency cost) is to copy the profile to a temporary NDF,
*     and pass the identifier back with the AMP, BACK, and SIGMA
*     variables, and then call KPS1_PSPLT from PSF.
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B, UB as appropriate.  The
*     array supplied to the routine must have the data type specified.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (Durham Univ.)
*     MJC: Malcolm J. Currie (STARLINK)
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
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions
 
*  Arguments Given:
      INTEGER
     :  DIM1, DIM2,
     :  LBND( 2 ),
     :  FD,
     :  ISIZE,
     :  NXY
 
      BYTE
     :  ARRAY( DIM1, DIM2 )
 
      REAL
     :  RANGE,
     :  SCALE,
     :  POS( 2, NXY )
 
      CHARACTER * ( * )
     :  COMMNT,
     :  RUNITS,
     :  PNCLER,
     :  PNDEV,
     :  PNMIN,
     :  PNXSIZ,
     :  PNYSIZ,
     :  PNTIT,
     :  PNABSL,
     :  PNORDL,
     :  PNMINT,
     :  PNMAJT,
     :  PNOUTT,
     :  PNFONT
 
      LOGICAL
     :  LOGF
 
*  Arguments Returned:
      REAL
     :  AXISR,
     :  FWHM,
     :  GAMMA,
     :  SIG( NXY, 5 ),
     :  THETA
 
      INTEGER
     :  WIDTH
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  External References:
      INTEGER CHR_LEN            ! Length of a character string less
                                 ! any trailing blanks
 
*  Local Constants:
      INTEGER MAXRAD             ! Maximum radius in pixels of the
                                 ! radial profile
      PARAMETER ( MAXRAD = 100 )
 
      REAL RESOL1                ! Resolution (no. of bins per pixel
                                 ! spacing) for binning the radial
                                 ! profiles of each star
      PARAMETER ( RESOL1 = 2.5 )
 
      REAL RESOL2                ! Resolution (no. of bins per pixel
                                 ! spacing) for binning the mean radial
                                 ! profile
      PARAMETER ( RESOL2 = 10.0 )
 
*  Local Variables:
      REAL
     :  ANGLE,                   ! Star orientation in degrees
     :  AMP,                     ! Gaussian amplitude
     :  BACK,                    ! Background level
     :  LIMIT,                   ! Radius limit
     :  PRF( 4 ),                ! Profile parameters
     :  RSCALE,                  ! Scale factor for converting radial
                                 ! distance into bins (per star)
     :  RSCL2,                   ! Scale factor for converting radial
                                 ! distance into bins (mean star)
     :  SEEING,                  ! FWHM seeing disk
     :  SIG0,                    ! Star sigma across minor axis
     :  SIGMA,                   ! Mean profile sigma across minor axis
     :  STAAXS,                  ! Star axis ratio
     :  STASIG,                  ! Star sigma
     :  STATHE                   ! Star orientation
 
      INTEGER
     :  BINPTS,                  ! Number of pixels in to be considered
     :  DAPTR,                   ! Pointer to the workspace for the
                                 ! radial-bin pixels
     :  I,                       ! Loop counter
     :  IAPTR,                   ! Pointer to the workspace for the
                                 ! linked list
     :  LLPTR,                   ! Pointer to the workspace for the
                                 ! linked-list starts
     :  MOPTR,                   ! Pointer to the workspace for the
                                 ! radial-bin modes
     :  MRPTR,                   ! Pointer to the workspace for the
                                 ! radial-bin mean radii
     :  NAPTR,                   ! Pointer to the workspace for the
                                 ! next address in  the linked list
     :  NBIN1,                   ! Number of bins for each star
     :  NBIN2,                   ! Number of bins for the mean star
     :  NC,                      ! Number of characters
     :  NGOOD,                   ! Number of stars in the fit
     :  NPPTR                    ! Pointer to the workspace for the
                                 ! number of pixels in each radial bin
 
      INTEGER
     :  PRPTR,                   ! Pointer to the workspace for the
                                 ! mean radial-profile radii
     :  PVPTR,                   ! Pointer to the workspace for the
                                 ! mean radial-profile values
     :  PWPTR                    ! Pointer to the workspace for the
                                 ! mean radial-profile weights
 
      CHARACTER
     :  BUFFER * 80              ! Buffer to write results
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Determine the mean ellipticity of the star images.
      CALL KPS1_STPAUB( DIM1, DIM2, ARRAY, LBND, NXY, ISIZE, POS,
     :                   SIG0, AXISR, THETA, NGOOD, SIG, STATUS )
 
*  If no stars could be found to determine the ellipticity, abort.
       IF ( STATUS .NE. SAI__OK ) GOTO 999
 
*  Print a table of the axis ratios of each star used.
*  ===================================================
      IF ( LOGF ) THEN
 
*  Print the headings.
         BUFFER = ' '
         CALL FIO_WRITE( FD, BUFFER( 1:1 ), STATUS )
 
         WRITE ( BUFFER, '( T52, ''Gaussian'', T68, ''Axis ratio'' )' )
         CALL FIO_WRITE( FD, BUFFER, STATUS )
 
         WRITE ( BUFFER, '( 5X, ''Identifier'', T22, '' X co-ord.'','/
     :           /'5X, '' Y co-ord.'', 4X, ''FWHM seeing'', 5X,'/
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
 
*  If the widths of the marginal profiles were found, find the
*  parameters specifying the star shape.
            IF ( SIG( I, 5 ) .GT. 1.0E-10 ) THEN
               PRF( 1 ) = SIG( I, 1 )
               PRF( 2 ) = SIG( I, 2 )
               PRF( 3 ) = SIG( I, 3 )
               PRF( 4 ) = SIG( I, 4 )
               CALL KPS1_ELGAU( PRF, STASIG, STAAXS, STATHE, STATUS )
 
*  Calculate the FWHM seeing disk, assuming a Gaussian profile and
*  print the star parameters.  Note the half- pixel shifts because the
*  co-ordinates are pixel indices and true co-ordinates that adhere to
*  the Starlink convention.
               SEEING = STASIG * 2.35482 * SCALE
               ANGLE = STATHE * 57.29578
               WRITE ( BUFFER, '( I11, T22, 2( SS, G13.6, 2X ), SS,'/
     :                /'G10.3, 6X, SS, G10.3 )' ) I, POS( 1, I ) - 0.5,
     :                 POS( 2, I ) - 0.5, SEEING, STAAXS
               CALL FIO_WRITE( FD, BUFFER( :78 ), STATUS )
 
               WRITE ( BUFFER, '( 66X, SS, G10.3 )' ) ANGLE
               CALL FIO_WRITE( FD, BUFFER( :76 ), STATUS )
 
            ELSE
 
*  If the marginal profile widths were not found, print the details.
*  Again note the change from pixel indices to co-ordinates.
               WRITE ( BUFFER, '( I10, T22, 2( SS, G13.6, 2X ), 2X,'/
     :                 /'''Cannot fit this star'' )' ) I,
     :                 POS( 1, I ) - 0.5, POS( 2, I ) - 0.5
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
 
*  Print the mean results.
         WRITE ( BUFFER, '( ''   Mean axis ratio ='', SS, G11.4 )' )
     :           AXISR
         CALL FIO_WRITE( FD, BUFFER( :31 ), STATUS )
 
         BUFFER = ' '
         CALL FIO_WRITE( FD, BUFFER( 1:1 ), STATUS )
 
         WRITE ( BUFFER, '( ''   Mean orientation of major axis ='','/
     :           /'SS, G11.4, '' degrees'' )' ) THETA * 57.29578
         CALL FIO_WRITE( FD, BUFFER( :54 ), STATUS )
 
         BUFFER = ' '
         CALL FIO_WRITE( FD, BUFFER( 1:1 ), STATUS )
 
*  End of logging.
      END IF
 
*  Report the mean results.
*  ========================
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      WRITE ( BUFFER, '( ''   Mean axis ratio ='', SS, G11.4 )' ) AXISR
      CALL MSG_OUT( 'AXISRATIO', BUFFER( :31 ), STATUS )
 
      BUFFER = ' '
      WRITE ( BUFFER, '( ''   Mean orientation of major axis ='','/
     :        /'SS, G11.4, '' degrees'' )' ) THETA * 57.29578
      CALL MSG_OUT( 'ORIENTATION', BUFFER( :54 ), STATUS )
 
      IF ( NGOOD .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
 
*  Find scale factors and the numbers of radial bins.
*  ==================================================
 
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
         CALL PSX_CALLOC( BINPTS, '_UBYTE', DAPTR, STATUS )
 
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
 
*  Report the error context if something has failed whilst trying to
*  obtain workspace.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'KPS1_SPARx_WSP',
     :        'Error obtaining workspace to determine the mean radial '/
     :        /'profile.', STATUS )
         END IF
 
*  Determine the form of the radial profile.
*  =========================================
         CALL KPS1_RPRFUB( DIM1, DIM2, ARRAY, LBND, SIG0, AXISR, THETA,
     :                     RANGE, POS, NXY, SIG( 1, 5 ), RSCALE,
     :                     RSCL2, NBIN1, NBIN2, BINPTS, %VAL( MOPTR ),
     :                     %VAL( MRPTR ), %VAL( LLPTR ), %VAL( NPPTR ),
     :                     %VAL( DAPTR ), %VAL( IAPTR ), %VAL( NAPTR ),
     :                     %VAL( PVPTR ), %VAL( PRPTR ), %VAL( PWPTR ),
     :                     FWHM, AMP, BACK, SIGMA, GAMMA, STATUS )
 
*  Optionally plot the mean radial profile.
*  ========================================
         CALL KPS1_PSPLT( NBIN2, SIGMA, AXISR, AMP, GAMMA, BACK, SCALE,
     :                    RUNITS, PNCLER, PNDEV, PNMIN, PNXSIZ, PNYSIZ,
     :                    PNTIT, PNABSL, PNORDL, PNMINT, PNMAJT, PNOUTT,
     :                    PNFONT, COMMNT, %VAL( PVPTR ), %VAL( PRPTR ),
     :                    %VAL( PWPTR ), STATUS )
 
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
 
*  Report results to the user and to the optional log file. These have
*  only been calculated if FWHM is not undefined.
         IF ( FWHM .NE. VAL__BADR .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            WRITE ( BUFFER, '(''   FWHM seeing ='', SS, G11.4, A )' )
     :              FWHM * SCALE, RUNITS
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
 
      ELSE
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_SPARx_NOFITS',
     :     'No stars could be fitted to a Gaussian profile.', STATUS )
      END IF
 
  999 CONTINUE
 
      END
