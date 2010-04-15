      SUBROUTINE ELLFOU(STATUS)
*+
*  Name:
*     ELLFOU

*  Purpose:
*     Ellipse fitting galaxy profiles using contour analysis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELLFOU( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Performs the calculations to fit galaxy profiles using ellipses.
*     The method used involves fitting an ellipse to the shape
*     of the isophote contour.
*
*     The output includes the ellipse parameters, the azimuthally-averaged
*     intensity around them and
*     the Fourier descriptors. The position of the centre of the
*     galaxy (and a number of other parameters) must be specified
*     interactively (using cursor or keyboard) by the user.
*
*     If MODE is false, a list containing the location of
*     galaxies within an image, is obtained from an ASCII file.
*     profiles are generated for all these objects.
*
*     If MODE is true, a value for the parameter CURSOR
*     is required. If CURSOR is true, then a cursor/mouse is used (in
*     conjunction with the most recent image displayed) to determine
*     information such as proposed galaxy centre and the largest
*     ellipse radius to be used. If CURSOR is false, a keyboard is
*     used for all input required by the application.

*  Usage:
*     ELLFOU MODE BACK SIGMA PSIZE ZEROP ARDFIL DEVICE OUT (OUTCAT)
*            AUTOL AUTOLT FRZORI [CURSOR] [IN] [ORIGIN] (FINE)
*            [RLIM] (LIM1) (LIM2) [SAME] [AGAIN] [INFILE]
*            [IMGDEV] (COLOUR) (ANGCON) (ANGOFF) (FRACT)

*  ADAM Parameters:
*     AGAIN=_LOGICAL (Read)
*        Allows the user to elect to repeat the profiling operation
*        on the current input image. Profiling is repeated if
*        AGAIN=TRUE.
*     ANGCON=_LOGICAL (Read)
*        Position angle convention. TRUE=clockwise positive
*     ANGOFF=_REAL (Read)
*        Positive angle offset. Units degrees.
*     ARDFIL=_CHAR (Read)
*        The name of an ARD file to be used to mask out regions of the
*        image that are not to be used.
*     AUTOL=_LOGICAL (Read)
*        Is a better estimate of the galaxy centre position to be
*        obtained? If AUTOL=FALSE the user estimate is employed,
*        otherwise the application examines the area of the image near
*        the user defined co-ordinates for a better estimate.
*     AUTOLT=_LOGICAL (Read)
*        The type of centroiding method used. N=centroid, Y=weighted mean
*     BACK=_REAL (Read)
*        The background count value for the image. Units counts.
*     COLOUR=_INTEGER (Read)
*        Colour of the pen used to mark the position of the galaxy
*        centre.
*     CURSOR=_LOGICAL (Read)
*        Whether the galaxy locations are to be identified using the
*        graphics cursor or the keyboard. Cursor/mouse is used if
*        CURSOR=TRUE.
*     DEVICE=_DEVICE (Read)
*        The name of the graphics device on which the graph of results
*        should be displayed.
*     FRACT=_REAL (Read)
*        Fraction of pixels that must be present for a fit to be okay.
*     FINE=_REAL (Read)
*        A factor modifying the default separation of isophotal
*        separation of the pixels used to create ellipses.
*        The default value is 1. Decreasing this value increases the
*        number of profiles generated for a given object.
*        Must be issued from the command line.
*     FRZORI=_LOGICAL (Read)
*        Allows the origin given (or the values determined via AUTOL)
*        to remain unchanged throughout the current profiling
*        operation. The origin is free to move if FRZORI=FALSE.
*     IMGDEV=_DEVICE (Read)
*        Name of the graphics device displaying the current image.
*     INFILE=_CHAR (Read)
*        Name of a text file containing the co-ordinates of galaxies
*        to be profiled. (Only used in file mode i.e. MODE=FALSE).
*        Co-ordinates are in the Current co-ordinate frame of the WCS
*        component of IN.  The file may also contain a third column
*        containing the background count value. If this is found to be
*        absent the global background count value (BACK) is substituted.
*     IN=_NDF (Read)
*        The name of the source NDF data structure/file.
*     LIM1=_REAL (Read)
*        The maximum ratio that is permitted between the average mean
*        count value of the two preceeding radii profiled and that of
*        the current radius. If this ratio is exceeded, the profiling
*        operation stops.
*        Must be issued from the command line.
*     LIM2=_REAL (Read)
*        The lower limit for mean profile count value. If the mean count
*        value for the current profile drops below this value the
*        profiling operation stops. Must be issued from the command line.
*     MODE=_LOGICAL (Read)
*        Whether the application is to run in file input mode or
*        interactively. Interactive MODE=TRUE. File mode=FALSE.
*     ORIGIN=_CHAR (Read)
*        Image co-ordinates for the galaxy origin point to be used. To be
*        given in the Current coordinate system of the source NDF.
*     OUT=_CHAR (Read)
*        File name for the output text file containing the profile
*        data.
*     OUTCAT=_CHAR (Read)
*        File name for an output file which is written using the CAT
*        library.  See SUN/181.  The type of catalogue which is written
*        depends on the file extension to the filename presented here.
*        A file ending .txt will be written as a STL (Small Text List)
*        file, and one ending .fits will be written as a FITS file.
*     PSIZE=_REAL (Read)
*        The size of each pixel in arc seconds.  If the image contains
*        a SKY co-ordinate frame this value will be determined
*        automatically.
*     RLIM=_REAL (Read)
*        Radius at which the profiling will be stopped. Units pixels.
*     SAME=_LOGICAL (Read)
*        Is the results graph to be displayed on the device currently
*        displaying the input image? Only valid if CURSOR is true.
*        If SAME is set to true then the user is prompted to identify
*        the quadrant of the input device in which graph will be
*        displayed.
*     SIGMA=_REAL (Read)
*        The standard deviation of the background count value. Units counts.
*     ZEROP=_REAL (Read)
*        Zero point of the scale for surface brightness plots. Units
*        magnitudes per arc seconds.

*  Examples:
*     ellfou mode=true back=6200. sigma=390. psize=1.
*            zerop=27.5 ardfil=^ardfile.dat device=xwindow
*            out=elf autol=true frzori=true cursor=true
*            same=true
*        Profiles are obtained for the image co-ordinates determined
*        using the cursor/mouse on the DATA image currently displayed
*        on device XWINDOW. The background count value of that image is
*        6200 with an associated standard deviation of 390. The
*        magnitude scale assumed has a zero point of 27.5, all profiles
*        will be output to text file ELF, the final results will
*        also be plotted on the XWINDOW device and the galaxy centre
*        co-ordinates are allowed to vary.
*
*     ellfou mode=true back=1267. sigma=45. psize=2.
*            zerop=26.2 ardfil=^ardfile.dat device=xwindow
*            out=elf2 autol=true frzori=true cursor=true
*            same=false imgdev=x2windows
*
*        Profiles are obtained for the current data image on device
*        XWINDOW. The results are output onto device X2WINDOWS. An
*        ARD file definition in ARDFILE.DAT is used to identify parts
*        of the image that may be used in the profiling operation.
*        An attempt will be made to improve the co-ordinates indicated
*        via the cursor/mouse but the galaxy centre co-ordinates will
*        not be allowed to vary from one profile to the next.
*
*     ellfou mode=true back=6200 sigma=390. zerop=27.5
*            ardfil=^ardfile.dat out=elf autol=true frzori=true
*            cursor=false in=p2 origin="12:36:53.42 62:12:21.8"
*            rlim=10. imgdev=x2windows
*
*        Profiles for the object at the co-ordinates indicated on image
*        P2 are obtained out to a radius of 10 pixels. The Current
*        co-ordinate frame of P2 is in the 'SKY' domain.  Pixel size in
*        arcseconds is determined automatically from the SKY co-ordinates.
*        The results are output to device X2WINDOWS and to a file text
*        file ELF. The background count is 6200 with an associated
*        standard deviation of 390.
*
*     ellfou mode=false infile=coords ardfil=^ardfile.dat in=jet
*            frzori=false back=3713 sigma=23 rlim=20 psize=0.5
*            zerop=26.2 autol=false
*
*        The program is operated in file mode where co-ordinates
*        of the galaxies to be profiled are read from file
*        COORDS. An ARD file ARDFILE.DAT is used to identify parts of
*        the image that can be used. The global value for the
*        background count value is input in case the COORDS file does not
*        contain a third column with local background values in.
*        The image used as the source is JET. During profiling the
*        galaxy centre is allowed to vary from that originally
*        provided in the file. The profiling operation ceases
*        if the ellipse radius reaches 20 pixels.

*  Notes:
*     The parameters surrounded by curved brackets may only be changed
*     from the command line.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1994 (GJP)
*       Original version.
*     16-OCT-1996 (GJP)
*       NAG free version.
*     27-JAN-1997 (GJP).
*       Modified output formatting to make it work better with
*       very large images. Some pointer usage slightly modified.
*     8-NOV-1999 (MBT).
*       Modified to work with World Coordinate System components.
*     5-Feb-2000 (NG).
*       Added OUTCAT parameter, assorted fixes.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL CURSOR                  ! Keyboard or cursor origin selction
      LOGICAL MODE                    ! Interactive or file mode
      REAL    TEMP                    ! Dummy
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialise the random number generator.
      CALL ELF1_RAND(0,2001,TEMP,STATUS)

*   Show that the application is running.
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT(' ','ESP ELLFOU running.',STATUS)

*   Get the user selection of working interactively or by file?
      CALL PAR_GET0L('MODE',MODE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Begin AST context.
      CALL AST_BEGIN(STATUS)

*   Transfer control to a fully interactive mode or to file input
*   handling routine.
      IF (.NOT.MODE) THEN

*      Pass control to a file input routine.
         CALL ELF1_FMODE(STATUS)

      ELSE

*      Get the user selection of using the cursor or a keyboard?
         CALL PAR_GET0L('CURSOR',CURSOR,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Pass control to an appropriate interactive routine.
         IF (.NOT.CURSOR) THEN
*         Keyboard user input.
            CALL ELF1_KMODE(STATUS)
         ELSE
*         Keyboard and mouse input.
            CALL ELF1_CMODE(STATUS)
         END IF

      END IF

*   Abort the program.
 9999 CONTINUE

*   Leave AST context.
      CALL AST_END(STATUS)

      END



      SUBROUTINE ELF1_BOUNDS(V,R,SLOOPS,X,MINX,MAXX,STATUS)
*+
*  Name:
*     ELF1_BOUNDS

*  Purpose:
*     Determines the new bounds within which an ellipse parameter may be
*     iteratively varied.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_BOUNDS(V,R,SLOOPS,X,MINX,MAXX,STATUS)

*  Description:
*     Looks at the residual values that were found for ellipses generated
*     with various values of the current parameter. Adjusts the range over
*     which the parameter may subsequently be adjusted. Is performed in such a
*     way as to generate a slow approach to the final value.

*  Arguments:
*     V(40) = REAL (Given)
*        Parameter values tried.
*     R(40) = REAL (Given)
*        Residuals obtained for each of the current parameter values tried.
*     SLOOPS = INTEGER (Given)
*        Number of values of the current parameter that were considered.
*     X = REAL (Given and Returned)
*        Initial value of ellipse parameter and the value to be used once
*        the parameter bounds values have been modified.
*     MINX = REAL (Given and Returned)
*        Lower limit of the current parameter.
*     MAXX = REAL (Given and Returned)
*        Upper limit of the current parameter
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER SLOOPS                  ! Number of parameter values tried.
      REAL R(40)                      ! Fit residuals found for each of
                                      ! the parameter values in V()
      REAL V(40)                      ! Current parameter values tried

*  Arguments Returned:

*  Arguments Given and Returned:
      REAL MAXX                       ! Maximum parameter value
      REAL MINX                       ! Minimum parameter value
      REAL X                          ! Parameter value

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Index storage
      INTEGER K                       ! Index storage
      REAL HIGH                       ! Greatest residual value found
      REAL LOW                        ! Lowest residual value found
      REAL RANGE                      ! Temporary storage
      REAL VALUE                      ! Mean residual
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialise the counters used to retain the indices of the
*   largest and smallest deviations.
      K=0
      J=0

*   Look through the stored deviations and find the smallest
*   and largest.
      HIGH=-1.E+20
      LOW=1.E+20
      DO 10 I=2,SLOOPS-1

*      Consider the average of three adjacent values (helps to
*      smooth out noise).
         VALUE=R(I-1)+R(I)+R(I+1)

*      Retain value found and index if it is the largest.
         IF (VALUE.GT.HIGH) THEN
           HIGH=VALUE
           J=I
         END IF

*      Retain value found and index if it is the smallest.
         IF (VALUE.LT.LOW) THEN
           LOW=VALUE
           K=I
         END IF

 10   CONTINUE

*    Only modify parameters if a maximum was found in the
*    deviation versus paramter values.
       IF (J.NE.0) THEN

*       Cope with the smallest deviation being at the edge of the
*       parameter space being tested.
         IF ((K.EQ.2).OR.(K.EQ.SLOOPS-1)) THEN

*         Adjust the range of values and current paramter
*         value to encompass the deviation versus parameter minimum.
*         Is weighted toward the current value to avoid oscilations.
            RANGE=MAXX-MINX
            X=(V(K)+5*X)/6.
            MINX=X-RANGE/2.
            MAXX=X+RANGE/2.

         ELSE

*         Adjust (tighten) the current parameter bounds.
            IF (V(J).GT.X) THEN
              MAXX=(V(SLOOPS-1)+V(SLOOPS)*2.)/3.
            ELSE
              MINX=(V(1)*2.+V(2))/3.
            END IF

*         Adjust the current parameter value. Again, weighted in
*         favour of the current value.
            X=(V(K)*.1+X+(MINX+MAXX)/2.)/2.1

         END IF

      END IF

 9999 CONTINUE

      END


      SUBROUTINE ELF1_CIRC(XE,YE,NPIX,RCIRC,STATUS)
*+
*  Name:
*     ELF1_CIRC

*  Purpose:
*     Determines a radius value for a circle that might fit the ellipse
*     if it were a circle.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_CIRC(XE,YE,NPIX,RCIRC,STATUS)

*  Description:
*     Looks through all the pixels and finds the maximum separation.

*  Arguments:
*     XE(ELF__PIXEL) = REAL (Given)
*        Pixel X co-ordinates. Units pixels.
*     YE(ELF__PIXEL) = REAL (Given)
*        Pixel Y co-ordinates. Units pixels.
*     NPIX = INTEGER (Given)
*        Number of pixels taken from the current isophote.
*     RCIRC = REAL (Returned)
*        Circle radius determined. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER NPIX                    ! Number of pixels in the current
                                      ! isophote and hence
      REAL XE(ELF__PIXEL)             ! X/Y co-ords of the pixels
                                      ! in the current isophote
      REAL YE(ELF__PIXEL)             ! X/Y co-ords of the pixels
                                      ! in the current isophote

*  Arguments Returned:
      REAL RCIRC                      ! Circle radius

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      REAL SEP                        ! Pixel-pixel separation

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the circle radius by looking for the two points furthest apart.
      DO 20 I=1,NPIX-1
         DO 30 J=I+1,NPIX

*         Find inter-pixel separation.
            SEP=(XE(I)-XE(J))*(XE(I)-XE(J))
     :                       +(YE(I)-YE(J))*(YE(I)-YE(J))
            SEP=SQRT(SEP)/2.

*         Keep value if is biggest so far.
            IF (SEP.GT.RCIRC) RCIRC=SEP

 30      CONTINUE
 20   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE ELF1_PRO(DMODE,ANGCON,ANGOFF,FRZORI,FINE,
     :                    LIM2,PSIZE,RLIM,BACK,SIGMA,ELEMS,ARRP,
     :                    PRANGE,XCO,YCO,VALIDP,RESULT,STATUS)
*+
*  Name:
*     ELF1_PRO

*  Purpose:
*     Routine wherein the ellipse parameters and Fourier descriptors
*     are determined.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_PRO(DMODE,ANGCON,ANGOFF,FRZORI,FINE,LIM2,PSIZE,
*                   RLIM,BACK,SIGMA,ELEMS,ARRP,PRANGE,XCO,YCO,VALIDP,
*                   RESULT,STATUS)

*  Description:
*     The routine works by extracting from the image all pixels (up to a
*     a limit defined by ELF__PIXEL) within a given isophotal range and
*     then attempting to fit an ellipse to these. The minimisation of the
*     method attempts to reduce the distance of each pixel from the fitted
*     ellipse. Ellipticity, radius and position angle are varied (initially)
*     over a wide range in order to avoid local minima caused by pixel noise.
*
*     An attempt to fit the pixel positions is only made if at least 5
*     pixels have been found.
*
*     To aid convergence toward accurate ellipse parameters, an initial guess
*     is made at the ellipse parameters for each isophote by routines
*     ELF1_CIRC and ELF1_RESID. If ellipse parameters have already been
*     determined at different isophotes, the parameters from the previous
*     isophotes are used to help refine the guessed initial parameter values.
*
*     Once the ellipse parameters have been determined these are used
*     to normalise the pixel positions to a unit circle. The brightness
*     variations around the circle are analysed to determine the Fourier
*     descriptors.

*  Arguments:
*     DMODE = INTEGER (Given)
*        Is a display to be generated? 0=No 1=Yes.
*     ANGCON = LOGICAL (Given)
*        Position angle rotation convention. TRUE=clockwise positive.
*     ANGOFF = REAL (Given)
*        Position angle offset. Units degrees.
*     FRZORI = LOGICAL (Given)
*        Is the galaxy centre given to be used unchanged or is it allowed to
*        be modified?
*     FINE = REAL (Given)
*        Determines how closely spaced the radii are to be.
*     LIM2 = REAL (Given)
*        Defines a lower limit of ellipse mean count value at which point the
*        ellipse fitting is terminated.
*     PSIZE = REAL (Given)
*        The pixel size. Units arc secs.
*     RLIM = REAL (Given)
*        The maximum distance from the origin at which pixels will still
*        be considered to be part of the object.
*     BACK = REAL (Given)
*        The background count for the image. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of BACK. Units counts.
*     ELEMS = INTEGER (Given)
*        Number of elements/pixels in the image array. Units pixels.
*     ARRP(1) = INTEGER (Given)
*        Pointer to the image array.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     XCO = REAL (Given)
*        X index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     YCO = REAL (Given)
*        Y index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     VALIDP = INTEGER (Returned)
*        Number of valid radius fits stored.
*     RESULT(17,ELF__RESUL) = REAL (Returned)
*        Fitted ellipse parameters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     20-Mar-1993 (GJP)
*     (Original version)
*     20-FEB-1997 (GJP)
*     Removed improper use of LOGICAL as an INTEGER.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ARRP(1)                 ! The image array pointer
      INTEGER DMODE                   ! Is a display to be generated?
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL FRZORI                  ! Frozen galaxy origin?
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER PRANGE(2)               ! Image size in pixels
      REAL ANGOFF                     ! Position angle offset
      REAL BACK                       ! Background count value
      REAL FINE                       ! Radii separation factor
      REAL LIM2                       ! Determines lower count limit
      REAL PSIZE                      ! Pixel size
      REAL RLIM                       ! Sampling radius maximum
      REAL SIGMA                      ! Standard deviation of BACK
      REAL XCO                        ! X co-ordinate of the object
      REAL YCO                        ! Y co-ordinate of the object

*  Arguments Returned:
      INTEGER VALIDP                  ! Number of valid isophote fits
      REAL RESULT(17,ELF__RESUL)      ! Ellipse and FD parameters

*  Arguments Given and Returned:
      REAL RADIUS                     ! Ellipse major axis radius
      REAL XCR(ELF__MXPOI)            ! X co-ordinates for fit
                                      ! ellipse points
      REAL XE(ELF__PIXEL)             ! X co-ordinate of suitable pixels
      REAL YCR(ELF__MXPOI)            ! Y co-ordinate for fit
                                      ! ellipse points
      REAL YE(ELF__PIXEL)             ! Y co-ordinate of suitable pixels

*  Local variables:
      CHARACTER *256 TEXT             ! An output text string
      INTEGER COUNTR                  ! Number of pixels within the
                                      ! required brightness range
      INTEGER FIRST                   ! First radius fitted flag
      INTEGER FOUND                   ! The number of ellipse points for
                                      ! which an interpolated value
                                      ! was found
      INTEGER USED(ELF__MXPOI)        ! Was a sensible value obtained for a
                                      ! given ellipse point via interpolation?
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER LOOPMX                  ! Maximum number of iteration loops
                                      ! in the fit residual minimisation
      INTEGER LOOPS                   ! Iteration loops so far
      INTEGER NUMB                    ! Number of isophotal levels
                                      ! being considered
      INTEGER POINTS                  ! Number of points in the
                                      ! ellipse generated
      INTEGER SLOOPS                  ! Number of guesses at a parameter
                                      ! value made per iteration
      INTEGER Z                       ! loop variable
      REAL ANG(ELF__MXPOI)            ! Angle of a pixel when creating
                                      ! an ellipse
      REAL ANGLE                      ! Initial position angle guess
      REAL BOUNDS                     ! Pixel range within the isophote
      REAL DIST                       ! Distance from the current
                                      ! galaxy origin
      REAL ELLIP                      ! Ellipticity value
      REAL FRACT                      ! Proportion of ellipse points required
      REAL HIGH                       ! Upper brightness limit on this
                                      ! ispohote
      REAL LOGINC                     ! Isophote increment
      REAL LOW                        ! Lower brightness limit on this
                                      ! isophote
      REAL MAXANG                     ! Upper limit of position angles
      REAL MAXE                       ! Upper limit of the ellipticity
      REAL MAXRAD                     ! Upper limit of radii values
      REAL MAXY                       ! Maximum permitted deviation
                                      ! from current x/y location of
                                      ! galaxy centre
      REAL MEAN                       ! Mean brightness of the current
                                      ! pixels
      REAL MIN                        ! Minimum value found
      REAL MINANG                     ! Lower limit of position angles
      REAL MINE                       ! Lower limit of the ellipticity
      REAL MINRAD                     ! Lower limit of the radii values
      REAL PCV(ELF__PIXEL)            ! Brightness of the isophotal pixels
      REAL R(40)                      ! Fit residuals found for trial
                                      ! parameter values
      REAL RAD(ELF__MXPOI)            ! Ellipse origin/pixel separation
      REAL RESDU                      ! A measure of the pixel brightness
                                      ! variation
      REAL RND                        ! A random number
      REAL S                          ! A random value
      REAL S1                         ! Positive amount
      REAL S2                         ! Negative amount
      REAL SDP                        ! Standard deviation of the pixels
                                      ! on the fit ellipse
      REAL SEARCH                     ! Radius about the galaxy centre
                                      ! from which pixels may be taken
      REAL SUMS                       ! Position residuals of a fit
      REAL TA                         ! Direction in which the X/Y
                                      ! co-ordinate of the galaxy is
                                      ! allowed to move
      REAL TEMP                       ! Temporary storage
      REAL THETA                      ! Position angle
      REAL V(40)                      ! The parameter values corresponding
                                      ! to R()
      REAL VA(ELF__MXPOI)             ! Values of image pixel count at the
                                      ! ellipse pixel points
      REAL VALUE                      ! Temporary value
      REAL VMAX                       ! Brightness of the brightest pixel
                                      ! within the chosen radius
      REAL VMIN                       ! Brightness of the faintest pixel
                                      ! within the chosen radius
      REAL X                          ! Trial X location for the galaxy
                                      ! centre
      REAL XO                         ! Current centre for the galaxy
      REAL Y                          ! Trial Y location for the galaxy
                                      ! centre
      REAL YO                         ! Current centre for the galaxy

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialise sensible profile count.
      VALIDP=0

*   Check the state of the parameter FRACT, to see if there is a
*   suggested value on the command line.
*   Otherwise, use the value specified in elf_par.
      CALL PAR_STATE('FRACT',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('FRACT',FRACT,STATUS)
         CALL MSG_OUT(' ','Command line FRACT value used.',STATUS)
      ELSE
         FRACT=ELF__FRACT
      END IF

*   Heading for the output.
      IF (DMODE.GT.0) THEN
         CALL MSG_BLANK(STATUS)
         TEXT='  X       Y      Points    Rad(*)    Count     PA   '//
     :       '  Ellipt  Dev.   PPU'
         CALL MSG_OUT(' ',TEXT,STATUS)
      END IF

*   Find the highest and lowest pixel values in the required area.
      CALL ELF1_HILOW(ELEMS,%VAL(CNF_PVAL(ARRP(1))),PRANGE,XCO,YCO,
     :                RLIM,VMIN,VMAX,STATUS)

*   Adjust the minimum value so that it is not below the background.
      IF (VMIN.LT.BACK) VMIN=BACK
      IF (BACK.EQ.0) BACK=.1

*   Set number of isophotal levels to consider.
      NUMB=NINT(20./FINE)
      IF (VMAX-VMIN.LT.NUMB) NUMB=INT(VMAX-VMIN)
      LOGINC=LOG(VMAX-VMIN)/NUMB

*   Consider different isophotal levels.
      K=-NINT(NUMB*.1)
      DO WHILE (K.LE.NUMB)

*      Increment isophotal level counter.
         K=K+1

*      Reset the initial galaxy origin indices.
         XO=XCO
         YO=YCO

*      Work out mean isophote.
         MEAN=VMIN+2.71828**(LOGINC*(NUMB-K))

*      Work out limits within which the pixel values should lie.
         BOUNDS=(VMAX-VMIN+.00001)/10.
         IF (((MEAN-VMIN).LT.(VMAX-VMIN)/10.).OR.
     :      (BOUNDS.LT.SIGMA*2.)) BOUNDS=(MEAN-VMIN)/2.
         LOW=MEAN-BOUNDS
         HIGH=MEAN+BOUNDS

*      Provide an estimate for the radius to be examined.
         CALL ELF1_LEVEL(MEAN,XO,YO,ELEMS,%VAL(CNF_PVAL(ARRP(1))),
     :                   PRANGE,SEARCH,STATUS)

*      Modify the range suggested to allow for big errors.
*      Assign default value if no better value was found.
         SEARCH=SEARCH*3.0+5.
         IF ((SEARCH.LT.0.0).OR.(SEARCH.GT.RLIM)) SEARCH=RLIM

*      Find points within required brightness range
         CALL ELF1_FIND(ELEMS,%VAL(CNF_PVAL(ARRP(1))),
     :                  PRANGE,XO,YO,SEARCH,
     :                  LOW,HIGH,COUNTR,XE,YE,PCV,STATUS)

*      Continue only if more than four were found.
         IF (COUNTR.GT.4) THEN

*         Determine the size of the circle that would fit the
*         locations of the pixels selected for use.
            CALL ELF1_CIRC(XE,YE,COUNTR,RADIUS,STATUS)

*         Calculate the position residuals and thereby crudely estimate
*         ellipticity and position angle.
            CALL ELF1_RESID(COUNTR,XO,YO,XE,YE,RADIUS,
     :                      ANGLE,ELLIP,STATUS)

*         Weed out points to required maximum number.
            CALL ELF1_SEPAR(XO,YO,LOW,HIGH,SEARCH,COUNTR,
     :                      XE,YE,PCV,STATUS)

*         Abort this isophote if no circular fit was possible.
            IF (RADIUS.LE.0.0) GOTO 9998

*         Set the first radius flag.
            FIRST=1

*         Set values for the initial ellipticity and position angle that
*         take the previous values into account
            IF (VALIDP.GT.1) THEN
               ELLIP=(ELLIP+RESULT(3,VALIDP)+RESULT(3,VALIDP-1))/3.
               RADIUS=(RADIUS+RESULT(4,VALIDP)*.5)/1.5
               THETA=(ANGLE/2.-RESULT(5,VALIDP)
     :                            -RESULT(5,VALIDP-1))/2.5
            END IF

*         Set the ranges over which position angle, radius and ellipticity
*         may be varied during minimisation.
            MINANG=THETA-50.
            MAXANG=THETA+50.
            MINRAD=RADIUS*0.7
            MAXRAD=RADIUS*0.8
            MINE=ELLIP-.15
            MAXE=ELLIP+.15

*         Set the number of different values for a given parameter to be
*         tried per loop (SLOOPS) and the total number of loops to
*         be tried.
            SLOOPS=20
            LOOPMX=200

*         Set the loop counter.
            LOOPS=0

*         Looping.
            DO WHILE (LOOPS.LE.LOOPMX)

*            Modify the ranges within which parameters may be varied (in the
*            event of the range becoming very narrow).

             IF (LOOPS.EQ.150) THEN

*              Radius limits.
                  IF ((MAXRAD-MINRAD).LT.0.05*RADIUS) THEN
                     MINRAD=RADIUS*.98
                     MAXRAD=RADIUS*1.02
                  END IF

*               Ellipticity limits.
                  IF ((MAXE-MINE).LT.0.1) THEN
                     CALL ELF1_RAND(1,0,RND,STATUS)
                     S=RND
                     S1=1.+S
                     S2=2.-S
                     MINE=ELLIP-.02*S1
                     MAXE=ELLIP+.02*S2
                  END IF

*               Position angle limits.
                  IF ((MAXANG-MINANG).LT.2.0) THEN
                     CALL ELF1_RAND(1,0,RND,STATUS)
                     S=RND
                     S1=1.+S
                     S2=2.-S
                     MINANG=THETA-1.*S1
                     MAXANG=THETA+1.*S2
                  END IF

               END IF

*            Keep the ellipticity limits within sensible limits.
               IF (MINE.LT.0.01) MINE=.01
               IF (MINE.GT.0.98) MINE=0.98
               IF (MAXE.GT.0.99) MAXE=.99
               IF (MAXE.LT.0.02) MAXE=.02

*            Try varying the orientation and see how good the fit is.
*            In this instance repeat the procedure 4 times (speeds up
*            the convergence).
               DO 30 Z=1,3

                  DO 40 J=1,SLOOPS
                     V(J)=MINANG+(J-1)*(MAXANG-MINANG)/(SLOOPS-1.)
                     CALL ELF1_SUM(COUNTR,XE,YE,XO,YO,V(J),RADIUS,
     :                        ELLIP,R(J),STATUS)
 40               CONTINUE

*               Look at the fit results and adjust the parameter range
*               and value to be used from here on.
                  CALL ELF1_BOUNDS(V,R,SLOOPS,THETA,MINANG,
     :                             MAXANG,STATUS)

 30            CONTINUE

*            Try varying the ellipticity and see how good the fit is.
               DO 50 J=1,SLOOPS
                  V(J)=MINE+(J-1)*(MAXE-MINE)/(SLOOPS-1.)
                  CALL ELF1_SUM(COUNTR,XE,YE,XO,YO,THETA,RADIUS,V(J),
     :                     R(J),STATUS)
 50            CONTINUE

*            Look at the fit results and adjust the parameter range
*            and value to be used from here on.
               CALL ELF1_BOUNDS(V,R,SLOOPS,ELLIP,MINE,MAXE,
     :                          STATUS)

*            Try varying the radius and see how good the fit is.
               DO 60 J=1,SLOOPS
                  V(J)=MINRAD+(J-1)*(MAXRAD-MINRAD)/(SLOOPS-1.)
                  CALL ELF1_SUM(COUNTR,XE,YE,XO,YO,THETA,V(J),ELLIP,
     :                     R(J),STATUS)
 60            CONTINUE

*            Look at the fit results and adjust the parameter range
*            and value to be used from here on.
               CALL ELF1_BOUNDS(V,R,SLOOPS,RADIUS,MINRAD,
     :                          MAXRAD,STATUS)

*            Only adjust the position when a good estimate of the
*            other parameters has been attained.
*            Also. Only adjust the origin if it has not been frozen
*            via parameter FRZORI.
               IF ((LOOPS.GT.100).AND.(.NOT.FRZORI)) THEN

*               Set up the maximum amount by which the position may change
*               and also the direction in which it should move away from the
*               current origin.
                  MAXY=RADIUS*.005
                  TA=THETA*ELF__PI2360

*               Try varying the orientation and see how good the fit is.
                  DO 70 J=1,SLOOPS
                     DIST=MAXY*(J-SLOOPS/2.)/REAL(SLOOPS)
                     X=XO+DIST*SIN(TA)
                     Y=YO+DIST*COS(TA)
                     CALL ELF1_SUM(COUNTR,XE,YE,X,Y,THETA,RADIUS,ELLIP,
     :                        SUMS,STATUS)
                     V(J)=DIST
                     R(J)=SUMS
 70               CONTINUE

*               Consider the fit results and assign a new origin
*               position accordingly.
                  J=0
                  MIN=ELF__VBIG
                  DO 80 I=1,SLOOPS
                     IF (R(I).LT.MIN) THEN
                        J=I
                        MIN=R(J)
                     END IF
 80               CONTINUE

*               Modify the current X/Y location of the galaxy centre.
                  IF (J.NE.0) THEN
                     XO=XO+V(J)*SIN(TA)
                     YO=YO+V(J)*COS(TA)
                  END IF
               END IF

               LOOPS=LOOPS+1

            END DO

            FIRST=0

*         Store the results for the current isophote.

*         Calculate positions for the points on an ellipse of the current
*         fit parameters.
            CALL ELF1_GENER(RADIUS,ELLIP,POINTS,ANG,RAD,STATUS)
            CALL ELF1_ROTAT(XO,YO,THETA,POINTS,ANG,RAD,XCR,YCR,STATUS)

*         Determine the mean isophotal value of the pixels on the
*         generated ellipse. Determine also its standard deviation
            CALL ELF1_STATS(ELEMS,ARRP,XCR,YCR,POINTS,
     :                      PRANGE,USED,MEAN,SDP,RESDU,
     :                      VA,FOUND,STATUS)

*         Modify theta so it is not in the range -90 to +90 degrees.
            IF (THETA.LT.-90) THETA=180.+THETA
            IF (THETA.GT.90.) THETA=THETA-180.

*         Profile mean brightness too low or radius is too big.
            IF ((MEAN-BACK.LE.0.0).OR.(MEAN-BACK.LT.LIM2*SIGMA).OR.
     :          (RADIUS.GT.RLIM).OR.(FOUND.LT.FRACT*POINTS/100.)
     :          .OR.(RADIUS.LT.4.0)) THEN

*            Don't bother to keep the current result.
            ELSE

*            Increment valid fit counter.
               VALIDP=VALIDP+1

*            Generate the Fourier descriptor.
               CALL ELF1_FOUR(POINTS,BACK,VALIDP,VA,XCR,YCR,XO,YO,
     :                        THETA,ELLIP,RESULT,STATUS)

*            Assign results values.
               RESULT(1,VALIDP)=XO
               RESULT(2,VALIDP)=YO
               RESULT(3,VALIDP)=ELLIP
               RESULT(4,VALIDP)=RADIUS
               RESULT(5,VALIDP)=-THETA
               RESULT(6,VALIDP)=MEAN-BACK
               RESULT(7,VALIDP)=SDP
               RESULT(8,VALIDP)=REAL(COUNTR)
               RESULT(9,VALIDP)=100.*REAL(FOUND)/REAL(POINTS)

*            Display the results in suitably formatted form (if required).
               IF (DMODE.GT.0) THEN

                  CALL MSG_FMTR('X','F6.1',XO)
                  CALL MSG_FMTR('Y','F6.1',YO)
                  CALL MSG_FMTI('N','I3',COUNTR)
                  TEMP=PSIZE*SQRT(RADIUS*RADIUS*ELLIP)
                  CALL MSG_FMTR('RAD','F8.2',TEMP)
                  CALL MSG_FMTR('VAL','F9.1',MEAN-BACK)
                  IF (ANGCON) THEN
                     TEMP=-THETA+ANGOFF
                  ELSE
                     TEMP=THETA+ANGOFF
                  END IF
                  CALL MSG_FMTR('POS','F5.1',TEMP)
                  CALL MSG_FMTR('ELL','F5.3',ELLIP)
                  CALL MSG_FMTR('DEV','F7.1',SDP)
                  CALL MSG_FMTR('POI','F4.0',RESULT(9,VALIDP))
                  TEXT='^X  ^Y    ^N   ^RAD  ^VAL  ^POS    ^ELL'//
     :                 ' ^DEV  ^POI'
                  CALL MSG_OUT(' ',TEXT,STATUS)

               END IF

            END IF

         END IF

*      Non-fatal error at the current iso-phote.
 9998    CONTINUE

      END DO

*   Sort the results into increasing radius (a).
      IF (VALIDP.GT.1) THEN

*      Look through all the results.
         DO 200 I=1,VALIDP-1

*         Set up the initial lowest radius value.
            MIN=RESULT(4,I)
            K=I

*         Look at all remaining radii to see which is smallest and
*         retain its index when it is found.
            DO 300 J=I+1,VALIDP
               IF (RESULT(4,J).LT.MIN) THEN
                  K=J
                  MIN=RESULT(4,J)
               END IF
 300        CONTINUE

*         Swap the ith and kth components if necessary.
            DO 400 J=1,10

*            Swap a pair.
               VALUE=RESULT(J,I)
               RESULT(J,I)=RESULT(J,K)
               RESULT(J,K)=VALUE

 400        CONTINUE

 200     CONTINUE

      END IF

      IF (DMODE.GT.0) CALL MSG_BLANK(STATUS)

 9999 CONTINUE

      END



      SUBROUTINE ELF1_RESID(COUNTR,XO,YO,XE,YE,RADIUS,
     :                      ANGLE,ELLIP,STATUS)
*+
*  Name:
*     ELF1_RESID

*  Purpose:
*     Provides an estimate for the ellipticity of the isophotal
*     pixel distribution and also the position angle.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_RESID(COUNTR,XO,YO,XE,YE,RADIUS,
*                     ANGLE,ELLIP,STATUS)

*  Description:
*     Looks through the list of isophotal pixels and determines
*     how far they lie from the proposed centre of the galaxy. Uses this
*     to determine the possible ellipticity. Then creates an angular
*     histogram and determines which angular sector is most occupied.
*     This allows the ellipticity to be estimated.

*  Arguments:
*     COUNTR = INTEGER (Given)
*        Number of isophotal pixels.
*     XO = REAL (Given)
*        X co-ordinate of the galaxy centre. Units pixels.
*     YO = REAL (Given)
*        Y co-ordinate of the galaxy centre. Units pixels.
*     XE(ELF__PIXEL) = REAL (Given)
*        X co-ordinates of the pixels. Units pixels.
*     YE(ELF__PIXEL) = REAL (Given)
*        Y co-ordinates of the pixels. Units pixels.
*     RADIUS = REAL (Given)
*        Estimated circle radius. Only accurate if ellipticity is 1.
*        Units pixels.
*     ANGLE = REAL (Returned)
*        Position angle of the isophotal pixels. Units degrees.
*     ELLIP = INTEGER (Returned)
*        Ellipticity of the ellipse.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     24-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER COUNTR                  ! Number of isophotal pixels
      REAL RADIUS                     ! Ellipse radius in use
      REAL XE(ELF__PIXEL)             ! X co-ords of pixels
      REAL YE(ELF__PIXEL)             ! Y co-ords of pixels
      REAL XO                         ! X co-ord of ellipse centre
      REAL YO                         ! Y co-ord of ellipse centre

*  Arguments Returned:
      REAL ANGLE                      ! Position angle about the origin
      REAL ELLIP                      ! Ellipticity of the ellipse

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER INDEX                   ! Angular bin index for the current pixel
      INTEGER SECTOR(ELF__PIXEL)      ! Angular bin array
      REAL MAX                        ! Number of pixels in the most highly
                                      ! occupied angular bin
      REAL R                          ! Residual deviation from a circle
      REAL RMS                        ! Root mean squared deviation
                                      ! from a circle
      REAL SUM                        ! Sum of squared deviations from a
                                      ! circle
      REAL XD                         ! X displacement
      REAL YD                         ! Y displacement

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Clear the angular distribution histogram counts.
      DO 5 I=1,COUNTR
         SECTOR(I)=0
 5    CONTINUE

*   Clear the deviation summation.
      SUM=0.0

*   Determine how the pixels are located in terms of angle
*   about the origin.
      DO 10 I=1,COUNTR

*      Calculate the angle for the current point.
         CALL ELF1_ANGLES(XE(I),YE(I),XO,YO,ANGLE,STATUS)

*      Add it to the necessary element of the angular histogram.
         INDEX=INT(ANGLE/36.+1)
         SECTOR(INDEX)=SECTOR(INDEX)+1

*      Determine the displacement from the galaxy centre.
         XD=XE(I)-XO
         YD=YE(I)-YO

*      Compare to the radius.
         R=SQRT(XD*XD+YD*YD)-RADIUS

*      Add to the summation.
         SUM=SUM+R*R

 10   CONTINUE

*   Find the RMS deviation from a circle.
      RMS=SQRT(SUM/REAL(COUNTR))

*   Assign a ellipticity value.
      ELLIP=(RADIUS-RMS)/RADIUS

*   Find the member of the angular distribution histogram
*   that is most highly occupied.
      MAX=0
      J=0
      DO 20 I=1,5

*      Retain the index of the most highly occupied histogram element.
         IF (SECTOR(I)+SECTOR(I+5).GT.MAX) THEN
            J=I
            MAX=SECTOR(I)+SECTOR(I+5)
         END IF

 20   CONTINUE

*   Calculate an approximate position angle.
      ANGLE=(J-1+.5)*36.
      IF (ANGLE.GT.90.) ANGLE=180.-ANGLE

 9999 CONTINUE

      END


      SUBROUTINE ELF1_SEPAR(XO,YO,LOW,HIGH,RADIUS,COUNTR,
     :                      XE,YE,PCV,STATUS)
*+
*  Name:
*     ELF1_SEPAR

*  Purpose:
*     Looks through the list of pixels found to be within the
*     desired isophote and wittles them down to a number that
*     shouldnt execute slowly on current machines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_SEPAR(XO,YO,LOW,HIGH,RADIUS,COUNTR,
*                     XE,YE,PCV,STATUS)

*  Description:
*     Looks throught the list of isophotal pixels and determines
*     how they are distributed around the origin by assigning them
*     to angular bins. Then identifies the most occupied bin(s)
*     and searches that for the pixel with the largest deviation from
*     the isophote required. This is then removed from the list of
*     the isophotal pixels. The procedure repeats until the required
*     number of pixels is attained.

*  Arguments:
*     XO = REAL (Given)
*        X co-ordinate of the galaxy centre. Units pixels.
*     YO = REAL (Given)
*        Y co-ordinate of the galaxy centre. Units pixels.
*     LOW = REAL (Given)
*        Low brightness count value for the pixels.
*     HIGH = INTEGER (Given)
*        High brightness count value for the pixels.
*     RADIUS = REAL (Given)
*        Approximate ellipse radius.
*     COUNTR = INTEGER (Given and Returned)
*        Number of pixels in use.
*     XE(ELF__PIXEL) = REAL (Given and Returned)
*        X co-ordinates of the pixels. Units pixels.
*     YE(ELF__PIXEL) = REAL (Given and Returned)
*        Y co-ordinates of the pixels. Units pixels.
*     PCV(ELF__PIXEL) = REAL (Given and Returned)
*        Count value for each of the isophotal pixels. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     02-SEP-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      REAL XO                         ! Galaxy centre
      REAL YO                         ! Galaxy centre
      REAL HIGH                       ! Upper pixel count limit
      REAL LOW                        ! Lower pixel count limit
      REAL RADIUS                     ! Approximate ellipse radius

*  Arguments Returned:

*  Arguments Given and Returned:
      INTEGER COUNTR                  ! Number of isophotal pixels
      REAL PCV(ELF__PIXEL)            ! Brightness of the pixels
      REAL XE(ELF__PIXEL)             ! X co-ord of the pixels
      REAL YE(ELF__PIXEL)             ! Y co-ord of the pixels

*  Local variables:
      INTEGER ANGI(ELF__PIXEL)        ! Indices of the angular bin each
                                      ! of the pixels are in
      INTEGER BIGGEST(40)             ! Indices of the angular bins that
                                      ! contain the largest number of pixels
      INTEGER COUNT                   ! Number of pixel still retained
      INTEGER I                       ! Loop variable
      INTEGER INDEX                   ! Index of the angular bin to which
                                      ! the current pixel should be assigned
      INTEGER INANG(40)               ! Angular bin
      INTEGER J                       ! Loop variable
      INTEGER K                       ! loop variable
      INTEGER LIMIT                   ! Maximum number of pixels to be
                                      ! retained
      INTEGER NUMBER                  ! Number of angular bins containing
                                      ! MAX pixels
      INTEGER OKAY                    ! Flag
      REAL ANG(ELF__PIXEL)            ! Angular position of each pixel
      REAL DEGS                       ! Pixel angle relative to origin
      REAL DEV(ELF__PIXEL)            ! Deviation of the pixel value from the
                                      ! desired isophote
      REAL MAX                        ! Number of pixels in the most occupied
                                      ! angular bin
      REAL MAXDEV                     ! Maximum deviation from the
                                      ! isophotal level
      REAL MEAN                       ! Mean value of the isophotal
                                      ! pixel range allowed
      REAL PERIM                      ! Circumfrence of a circle of the
                                      ! current radius

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the mean value of the pixel count range.
      MEAN=(HIGH+LOW)/2.

*   Calculate an approximate ellipse perimeter.
      PERIM=2.*ELF__PIVAL*RADIUS

*   Determine limit of number of pixels to be retained.
      IF (PERIM.GT.ELF__MXPOI) THEN
         LIMIT=ELF__MXPOI
      ELSE
         LIMIT=PERIM
      END IF

*   Angular histogram bins cleared.
      DO 10 I=1,40
         INANG(I)=0
         BIGGEST(I)=0
 10   CONTINUE

*   Determine the angular bin for each pixel.
*   Add values to the histogram.
      INDEX=0
      DO 20 I=1,COUNTR

*      Find the angle of the current pixel relative to the origin.
         CALL ELF1_ANGLES(XE(I),YE(I),XO,YO,DEGS,STATUS)

*      Calculate the index of the histogram bin it belongs in.
         INDEX=INT(DEGS/9.)+1

*      Increment the bin element and COUNTR.
         INANG(INDEX)=INANG(INDEX)+1
         ANGI(I)=INDEX

*      Store the angle and also the amount by which it differ from
*      the prefered isophote.
         ANG(I)=DEGS
         DEV(I)=ABS(PCV(I)-MEAN)

 20   CONTINUE

      COUNT=COUNTR

*   Loop round removing pixels until the number is below that required.
      DO WHILE (COUNT.GT.LIMIT)

*      Find most highly occupied angular bin.
         MAX=0
         DO 30 I=1,40

*         Compare with previous highest occupancy.
            IF (INANG(I).GT.MAX) MAX=INANG(I)

 30      CONTINUE

*      Find out how many times it occurs.
         NUMBER=0
         DO 40 I=1,40

*         Compare with the required value and count up number of times.
*         Also, keep one occurence.
            IF (INANG(I).EQ.MAX) THEN
               NUMBER=NUMBER+1
               BIGGEST(NUMBER)=I
            END IF

 40      CONTINUE

*      Search for the pixels that are in the most occupied angular bins.
*      From those identify the pixel with the largest brightness deviation
*      from the required value.
*      MAXDEVV must start as -1 or the algorithm will not work when
*      all are the of the correct brightness.
         MAXDEV=-1.
         J=0
         DO 50 I=1,COUNTR

*         Is the current pixel still allowed?
            IF (PCV(I).GT.ELF__VSMAL) THEN

*            Is current pixel in one of the most occupied angular bins?
               OKAY=0
               DO 55 K=1,NUMBER
                  IF (ANGI(I).EQ.BIGGEST(K)) OKAY=1
 55            CONTINUE

*            Act if is in one of the most occupied bins.
               IF (OKAY.GT.0) THEN

*               Is the pixel brightness further from the mean
*               required value than all the pixels so far considered.
                  IF (DEV(I).GT.MAXDEV) THEN

*                  Retain the details of the pixel.
                     MAXDEV=DEV(I)
                     INDEX=ANGI(I)
                     J=I

                  END IF

               END IF

            END IF

 50      CONTINUE

*      Remove the pixel with the greatest deviation from the array.
         PCV(J)=-1.0
         INANG(INDEX)=INANG(INDEX)-1
         COUNT=COUNT-1

      END DO

*   Close up the gaps in the arrays left by the discarded pixels.
      J=0
      DO 60 I=1,COUNTR

*      Is the current data point to be used?
         IF (PCV(I).GT.0.5) THEN

*         Increment counter and store values.
            J=J+1
            XE(J)=XE(I)
            YE(J)=YE(I)

         END IF

 60   CONTINUE
      COUNTR=J

 9999 CONTINUE

      END



      SUBROUTINE ELF1_SUM(COUNTR,XE,YE,XO,YO,POSANG,RADIUS,ELLIP,
     :                    SUMS,STATUS)
*+
*  Name:
*     ELF1_SUM

*  Purpose:
*     Determine some measure (SUMS) of how closely the current ellipse
*     parameters compare with the data pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_SUM(COUNTR,XE,YE,XO,YO,POSANG,RADIUS,ELLIP,
*                    SUMS,STATUS)

*  Description:
*     Converts the co-ordinates of the current pixels into those
*     of an equivalent unit radius ellipse using the current fit
*     ellipse parameters to normalise. The output SUM is the absolute sum
*     of the deviations found for all the pixels from the fit ellipse.

*  Arguments:
*     COUNTR = INTEGER (Given)
*        Number of pixels currently being considered.
*     XE(ELF__PIXEL) = REAL (Given)
*        X co-ordinates of the pixels. Units pixels.
*     YE(ELF__PIXEL) = REAL (Given)
*        Y co-ordinates of the pixels. Units pixels.
*     XO = REAL (Given)
*        Fit ellipse centre X co-ordinate. Units pixels.
*     YO = REAL (Given)
*        Fit ellipse centre Y co-ordinate. Units pixels.
*     POSANG = REAL (Given)
*        Position angle of the current ellipse fit. Units degrees.
*     RADIUS = REAL (Given)
*        Radius of the fit ellipse semi-major axis. Units pixels.
*     ELLIP = REAL (Given)
*        Ellipticity of the fit ellipse.
*     SUMS = REAL (Returned)
*        Ellipse fit residuals i.e. the deviation of the pixels from the
*        current ellipse fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     9-JUL-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'FIO_ERR'               ! FIO error definitions
      INCLUDE 'ELF_PAR'               ! ELLFOU constants

*  Arguments Given:
      INTEGER COUNTR                  ! Number of pixels
      REAL ELLIP                      ! Ellipse ellipticity
      REAL POSANG                     ! Ellipse position angle
      REAL RADIUS                     ! Ellipse semi-major radius
      REAL XE(ELF__PIXEL)             ! Pixel positions (x)
      REAL XO                         ! Galaxy centre on image (x)
      REAL YE(ELF__PIXEL)             ! Pixel positions (y)
      REAL YO                         ! Galaxy centre on image (y)

*  Arguments returned:
      REAL SUMS

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL ASQ                        ! Semi-minor axis radius squared
      REAL BSQ                        ! Semi-major axis radius squared
      REAL COSV                       ! Cosine of the position angle
      REAL SINV                       ! Sine of the position angel
      REAL THETA                      ! The position angle
      REAL V                          ! Temporary value
      REAL V1                         ! Temporary value
      REAL V2                         ! Temporary value
      REAL XD                         ! Pixel X displacement from ellipse
                                      ! origin
      REAL YD                         ! Pixel Y displacement from ellipse
                                      ! origin
*.


*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Convert the position angle into radians.
      THETA=POSANG*ELF__PI2360

*   Set up cosine and sine factors.
      COSV=COS(THETA)
      SINV=SIN(THETA)

*   Calculate the radii normalisation factors.
      BSQ=RADIUS*RADIUS
      ASQ=ELLIP*ELLIP*BSQ

*   Look at the deviation of each point and add the result to
*   the summation.
      SUMS=0.0
      DO 10 I=1,COUNTR

*      Calculate distances from the centre in both x and y directions.
         XD=XE(I)-XO
         YD=YE(I)-YO

*      Calculate the deviation from the expected form.
         V1=XD*COSV+YD*SINV
         V2=YD*COSV-XD*SINV
         V=V1*V1/ASQ+V2*V2/BSQ-1.

*      Add result to the summation.
         SUMS=SUMS+V*V

 10   CONTINUE

 9999 CONTINUE

      END
