      SUBROUTINE GAUFIT(STATUS)
*+
*  Name:
*     GAUFIT

*  Purpose:
*     Performs a 2-D Gaussian fit of multiple sources.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAUFIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Uses a minimisation routine to determine the 2-D Gaussian profiles
*     of multiple sources on an NDF format image. This will be especially
*     useful for those using JCMT data (see also \xref{JCMTDR}{sun132}{}).
*
*     Source locations can be specified using a cursor or by
*     text file. The user is allowed to restrain the extent to which
*     each minimisation iteration is allowed to modify the location,
*     breadth or position angle of the sources. This is
*     essential when the package is used with overlapping sources.
*
*     Input text files must contain the x and y coordinates of the
*     source in the Current co-ordinates of the NDF, and may in
*     addition contain estimates for the position angle, Sa, Sb
*     (std deviation of the Gaussian functions in 2 directions -
*     major axis then minor) and the peak value.
*
*     Output image options are for the generation of the complete whole
*     image model or an image containing the residuals in the regions
*     surrounding the sources.
*
*     There are two separate fitting algorithms within GAUFIT, a
*     non-linear least-squares routine (selectable with parameter
*     LSQFIT=true) or the original parameter-search routine.  Some of
*     the parameters below have slightly different behaviour in the two
*     modes, or are only available in one mode or the other.  These are
*     indicated by prefacing the variant descriptions with either [PS:]
*     for parameter-search mode (lsqfit=false) or [LSQ:] for
*     least-squares (lsqfit=true)
*
*
*  Usage:
*     GAUFIT IN INFILE OUT MODE MODEL IMGDEV
*            MODTYPE COLOUR ANGCON ANGOFF FWHM PSIZE
*            BACK SIGMA NSIGMA
*            LSQFIT=false AUTOL XINC YINC SAINC SBINC PINC ANGINC NITER
*
*     GAUFIT IN INFILE OUT MODE MODEL IMGDEV
*            MODTYPE COLOUR ANGCON ANGOFF FWHM PSIZE
*            BACK SIGMA NSIGMA
*            LSQFIT=true CALCSD MAXITER

*  ADAM Parameters:
*     ANGCON = _LOGICAL (Read)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.
*     ANGINC = _REAL (Read)
*        [PS:] The amount by which the angle of a source may vary.
*        Arbitrary range 0 to 1. 1 = free to move as required.
*        0 = unable to move.
*     ANGOFF = _REAL (Read)
*        Angular offset for position angles generated. Units degrees.
*     AUTOL = _LOGICAL (Read)
*        [PS:] Is the source origin provided to be refined?
*     BACK = _REAL (Read)
*        The background value for the image.  [LSQ:] You may give this
*        as a negative number, to have the routine obtain and report the
*        best-fit background; in this case, the SIGMA and NSIGMA
*        parameters are ignored.
*     CALCSD = _LOGICAL (Read)
*        [LSQ:] Should we calculate and display parameter uncertainties?
*        A significant part of the calculation is taken up with this
*        calculation, so if you do not want the uncertainties, you will save
*        time by opting not to calculate them.
*     COLOUR = _INTEGER (Read)
*        Colour of the pen used to mark source centres.
*     FWHM = _LOGICAL (Read)
*        Are the gaussian widths to be read and written as FWHM or
*        standard deviations?
*     IMGDEV = _DEVICE (Read)
*        Name of the graphics device on which the results graph should
*        be displayed.
*     INFILE = _CHAR (Read)
*        Name of a text file containing the co-ordinates of sources
*        to be profiled.  Co-ordinates are in the Current co-ordinate
*        system of the WCS component of IN.
*     IN = _NDF (Read)
*        The name of the source NDF data structure/file.
*     LSQFIT = _LOGICAL (Read)
*        Is the application to use the least-squares fitting
*        routine, or the older parameter-search method?
*     MAXITER = _INTEGER (Read)
*        [LSQ:] Upper-bound on the iteration count within the
*        least-squares method
*        (-1 indicates that you are happy
*        with the default limit).  The default maximum count is large, and
*        intended as an upper bound on the iteration count, to stop it spinning
*        its wheels uselessly on some pathological dataset.  You should not
*        need to change this unless you suspect that the limit is genuinely
*        being reached by a correct calculation.
*     MODE = _LOGICAL (Read)
*        Whether the application is to run in file input mode or
*        interactively. Interactive MODE=TRUE. File mode=FALSE.
*     MODEL = _NDF (Read)
*        The output NDF.
*     MODTYP=_CHAR (Read)
*        The type of output NDF file to be created. MODTYP=R gives
*        residuals near the sources. MODTYP=W gives the whole
*        image model.
*     NITER = _INTEGER (Read)
*        [PS:] The number of iterations performed by the parameter-search
*        routine.
*     NSIGMA = _REAL (Read)
*        Number of sigma above sky at which pixels are considered
*        to be significant. [LSQ:] If you give back=-1, then this is ignored.
*     OUT = _CHAR (Read)
*        File name for the output text file containing the profile
*        data.
*     PINC = _REAL (Read)
*        [PS:] The amount by which the peak of a source may vary.
*        1 = free to move as required. 0 = unable to move.
*     PSIZE = _REAL (Read)
*        The size of each pixel in arc seconds.  If the image contains
*        a SKY co-ordinate frame this value will be determined
*        automatically.
*     SAINC = _REAL (Read)
*        [PS:] The amount by which the standard deviation of a source may vary
*        per iteration. Largest axis. 1 = free to move as required.
*        0 = unable to move.
*     SBINC = _REAL (Read)
*        [PS:] The amount by which the standard deviation of a source may vary
*        per iteration. Smallest axis.
*     SIGMA = _REAL (Read)
*        Standard deviation of the sky count. [LSQ:] If you give
*        back=-1, then this is ignored.
*     XINC = _REAL (Read)
*        [PS:] The amount by which the x coordinate of a source may vary
*        per iteration. 1 = free to move as required.
*        0 = unable to move.
*     YINC = _REAL (Read)
*        [PS:] The amount by which the x coordinate of a source may vary
*        per iteration. 1 = free to move as required.
*        0 = unable to move.
*
*  Examples:
*     gaufit mode=false infile=coords.dat in=image out=sources
*            modtyp=w model=imodel
*
*        Will read source coordinates from the text file coords.dat.
*        The image on which these appear is image, the output image
*        containing the model for each pixel will be imodel.
*        The coordinates provided by the file are in the Current
*        coordinate system of the WCS component of the NDF image.
*
*     gaufit mode=true out=test1 modtyp=r angoff=90
*
*        The sources will be identified by cursor. The output
*        image test1 will only show the residual (discrepancy
*        between the models and the source image in the vicinity
*        of the sources. The resultant position angles will be
*        modified by 90 degrees.
*
*     gaufit mode=true lsqfit=true back=-1 out=test1 angoff=90
*
*        The sources will be identified by cursor. The resultant
*        position angles will be modified by 90 degrees.  The source
*        positions will be identified using a least-squares fitting
*        technique, which will also fit the background.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG:  Norman Gray (Starlink, GLA)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}
*
*  History:
*     01-AUG-1996 (GJP)
*       (Original version)
*     10-JUN-1998 (NG)
*       Merged gau2_pro from gaufit2.f - alternative fitting routine
*     8-NOV-1999 (MBT)
*       Modified to work with World Coordinate Systems.
*
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL MODE                    ! Interactive or file mode

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Enter AST context.
      CALL AST_BEGIN(STATUS)

*   Show that the application is running.
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT(' ','ESP GAUFIT running.',STATUS)
      CALL MSG_BLANK(STATUS)

*   Get the user selection of working interactively or by file?
      CALL PAR_GET0L('MODE',MODE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Transfer control to a fully interactive mode or to file input
*   handling routine.
      IF (MODE) THEN

*      Cursor input for the coordinates.
         CALL GAU1_CMODE(STATUS)

      ELSE

*      File input for the coordinate values.
         CALL GAU1_FMODE(STATUS)

      END IF

*   Abort the program.
 9999 CONTINUE

*   Exit AST context.
      CALL AST_END(STATUS)

      END



      SUBROUTINE GAU1_OUTIM(ELEMS,ARRAY1,ARRAY2,STATUS)
*+
*  Name:
*     GAU1_OUTIM

*  Purpose:
*     Transfers the working image (containing residuals) to the
*     output image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_OUTIM(ELEMS,ARRAY1,ARRAY2,STATUS)

*  Description:
*      Copies values from the array containing the residuals.
*      The residuals array is the actual image minus the
*      model image.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY1(ELEMS) = REAL (Given)
*        The output image.
*     ARRAY2(ELEMS) = REAL (Returned)
*        The residuals image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels
      REAL ARRAY1(ELEMS)              ! Residuals image
      REAL ARRAY2(ELEMS)              ! Output image

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! A loop variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Transfer the image.
      DO 10 I=1,ELEMS
        ARRAY2(I)=ARRAY1(I)
 10   CONTINUE

      END


      SUBROUTINE GAU1_PRO(NSOUR,MODTYP,XINC,YINC,
     :                    SAINC,SBINC,ANGINC,PINC,
     :                    ANGCON,ANGOFF,PSIZE,
     :                    NITER,RLIM,BACK,SIGMA,
     :                    ELEMS,UPIX,POINT,PRANGE,GUESS,
     :                    STATUS)
*+
*  Name:
*     GAU1_PRO

*  Purpose:
*     Routine wherein the Gaussian parameters are determined.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_PRO(NSOUR,XINC,YINC,SAINC,SBINC,ANGINC,PINC,
*                   ANGCON,ANGOFF,PSIZE,NITER,
*                   RLIM,BACK,SIGMA,
*                   ELEMS,UPIX,POINT,PRANGE,GUESS
*                   STATUS)

*  Description:
*     The routine minimises the residuals of the model/source fits
*     by creating the models for a variety of parameter values and
*     performing a walk through parameter space in the direction that
*     minimises the residual. Not ideal but it works reasonably well
*     given the time restraints.

*  Arguments:
*     NSOUR = INTEGER (Given)
*        Number of sources.
*     MODTYP = INTEGER (Given)
*        Type of output image.
*     XINC = REAL (Given)
*        Size of movement permitted in X direction.
*     YINC = REAL (Given)
*        Size of movement permitted in Y direction.
*     SAINC = REAL (Given)
*        Size of variation allowed in Sa.
*     SBINC = REAL (Given)
*        Size of variation allowed in Sb.
*     ANGINC = REAL (Given)
*        Size of variation allowed in angle.
*     PINC = REAL (Given)
*        Size of variation allowed in peak value.
*     ANGCON = LOGICAL (Given)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.
*     ANGOFF = REAL (Given)
*        Angular offset for position angles generated. Units degrees.
*     PSIZE = REAL (Given)
*        Pixel size for display.  Units arcsec
*     NITER = INTEGER (Given)
*        Number of iterations.
*     RLIM(10) = REAL (Given)
*        The maximum distance from the origin at which profiling
*        takes place.
*     BACK = REAL (Given)
*        The background count for the image.
*     SIGMA = REAL (Given)
*        Standard deviation value of BACK.
*     ELEMS = INTEGER (Given)
*        Number of elements/pixels in the image array. Units pixels.
*     UPIX = INTEGER (Given)
*        Number of non-masked pixels
*     POINT(6) = INTEGER (Given)
*        Pointers to the image arrays.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     GUESS(10,7) = REAL (Given and Returned)
*        Current parameter estimates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     20-Mar-1996 (GJP)
*     (Original version)
*     20-FEB-1997 (GJP)
*     Corrected size of POINT array.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data
      include 'GAU_PAR'
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      LOGICAL ANGCON                  ! Angle rotation convention
      INTEGER MODTYP           ! Type of output image
      INTEGER NITER                   ! Number of iterations
      INTEGER NSOUR                   ! Number of sources
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER POINT(6)                ! Pointers to images
      INTEGER PRANGE(2)               ! Image size
      INTEGER UPIX                    ! Number of unmasked pixels
      REAL ANGOFF                     ! Angular offset
      REAL PSIZE		      ! Pixel size/arcsec
      REAL ANGINC                     ! Size of movement in angle
      REAL BACK                       ! Background count value
      REAL PINC                       ! Size of movement in peak value
      REAL RLIM(10)                   ! Maximum source radius used
      REAL SAINC                      ! Variation in Sa allowed
      REAL SBINC                      ! Variation in Sb allowed
      REAL SIGMA                      ! Std deviation of BACK
      REAL XINC                       ! Size of movement in X
      REAL YINC                       ! Size of movement in Y

*  Arguments Given and Returned:
      REAL GUESS(10,7)                ! Current estimates of the parameters

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Loop variable
      INTEGER L2                      ! Loop variable
      REAL KEEP                       ! Temporary storage
      REAL PASS(10,7)                 ! Current parameters value
      REAL RESID                      ! Normalised residuals
      REAL STEP1                      ! Parameter increment
      REAL BEST                       ! Minimum residual so far
      REAL TEMP1                      ! Temporary value

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the loops required.

*   For each source set up the parameters for each Gaussian.
      DO 50 I=1,NSOUR
         DO 60 J=1,7

*         Set up the parameters established.
            PASS(I,J)=GUESS(I,J)

 60      CONTINUE
 50   CONTINUE

*   Build the residuals image.
      CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                %VAL(CNF_PVAL(POINT(2))),
     :                %VAL(CNF_PVAL(POINT(3))),
     :                %VAL(CNF_PVAL(POINT(4))),
     :                %VAL(CNF_PVAL(POINT(5))),
     :                %VAL(CNF_PVAL(POINT(6))),
     :                RESID,STATUS)

*   Tell the user what the initial residual was.
      CALL MSG_BLANK(STATUS)
      CALL MSG_FMTR('DEV','F7.2',RESID)
      CALL MSG_OUT(' ','Initial arbitrary residual ^DEV',STATUS)
      CALL MSG_BLANK(STATUS)

*   Do the number of iterations requested.
      DO 2000 L2=1,NITER

*      Tell the users the number of iteration underway.
         CALL MSG_BLANK(STATUS)
         CALL MSG_FMTI('I','I2',L2)
         CALL MSG_FMTI('NI','I2',NITER)
         CALL MSG_OUT(' ','Iteration ^I  of ^NI',STATUS)

*      For each source.
         DO 170 I=1,NSOUR

*         Do angle and sigmas more than position and peak value.
            DO 1000 L=1,5

*            Vary the angle.
               BEST=1e20
               STEP1=1.*ANGINC
               TEMP1=PASS(I,7)
               PASS(I,7)=PASS(I,7)-4.*STEP1
               DO 80 J=-4,4

*               Build the residuals image.
                  CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,
     :                            ELEMS,UPIX,
     :                            %VAL(CNF_PVAL(POINT(2))),
     :                            %VAL(CNF_PVAL(POINT(3))),
     :                            %VAL(CNF_PVAL(POINT(4))),
     :                            %VAL(CNF_PVAL(POINT(5))),
     :                            %VAL(CNF_PVAL(POINT(6))),
     :                            RESID,STATUS)

*               Test for better residuals.
                  IF(RESID.LT.BEST)THEN
                     KEEP=PASS(I,7)
                     BEST=RESID
                  END IF

*               Increment the angle.
                  PASS(I,7)=PASS(I,7)+STEP1

 80            CONTINUE

*            Set the angle to the best value.
               PASS(I,7)=KEEP*.3333+TEMP1*.6667

*            Vary sigma x.
               BEST=1E20
               STEP1=PASS(I,5)*.05*SAINC
               TEMP1=PASS(I,5)
               PASS(I,5)=PASS(I,5)-4.*STEP1
               DO 210 K=-4,4

*               Build the main image.
                  CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,
     :                            UPIX,
     :                            %VAL(CNF_PVAL(POINT(2))),
     :                            %VAL(CNF_PVAL(POINT(3))),
     :                            %VAL(CNF_PVAL(POINT(4))),
     :                            %VAL(CNF_PVAL(POINT(5))),
     :                            %VAL(CNF_PVAL(POINT(6))),
     :                            RESID,STATUS)

*               Test for better residuals.
                  IF(RESID.LT.BEST)THEN
                     KEEP=PASS(I,5)
                     BEST=RESID
                  END IF

*               Increment sigma x.
                  PASS(I,5)=PASS(I,5)+STEP1

 210           CONTINUE

*            Set sigma x to best value.
               PASS(I,5)=KEEP*.3333+TEMP1*.6667

*            Vary sigma y.
               BEST=1E20
               STEP1=PASS(I,6)*.05*SBINC
               TEMP1=PASS(I,6)
               PASS(I,6)=PASS(I,6)-4.*STEP1
               DO 220 K=-4,4

*               Build the main image.
                  CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,
     :                            ELEMS,UPIX,
     :                            %VAL(CNF_PVAL(POINT(2))),
     :                            %VAL(CNF_PVAL(POINT(3))),
     :                            %VAL(CNF_PVAL(POINT(4))),
     :                            %VAL(CNF_PVAL(POINT(5))),
     :                            %VAL(CNF_PVAL(POINT(6))),
     :                            RESID,STATUS)

*               Test for better residuals.
                  IF(RESID.LT.BEST)THEN
                     KEEP=PASS(I,6)
                     BEST=RESID
                  END IF

*               Increment sigma y.
                  PASS(I,6)=PASS(I,6)+STEP1

 220           CONTINUE

*            Set sigma y to the best value.
               PASS(I,6)=KEEP*.333+TEMP1*.667

 1000       CONTINUE

*         Vary peak value.
            BEST=1E20
            STEP1=PASS(I,4)*.05*PINC
            IF (STEP1.GT.SIGMA/3.) STEP1=SIGMA/3.
            TEMP1=PASS(I,4)
            PASS(I,4)=PASS(I,4)-4.5*STEP1
            DO 205 K=-4,4

*            Build the main image.
               CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                   %VAL(CNF_PVAL(POINT(2))),
     :                   %VAL(CNF_PVAL(POINT(3))),
     :                   %VAL(CNF_PVAL(POINT(4))),
     :                   %VAL(CNF_PVAL(POINT(5))),
     :                   %VAL(CNF_PVAL(POINT(6))),
     :                   RESID,STATUS)

*            Test for better residuals.
               IF(RESID.LT.BEST)THEN
                  KEEP=PASS(I,4)
                  BEST=RESID
               END IF

*            Increment peak value.
               PASS(I,4)=PASS(I,4)+STEP1

 205         CONTINUE

*      Set peak value to the best value.
         PASS(I,4)=KEEP*.3333+TEMP1*.6667

*       Vary x coordinate
           BEST=1E20
           STEP1=1.*XINC
           TEMP1=PASS(I,1)
           PASS(I,1)=PASS(I,1)-4.*STEP1
           DO 215 K=-4,4

*            Build the main image.
               CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                         %VAL(CNF_PVAL(POINT(2))),
     :                         %VAL(CNF_PVAL(POINT(3))),
     :                         %VAL(CNF_PVAL(POINT(4))),
     :                         %VAL(CNF_PVAL(POINT(5))),
     :                         %VAL(CNF_PVAL(POINT(6))),
     ;                         RESID,STATUS)

*            Test for better residuals.
               IF(RESID.LT.BEST)THEN
                  KEEP=PASS(I,1)
                  BEST=RESID
               END IF

*            Increment x value.
               PASS(I,1)=PASS(I,1)+STEP1

 215        CONTINUE

*         Set x to the best value.
            PASS(I,1)=KEEP*.3333+TEMP1*.6667

*         Vary y coordinate
            BEST=1E20
            STEP1=1.*YINC
            TEMP1=PASS(I,2)
            PASS(I,2)=PASS(I,2)-4.*STEP1
            DO 225 K=-4,4

*            Build the main image.
               CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                         %VAL(CNF_PVAL(POINT(2))),
     :                         %VAL(CNF_PVAL(POINT(3))),
     :                         %VAL(CNF_PVAL(POINT(4))),
     :                         %VAL(CNF_PVAL(POINT(5))),
     :                         %VAL(CNF_PVAL(POINT(6))),
     :                         RESID,STATUS)

*            Test for better residuals.
               IF(RESID.LT.BEST)THEN
                  KEEP=PASS(I,2)
                  BEST=RESID
               END IF

*         Reset the y value.
             PASS(I,2)=PASS(I,2)+STEP1

 225     CONTINUE

*      Set y value to the best value.
         PASS(I,2)=KEEP*.3333+TEMP1*.6667

*      Indicate the current parameter values.
         CALL GAU1_DISP(I,ANGCON,ANGOFF,PSIZE,PASS,STATUS)


 170  CONTINUE

*   Indicate the current residuals.
      CALL MSG_BLANK(STATUS)
      CALL MSG_FMTR('DEV','F6.1',BEST)
      CALL MSG_OUT(' ','Current arbitrary residual ^DEV',STATUS)

 2000 CONTINUE


*   Transfer the results to GUESS array.
      DO 3000 I=1,NSOUR
         DO 4000 J=1,7
            GUESS(I,J)=PASS(I,J)
 4000    CONTINUE
 3000  CONTINUE

*   Build the output image.
      IF(MODTYP.EQ.GAU2WHOLE) THEN

*      Model of the whole image.
         CALL GAU1_BUILD2(NSOUR,BACK,PRANGE,PASS,ELEMS,
     :                   %VAL(CNF_PVAL(POINT(3))),STATUS)


      ELSE

*      The image residuals near the sources.
         CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                   %VAL(CNF_PVAL(POINT(2))),
     :                   %VAL(CNF_PVAL(POINT(3))),
     :                   %VAL(CNF_PVAL(POINT(4))),
     :                   %VAL(CNF_PVAL(POINT(5))),
     :                   %VAL(CNF_PVAL(POINT(6))),
     :                   RESID,STATUS)

      END IF

 9999 CONTINUE

      END


      SUBROUTINE GAU1_TRAN2(ELEMS,ARRAY2,UPIX,XMAX,
     :                      ARRAY4,ARRAY5,ARRAY6,STATUS)
*+
*  Name:
*     GAU1_TRANS

*  Purpose:
*     Store the locations of all pixels in ARRAY2 that are non-bad.
*     Their locations are stored as indices and x/y pairs to allow the
*     fastest possible processing later.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_TRAN2(ELEMS,ARRAY2,UPIX,XMAX
*                      ARRAY4,ARRAY5,ARRAY6,STATUS)

*  Description:
*      Look through the ARRAY2 image and find the index,
*      X co-ordinate and Y co-ordinate of non-bad pixels and store them
*      in ARRAY4, ARRAY5 and ARRAY6 respectively.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of elements in the data NDF.
*     ARRAY2(ELEMS) = REAL (Given)
*        Array into which the mapped NDF will be transfered.
*     UPIX = INTEGER (Given)
*        The number of good pixels.
*     XMAX = INTEGER (Given)
*        Width of the image stored in ARRAY2.
*     ARRAY4(UPIX) = INTEGER (Returned)
*        Indices of non-bad pixels.
*     ARRAY5(UPIX) = INTEGER (Returned)
*        X co-ordinates of non-bad pixels.
*     ARRAY6(UPIX) = INTEGER (Returned)
*        Y co-ordinates of non-bad pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     22-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in ARRAY2
      INTEGER UPIX                    ! Number of non-bad pixels in ARRAY2
      INTEGER XMAX                    ! Width of the image
      REAL ARRAY2(ELEMS)              ! The mapped/masked NDF data array

*  Arguments Returned:
      INTEGER ARRAY4(UPIX)            ! Indices
      INTEGER ARRAY5(UPIX)            ! X co-ordinate
      INTEGER ARRAY6(UPIX)            ! Y co-ordinate

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
      INTEGER N                       ! A counter
      INTEGER X                       ! X co-ordinate
      INTEGER Y                       ! Y co-ordinate
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find all pixels in the image that are non-bad and determine
*   their co-ordinates.
      N=0
      DO 2000 I=1,ELEMS

*      Look for non-bad pixels only.
         IF(ARRAY2(I).NE.VAL__BADR) THEN

*         Increment the counter.
            N=N+1

*         Store the index
            ARRAY4(N)=I

*         Calculate X and Y value for the current pixel.
            Y=I/XMAX+1
            X=I-(Y-1)*XMAX

*         Store the values.
            ARRAY5(N)=X
            ARRAY6(N)=Y

         END IF

 2000 CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE GAU1_DISP(I,ANGCON,ANGOFF,PSIZE,PASS,STATUS)
*+
*  Name:
*     GAU1_DISP

*  Purpose:
*     Display on the screen what the results for the current iteration are.
*     Will use the rotation convention requested by the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_DISP(I,ANGCON,ANGOFF,PSIZE,PASS,STATUS)

*  Arguments:
*     I = INTEGER (Given)
*        Index of the PASS array elements to be used.
*     ANGCON = LOGICAL (Given)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.
*     ANGOFF = Given (Given)
*        Angular offset for position angles generated. Units degrees.
*     PSIZE = REAL (Given)
*        Pixel size/arcsec.  See psize in gau1_cmode for discussion.
*     PASS(10,7) = REAL (Given)
*        The Gaussian parameters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG:  Norman Gray (Starlink, GLA)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     22-Mar-1996 (GJP)
*       (Original version)
*     23-Feb-1998 (NG)
*       Included PSIZE argument.
*     19-JAN-2007 (TIMJ):
*       Correct FWHM calculation (missing sqrt(2) factor)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      LOGICAL ANGCON                  ! Position angle convention
      INTEGER I                       ! Index of current parameters
      REAL ANGOFF                     ! Position angle offset
      REAL PSIZE		      ! Pixel size in arcsec
      REAL PASS(10,7)                 ! Current parameters value

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL VALUE                      ! Temporary angle value
      CHARACTER WIDA*5,WIDB*5,UNITLAB*2
*   pass(i,5) and pass(i,6) are sigma in pixels.
*   Convert to arsecs (if PSIZE >= 1e-6) by multiplying by PSIZE
*   Convert to FWHM (if PSIZE > 0) by multiplying by 2sqrt(log(2))
      REAL SIZECONV		      ! Conversion factor sigma/px -> ?
      CHARACTER LINE*80		      ! output line

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Display the heading if I=1.
      IF(I.EQ.1) THEN
*      Create the heading, varying the legends depending on the value of PSIZE
         IF (PSIZE.GT.0.0) THEN
            WIDA='FWHMa'
            WIDB='FWHMb'
         ELSE
            WIDA='   Sa'
            WIDB='   Sb'
         ENDIF
         IF (ABS(PSIZE).GE.1E-6) THEN
            UNITLAB='as'
         ELSE
            UNITLAB='px'
         ENDIF
*      Format of header and data line
*ource      X           Y         Angle   FWHMa/xx     FWHMb/xx         Peak
*ii  FFFFFFFFFF  FFFFFFFFFF  FFFFFFFFFF  FFFFFFFFFF   FFFFFFFFFF   FFFFFFFFFF
*234567890123456789012345678901234567890123456789012345678901234567890
*        1         2         3         4         5         6         7
         CALL MSG_OUT (' ', 'Source      X           Y         Angle'/
     :     /'   '//WIDA//'/'//UNITLAB//'     '//WIDB//'/'/
     :     /UNITLAB//'         Peak ', STATUS)
      END IF

*   Calculate the conversion factor
      SIZECONV = 1.0
      IF (PSIZE.GT.0.0) THEN
*      Display FWHM, rather than sigma
         SIZECONV = SIZECONV * 2*SQRT(2*LOG(2.0))
      ENDIF
      IF (ABS(PSIZE).GE.1E-6) THEN
*      Display in units of arcsec, rather than pixels
         SIZECONV = SIZECONV * ABS(PSIZE)
      END IF

*   Adjust the ang according to the convention.
      VALUE=PASS(I,7)
      IF(ANGCON) VALUE=-VALUE
      VALUE=VALUE+ANGOFF

*   Apply limits.
      IF (VALUE.GT.179.99)  VALUE=VALUE-180.
      IF (VALUE.LT.-179.99) VALUE=VALUE+180.

*   Indicate the current parameter values.
      WRITE(LINE, 100) I,PASS(I,1),PASS(I,2),VALUE,
     :     PASS(I,5)*SIZECONV,PASS(I,6)*SIZECONV,PASS(I,4)
 100  format (T2,I2,T6,F10.2,T18,F10.2,T30,F10.2,
     :     T42,G10.2,T55,G10.2,T68,E10.3)

      CALL MSG_OUT (' ',LINE,STATUS)

 9999 CONTINUE

      END
