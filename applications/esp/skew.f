      SUBROUTINE SKEW( STATUS )
*+
*  Name:
*     SKEW

*  Purpose:
*     Generates a skewness representation of the image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SKEW( STATUS )

*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.

*  Description:
*     Performs skewness calculations on an input NDF image
*     file. The resulting skewness image/plot is stored to disk.
*
*     Two actions have been taken to reduce the influence of bright
*     objects or cosmic rays:
*
*     - the user may elect to employ a cut out pixel count value
*       where any pixel found to be above that value is ignored. The
*       cutout value is determined by the user inputting a global mode
*       value, the background count standard deviation (available via
*       HISTPEAK) and the number of standard deviations above sky level
*       at which the cutout should be.
*
*     - a local mean value may be used as the mode.
*
*     The user is required to enter the size of the sampling area
*     and the pixel size in arc secs. This is used to define the
*     width of pixel template radius employed. It is assumed that
*     pixels are the same size in the x and y directions.
*
*     The skewness value assigned to each pixel of the output image
*     is calculated using the values of pixel count found for all the
*     non-bad pixels within the calculated radius. The value obtained
*     is multiplied by 1000 (or a user defined value) to make display
*     easier.
*
*     The modal count value used during the calculation is either the
*     global value (defined by the user) or a local value calculated
*     as required.
*
*     The resultant value is some measure of the extent to which the
*     pixel count values surrounding a given pixel are not distributed
*     in a Gaussian manner.
*
*     A border is present in the final output image which is the same
*     width as the radius of the template used. Pixels within the
*     border have been assigned the value bad.

*  Usage:
*     SKEW IN OUT MODET WIDTH PSIZE MULT [BACK] [SIGMA]
*          [NSIGMA] [USEALL]

*  ADAM Parameters:
*     BACK = _REAL (Read)
*        The background pixel count value found in the input NDF.
*        Units counts. Only used if MODET = TRUE.
*     IN = _NDF (Read)
*        The name of the NDF that is to be examined.
*     MODET = _LOGICAL (Read)
*        Used to indicate whether a global modal count value
*        is to be used when calculating the skewness values.
*        The alternative is for the application to calculate and use the
*        local mode value. See BACK. Using a local background
*        calculation can be slow.
*     MULT = _REAL (Read)
*        A multiplying factor applied to each of the results.
*        Default value is 1000.
*     NSIGMA = _REAL(Read)
*        The number of standard deviations above the sky level
*        count value, where the pixel count cutoff occurs.
*        Only employed if a global pixel count modal value is
*        in use (MODET = TRUE).
*     OUT = _NDF (Write)
*        The name of the NDF that will be created.
*     PSIZE = _REAL (Read)
*        The size of each pixel in arc seconds.  If the image contains
*        a SKY co-ordinate frame this value will be determined
*        automatically.
*     SIGMA = _REAL (Read)
*        The standard deviation of the back ground count within the
*        input NDF. Should be determined using a routine such as
*        HISTPEAK which ignores outliers. Only employed if a global
*        pixel count modal value is in use (MODET = TRUE).
*        Units counts.
*     USEALL = _LOGICAL (Read)
*        Used to indicate whether a pixel count threshold is to
*        be a applied when calculating the skewness.
*        Only employed if MODET has been set to ensure that
*        a global modal value is in use.
*     WIDTH = _REAL (Read)
*        The width of the sampling area/filter to be passed over the
*        image. Units arc seconds.

*  Examples:
*     skew in=ic3374 out=skewed modet=false width=10. psize=0.5
*          mult=1000
*        A skewness image named SKEWED is generated using IC3374 as
*        the source image. The sampling area from which pixels are
*        selected is 10 arc seconds across. The individual pixel size
*        is .5 arc seconds so the area is 20 pixels across. All the
*        skewness values generated for the output image are multiplied
*        by a factor of 1000, and local background values are used
*        throughout.
*
*     skew in=jet out=sjet modet=true width=5. mult=1000.
*          back=2010. useall=true
*        An output image SJET is generated using JET as the source
*        image. The background count is 2010 and the pixel size will
*        be determined from the WCS component of the source image.
*        All the pixels in the image can be used in the calculation.
*        The sampling area width is 5 arc seconds. All the pixels
*        in the image can be used in the calculation.
*
*     skew in=sgp27 out=result modet=true width=8. psize=1. mult=1000.
*          back=4505. sigma=23.7 nsigma=10. useall=false
*        The output image generated is created by assuming a global
*        background count of 4505. with an associated standard deviation
*        of 23.7 counts. All pixels of a count value greater
*        than 4505+23.7x10. are excluded from the calculations.

*  Implementation Status:
*     As the program stands it is useful for looking at an image to
*     with a view to detecting faint objects and flat-fielding
*     faults. It may be easily extended by the user to provide
*     plots showing other statistical quantities such as kurtosis
*     or S/N.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1992 (GJP)
*     (Original version)
*     11-NOV-1999 (MBT)
*     Modified for use with WCS components.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'SKE_PAR'               ! SKEW constants
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER ELEMS                   ! Number of data items in the NDF
      INTEGER LBND(7)                 ! Lower bounds for each image axis
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDF2                    ! Identifier for the results NDF
      INTEGER NDIM                    ! Number of dimensions in the image
      INTEGER POINT1(10)              ! Pointer to the data component of
                                      ! NDF1
      INTEGER POINT2(10)              ! Pointer to the data component of
                                      ! NDF2
      INTEGER PRANGE(2)               ! Number of pixels in the image x
                                      ! and y axes
      INTEGER RADIUS                  ! Radius of the circular area used
                                      ! for calculating skewness
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER XMAX                    ! Width in pixels of the image
      INTEGER YMAX                    ! Length in pixels of the image
      REAL HIEST                      ! Highest pixel count used
                                      ! in the correlation
      REAL MODE                       ! The global mode value for the
                                      ! NDF image
      REAL MULT                       ! Multiplying factor applied to
                                      ! all the output values
      REAL NSIGMA                     ! The number of standard deviations
                                      ! above the modal value that a
                                      ! pixel must be, to be ignored
      REAL PSIZE                      ! The pixel size of the image
      REAL SIGMA                      ! Standard deviation of the background
      REAL WIDTH                      ! Width of the circular area used
                                      ! for calculating skewness
      LOGICAL MODET                   ! What type of mode is to be used
      LOGICAL USEALL                  ! Are all the pixels to be used or
                                      ! will the very bright be ignored
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Begin an NDF context.
      CALL NDF_BEGIN

*   Indicate that the applicatiion is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP SKEW running.',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display the name of the NDF.
      CALL NDF_MSG('IN',NDF1)
      CALL MSG_OUT(' ','Filename:   ^IN',STATUS)

*   See if the title component is defined. If so, display its value.
      CALL NDF_CMSG('TITLE',NDF1,'Title',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT('TITLE','Title:      ^TITLE',STATUS)

*   Get the pixel-index bounds of an NDF and store in LBND and UBND.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Store the size (in pixels) of the image dimensions.
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1

*   Display the image x and y axis sizes (pixels).
      CALL MSG_SETI('PR1',PRANGE(1))
      CALL MSG_SETI('PR2',PRANGE(2))
      CALL MSG_OUT(' ','Shape:      ^PR1 x ^PR2 pixels',STATUS)

*   Display the image x and y axis ranges (pixels).
      CALL MSG_SETI('L1',LBND(1))
      CALL MSG_SETI('L2',UBND(1))
      CALL MSG_SETI('L3',LBND(2))
      CALL MSG_SETI('L4',UBND(2))
      CALL MSG_OUT(' ','Bounds:     x= ^L1:^L2  y= ^L3:^L4'
     :             ,STATUS)

*   Calculate the maximum number of pixels in the image.
      ELEMS=PRANGE(2)*PRANGE(1)

*   Display the image size.
      CALL MSG_SETI('ELEMS',ELEMS)
      CALL MSG_OUT(' ','Image size: ^ELEMS pixels',STATUS)

*   Map the NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'Data','_REAL','READ',POINT1(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Propogate an NDF to contain the results.
      CALL NDF_PROP(NDF1,'Data,WCS','OUT',NDF2,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set the output NDF data type to real.
      CALL NDF_STYPE('_REAL',NDF2,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the results NDF data array as _REAL values for updating.
      CALL NDF_MAP(NDF2,'Data','_REAL','UPDATE',POINT2(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Change the propogated title.
      CALL NDF_CPUT('ESP - Skewness Image',NDF2,'TITLE',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the sampling width required.
      CALL PAR_GET0R('WIDTH',WIDTH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the pixel size.
      CALL ESP1_GTPSZ(NDF1,PSIZE,STATUS)

*   Check that the resultant radius isnt too small.
      RADIUS=NINT(WIDTH/PSIZE/2.)
      IF (RADIUS.LT.1) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The number of pixels involved is'//
     :                ' too small.',STATUS)
         GOTO 9999
      END IF

*   Determine whether the local mode or a global mode value is to
*   be used.
      CALL PAR_GET0L('MODET',MODET,STATUS)

*   Determine the global mode value required.
      IF (MODET) THEN

         CALL PAR_GET0R('BACK',MODE,STATUS)

*      Determine whether any pixels are to be excluded if their value
*      is too high.
         CALL PAR_GET0L('USEALL',USEALL,STATUS)

*      Set up the cutout value.
         IF (.NOT.USEALL) THEN

*         Determine the background standard deviation to be used.
            CALL PAR_GET0R('SIGMA',SIGMA,STATUS)


*         Determine the number of standard deviations above sky to
*         apply the cutout. This is used to calculate at what value
*         of count, pixels are ignored. This is to allow very high
*         values to be ignored, which is useful for reducing the
*         influence of very bright objects or gamma rays.
            CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS)

*         Calculate the pixel count value, above which pixels
*         are to be ignored.
            HIEST=MODE+NSIGMA*SIGMA

         END IF

      END IF

*   Determine the skewness multiplying factor to be used.
      CALL PAR_GET0R('MULT',MULT,STATUS)

      CALL MSG_BLANK(STATUS)

*   Inform the user of what will be done, since the routine takes a
*   long while to run.
      CALL NDF_MSG('FOUT',NDF1)
      CALL MSG_OUT(' ','Applying SKEW to file: ^FOUT',STATUS)
      CALL NDF_MSG('FOUT',NDF2)
      CALL MSG_OUT(' ','Results file will be:  ^FOUT',STATUS)
      IF (MODET) THEN
         CALL MSG_OUT(' ','Global background value used.',STATUS)
         IF (USEALL) THEN
            CALL MSG_OUT(' ','High count cutoff was not used.',STATUS)
         ELSE
            CALL MSG_OUT(' ','High count cutoff was used.',STATUS)
         END IF
      ELSE
         CALL MSG_OUT(' ','Local background values used.',STATUS)
      END IF
      CALL MSG_BLANK(STATUS)

*   Prepare values that will be passed to the subroutine.
      XMAX=PRANGE(1)
      YMAX=PRANGE(2)

*   Create a skewness image.
      IF (MODET) THEN

*      Global mode version.
         CALL SKE1_GLOBAL(MULT,ELEMS,%VAL(CNF_PVAL(POINT1(1))),
     :               RADIUS,MODE,
     :               XMAX,YMAX,USEALL,HIEST,STATUS,
     :               %VAL(CNF_PVAL(POINT2(1))))
         IF (STATUS.NE.SAI__OK) GOTO 9999

      ELSE

*      Local mode version.
         CALL SKE1_LOCAL(MULT,ELEMS,%VAL(CNF_PVAL(POINT1(1))),
     :                   RADIUS,
     :                   XMAX,YMAX,STATUS,
     :                   %VAL(CNF_PVAL(POINT2(1))))
         IF (STATUS.NE.SAI__OK) GOTO 9999

      END IF

 9999 CONTINUE

*   Un-map the NDF data arrays.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_UNMAP(NDF2,'Data',STATUS)

*   End the NDF context.
      CALL NDF_END(STATUS)

      END

