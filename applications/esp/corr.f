      SUBROUTINE CORR( STATUS )
*+
*  Name:
*     CORR

*  Purpose:
*     Performs cross-correlations on an image using a galaxy template.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CORR( STATUS )

*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.

*  Description:
*     Performs calculations to cross-correlate a circular shaped
*     exponential template with an image.
*     The exponential profile template chosen optimises the chances of
*     identifying faint diffuse galaxies/galaxies of (and near) a
*     scale length defined by the user.
*
*     Performs cross-correlation calculations on an input NDF image
*     file. The resulting image/plot is stored in an output NDF.
*
*     For each image pixel in turn, all the pixels within a defined
*     radius are identified. The values for each of these in turn have
*     their background values subtracted and the result (F1)
*     multiplied by a factor (F2) generated using an exponential function.
*     The values obtained for all the surrounding image pixels are
*     summed. The total generated is divided by using a normalisation
*     value created by taking the sums of square for F1 and F2,
*     multiplying them together and then taking the square root.
*     This normalised sum is placed in the
*     appropriate pixel of the output image and the program moves on
*     to the next input image pixel to be considered.
*
*     The circular elliptical mask used is of a radius 1.8x the
*     scale length requested. Studies undertaken by Phillipps and
*     Davies at Cardiff suggest that this value optimises the
*     detection sensitivity.
*
*     The correlation value obtained is multiplied by 1000 (or a user
*     defined value) to make display easier.
*
*     A border is present in the final output image which is the
*     same width as the radius of the template used. Pixels within the
*     border have been assigned the value bad.

*  Usage:
*     CORR IN OUT SCALE PSIZE BACK USEALL MULT [SIGMA] [NSIGMA]

*  ADAM Parameters:
*     BACK = _REAL (Read)
*        The modal pixel count value found in the input NDF.
*        Units counts.
*     IN = _NDF (Read)
*        The name of the NDF image that is to be examined.
*     MULT = _REAL (Read)
*        A multiplying factor applied to each of the results.
*        Default value is 1000.
*     NSIGMA = _REAL(Read)
*        The number of standard deviations above the sky level
*        count value, where the pixel count cutoff occurs.
*     OUT = _NDF (Write)
*        The name of the NDF data that will be created.
*     PSIZE = _REAL (Read)
*        The size of each pixel in arc seconds.  If the image contains
*        a SKY co-ordinate frame this value will be determined
*        automatically.
*     SCALE = _REAL (Read)
*        The scale length of the galaxies to be highlighted in the
*        output image. Units arc seconds.
*     SIGMA = _REAL (Read)
*        The standard deviations of the background pixel count within the
*        input NDF. Should be determined using a routine such as
*        HISTPEAK which ignores outliers.
*     USEALL = _LOGICAL (Read)
*        Used to indicate whether a pixel count threshold is to
*        be applied when calculating the correlation.

*  Examples:
*     corr in=hh1826 out=correl scale=8. psize=0.3 back=7437.
*          useall=true mult=1000.
*
*       Correlates image HH1826 with a mask/template optimised for
*       galaxies of 8 arc seconds scale length. The pixel size on the
*       image is .3 arc second, the background count value 7437 and
*       all the pixels on the image can be used in the calculation.
*       The output image is to be named CORREL.
*
*     corr in=forn out=forn4 scale=4. psize=0.22 mult=1000. back=666
*          useall=false sigma=15 nsigma=3
*
*       Correlates image FORN with a mask/template optimised for
*       galaxies of 4 arc seconds scale length. The pixel size is .22
*       arc seconds and the background count value 666.
*
*       Pixels that are brighter than 666+15x3 counts are not
*       included in the correlation calculations (USEALL=FALSE).
*       The output image is to be named FORN4.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-MAY-1993 (GJP)
*     (Original version)
*     11-NOV-1999 (MBT)
*     Modified for use with WCS components.

*  Notes:
*     It is assumed that the x and y axis pixels are of the same size.
*
*     To establish the statistical significance of a detection, this
*     application should be used in conjunction with MIXUP to allow noise
*     equivalent images to be generated and correlated thereby
*     establishing a 3 sigma limit.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'COR_PAR'               ! CORR constants
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
                                      ! for calculating correlation
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER XMAX                    ! Width in pixels of the image
      INTEGER YMAX                    ! Length in pixels of the image
      REAL DIAM                       ! Template diameter.
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
      REAL SCALE                      ! Scale length of the galaxy
      REAL SIGMA                      ! Standard deviation of the background count
      LOGICAL USEALL                  ! Are all the pixels to be used or
                                      ! will the very bright be ignored
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Begin an NDF context.
      CALL NDF_BEGIN

*   Indicate that the application is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP CORR running.',STATUS)
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

*   Set the data type in the output array to real.
      CALL NDF_STYPE('_REAL',NDF2,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the results NDF data array as _REAL values for updating.
      CALL NDF_MAP(NDF2,'Data','_REAL','UPDATE',POINT2(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Change the propogated title.
      CALL NDF_CPUT('ESP - Cross-correlation Image',NDF2,'TITLE',
     :              STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the sampling diameter required.
      CALL PAR_GET0R('SCALE',SCALE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   1.8 factor optimises the filter size.
      DIAM=2.*SCALE*1.8

*   Determine the pixel size.
      CALL ESP1_GTPSZ(NDF1,PSIZE,STATUS)

*   Check that the resultant radius isnt too small.
      RADIUS=NINT(DIAM/PSIZE/2.)
      IF (RADIUS.LT.1) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The number of pixels involved is'//
     :                ' too small.',STATUS)
         GOTO 9999
      END IF

*   Get the image background value.
      CALL PAR_GET0R('BACK',MODE,STATUS)

*   Determine whether any pixels are to be excluded if their value
*   is too high.
      CALL PAR_GET0L('USEALL',USEALL,STATUS)

*   Set up the cutout value.
      IF (.NOT.USEALL) THEN

*      Determine the background standard deviation to be used.
         CALL PAR_GET0R('SIGMA',SIGMA,STATUS)

*      Determine the number of standard deviations above sky at which to
*      apply the cutout. This is used to calculate at what value
*      of count, pixels are ignored. This is to allow very high
*      values to be ignored, which is useful for reducing the
*      influence of very bright objects or gamma rays.
         CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS)

*      Calculate the pixel count value, above which pixels
*      are to be ignored.
            HIEST=MODE+NSIGMA*SIGMA

      END IF

*   Determine the correlation multiplying factor to be used.
      CALL PAR_GET0R('MULT',MULT,STATUS)

      CALL MSG_BLANK(STATUS)

*   Inform the user of what will be done, since the routine takes a
*   long while to run.
      CALL NDF_MSG('FOUT',NDF1)
      CALL MSG_OUT(' ','Applying CORR to file: ^FOUT',STATUS)
      CALL NDF_MSG('FOUT',NDF2)
      CALL MSG_OUT(' ','Results file will be:  ^FOUT',STATUS)
      IF (USEALL) THEN
         CALL MSG_OUT(' ','High count cutoff was not used.',STATUS)
      ELSE
         CALL MSG_OUT(' ','High count cutoff was used.',STATUS)
      END IF
      CALL MSG_BLANK(STATUS)

*   Prepare values that will be passed to the subroutine.
      XMAX=PRANGE(1)
      YMAX=PRANGE(2)

*   Perform the calculations.
      CALL COR1_CORR(MULT,ELEMS,%VAL(CNF_PVAL(POINT1(1))),RADIUS,MODE,
     :                 XMAX,YMAX,USEALL,HIEST,STATUS,
     :                %VAL(CNF_PVAL(POINT2(1))))
         IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   Un-map the NDF data arrays.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_UNMAP(NDF2,'Data',STATUS)

*   End the NDF context.
      CALL NDF_END(STATUS)

      END
