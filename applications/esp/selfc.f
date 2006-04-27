      SUBROUTINE SELFC( STATUS )
*+
*  Name:
*     SELFC

*  Purpose:
*     To perform self-correlations on an NDF image file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SELFC( STATUS )

*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.

*  Description:
*     Performs a self-correlation calculation on an input NDF image
*     file. The resulting correlation image/plot is stored to disk.
*
*     The self-correlated image may be used to find flat-fielding
*     faults and faint diffuse objects of a given size.
*
*     To reduce the influence of bright objects or cosmic rays;
*     the user may elect to employ a cut out pixel count value where
*     any pixel found to be above that value is ignored. The cutout
*     value is determined by the user inputting a global mode value
*     (usually the sky background count - obtained via HISTPEAK),
*     the background count standard deviation and the number of standard
*     deviations above sky level at which the cutout should occur.
*
*     The user is required to enter a value for the size of object(s)
*     of interest (roughly the template size) and also the image pixel
*     size.
*
*     The value for each pixel of the output image is determined
*     as follows. An imaginary circle is drawn about the pixel
*     and all pixel pairs within that circle, that lie on opposite
*     sides of the centre from each other, are stored.
*
*     Each pair is then considered in turn and the modal count value
*     subtracted from each. The resultant residual pixel count values
*     are then multiplied together. The values found for all the pairs
*     are then summed, the total divided by the number of pixel pairs
*     found and the square root taken. In the event of a negative sum
*     being found the value given is the square root of the magnitude
*     of the self-correlation multiplied by -1.
*
*     The resultant value is some measure of the extent to
*     which points within that circle (about the current pixel)
*     are correlated.
*
*     The method assumes some sort of symmetry is present in the
*     objects detected but appears to work well on a wide range of
*     image types.
*
*     A border is present in the output image which the same width
*     as the radius of the template. All pixels within this border
*     are assigned the value bad.
*

*  Usage:
*     SELFC IN OUT DIAM PSIZE BACK USEALL [SIGMA] [NSIGMA]

*  ADAM Parameters:
*     BACK = _REAL (Read)
*        The modal pixel count value found in the input NDF.
*        Units counts.
*     DIAM = _REAL (Read)
*        The diameter of the galaxies to be searched for in the
*        image. Units arc seconds.
*     IN = _NDF (Read)
*        The name of the NDF data that is to be examined.
*     NSIGMA = _REAL(Read)
*        The number of standard deviations above the sky level
*        count value, where the pixel count cutoff occurs.
*     OUT = _NDF (Write)
*        The name of the NDF that will be created.
*     PSIZE = _REAL (Read)
*        The size of each pixel in arc seconds.  If the image contains
*        a SKY co-ordinate frame this value will be determined
*        automatically.
*     SIGMA = _REAL (Read)
*        The standard deviation of the background count within the
*        input NDF. Should be determined using a routine such as
*        HISTPEAK which ignores outliers.
*     USEALL = _LOGICAL (Read)
*        Used to indicate whether a pixel count threshold is to
*        be a applied when calculating the self-correlation.

*  Examples:
*     selfc in=search out=scorr diam=15. psize=0.5 back=727.
*           useall=true
*        A self-correlation is carried out on image GALAXIES. The
*        search is for an object 15 arc seconds across. The image
*        pixel size is .5 arc seconds and the background count level
*        is 727. USEALL=TRUE ensures that no pixels are excluded from
*        the correlation calculation.
*
*     selfc in=plate out=plates diam=5. back=6600.
*           useall=false sigma=35. nsigma=5.
*        A self-correlation is carried out on image PLATE. The search
*        is for an object 5 arc seconds across.  The background count
*        level is 6600 and the image pixel size in arc seconds will
*        be determined from a SKY coordinate frame in the image's WCS
*        component if possible.  Since USEALL=FALSE, all pixels with
*        a count value above 6600+35.x5. are excluded from the
*        correlation calculation.  The output image is named PLATES.

*  Implementation Status:
*     At present suitable normalisation factors have not been
*     implemented. These may be added. As the program stands it is
*     useful for looking at an image to detect faint objects and
*     provides a comparison for users employing cross-correlation
*     techniques. In addition, it provides a simple way of detecting
*     areas of an image where flatfielding has not been entirely
*     successful.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1992 (GJP)
*     (Original version)
*     11-NOV-1999 (MBT)
*     Modified for use with WCS components.

*  Notes:
*     It is assumed through out that the x and y axis pixels
*     sizes are the same.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'SEL_PAR'               ! SELFC constants
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
                                      ! for calculating self-correlation
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER XMAX                    ! Width in pixels of the image
      INTEGER YMAX                    ! Length in pixels of the image
      REAL DIAM                       ! Width of the circular area used
                                      ! for calculating self-correlation
      REAL HIEST                      ! Highest pixel count used
                                      ! in the correlation
      REAL MODE                       ! The global mode value for the
                                      ! NDF image
      REAL NSIGMA                     ! The number of standard deviations
                                      ! above the modal value that a
                                      ! pixel must be, to be ignored
      REAL PSIZE                      ! The pixel size of the image
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
      CALL MSG_OUT(' ','ESP SELFC running.',STATUS)
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

*   Set the output NDF data type as real.
      CALL NDF_STYPE('_REAL',NDF2,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the results NDF data array as _REAL values for updating.
      CALL NDF_MAP(NDF2,'Data','_REAL','UPDATE',POINT2(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Change the propogated title.
      CALL NDF_CPUT('ESP - Self-correlation Image',NDF2,'TITLE',
     :               STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the galaxy size required.
      CALL PAR_GET0R('DIAM',DIAM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the pixel size.
      CALL ESP1_GTPSZ(NDF1,PSIZE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Check that the pixel size is not too small.
      IF (PSIZE.LT.SEL1__VSMAL) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The pixel size supplied is too small.',
     :                STATUS)
         GOTO 9999
      END IF

*   Check that the resultant radius isnt too small.
*   1.8 value put in as a guess at the region over which pixels
*   may contribute to objects of the size required.
      RADIUS=NINT(DIAM/PSIZE/2.*1.8)
      IF (RADIUS.LT.1) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The number of pixels involved is'//
     :                ' too small.',STATUS)
         GOTO 9999
      END IF

*   Determine the global mode value required.
      CALL PAR_GET0R('BACK',MODE,STATUS)

*   Determine whether any pixels are to be excluded if their value
*   is too high.
      CALL PAR_GET0L('USEALL',USEALL,STATUS)

*   Set up the cutout value.
      IF (.NOT.USEALL) THEN

*      Determine the image standard deviation to be used.
         CALL PAR_GET0R('SIGMA',SIGMA,STATUS)

*      Determine the number of SIGMAs above sky at which to
*      apply the cutout. This is used to calculate at what value
*      of count, pixels are ignored. This is to allow very high
*      values to be ignored, which is useful for reducing the
*      influence of very bright objects or gamma rays.
         CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS)

*      Calculate the pixel count value, above which pixels
*      are to be ignored.
         HIEST=MODE+NSIGMA*SIGMA

      END IF

      CALL MSG_BLANK(STATUS)

*   Inform the user of what will be done, since the routine takes a
*   long while to run.
      CALL NDF_MSG('FOUT',NDF1)
      CALL MSG_OUT(' ','Self-correlating file: ^FOUT',STATUS)
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

*   Create an self-correlation image.
         CALL SEL1_SELFC(ELEMS,%VAL(CNF_PVAL(POINT1(1))),
     :              RADIUS,MODE,
     :              XMAX,YMAX,USEALL,HIEST,STATUS,
     :              %VAL(CNF_PVAL(POINT2(1))))
         IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   Un-map the NDF data arrays.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_UNMAP(NDF2,'Data',STATUS)

*   End the NDF context.
      CALL NDF_END(STATUS)

      END

