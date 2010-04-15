      SUBROUTINE FASTMED(STATUS)
*+
*  Name:
*     FASTMED

*  Purpose:
*     Applies a square median filter of user defined size to an input
*     image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FASTMED(STATUS)

*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.

*  Description:
*     The method used employs a rolling histogram and allows
*     the whole image to be filtered.
*
*     The median pixel value in the region surrounding each input
*     image pixel is  calculated. This value is then subtracted from
*     the value of the pixel being considered. Finally, the original
*     background count is added to the result, which is placed in the
*     corresponding pixel of the output image.

*  Usage:
*     FASTMED IN OUT BACK SIGMA WIDTH

*  ADAM Parameters:
*     BACK = _REAL (Read)
*        Image background count value. Units counts.
*     IN = _NDF (Read)
*        The name of the NDF to which the filter will be applied.
*     OUT = _NDF (Write)
*        The name of the output NDF that will be created.
*     SIGMA = _REAL (Read)
*        Standard deviation of the background count value. Units counts.
*     WIDTH = _INTEGER (Read)
*        The width of the filter to be employed. Units pixels.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-MAY-1993 (GJP)
*     (Original version)
*     11-NOV-1999 (MBT)
*     Modified for use with WCS components.

*  Examples:
*     fastmed in=field out=flatgal back=760. sigma=27. width=72
*
*        In this example, a 72x72 pixel filter will be applied to
*        the input image FIELD. The resulting median filtered image
*        will be placed in output image FLATGAL. The input image
*        (FIELD) had a global background count value of 760 with
*        an associated standard deviation of 27 counts.

*  Notes:
*     With small filters it may be found that the resulting output
*     images are noisy. This is due to the small
*     number of pixels contributing to the histogram. The problem will
*     be most obvious at the image edges and corners. For this reason
*     some users may find it necessary to clip the output image by
*     WIDTH/2 pixels on each edge to generate better results.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
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
      INTEGER RADIUS                  ! Radius of the median filter
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER WIDTH                   ! Width of the median filter
      INTEGER XMAX                    ! Width in pixels of the image
      INTEGER YMAX                    ! Length in pixels of the image
      REAL BACK                       ! Background count value
      REAL SIGMA                      ! Standard deviation of BACK
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Begin an NDF context.
      CALL NDF_BEGIN

*   Indicate that the application is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP FASTMED running.',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display the name of the NDF.
      CALL NDF_MSG('IN',NDF1)
      CALL MSG_OUT(' ','Filename:   ^IN',STATUS)

*   See if the title component is defined. If so, display its value.
      CALL NDF_CMSG('TITLE',NDF1,'Title',STATUS)
      CALL MSG_OUT('TITLE','Title:      ^TITLE',STATUS)

*   Get the pixel-index bounds of an NDF and store in LBND and UBND.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Store the size (in pixels) of the image dimensions.
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1
      XMAX=PRANGE(1)
      YMAX=PRANGE(2)

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
      CALL NDF_STYPE('_REAL',NDF2,'Data',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the results NDF data array as _REAL values for updating.
      CALL NDF_MAP(NDF2,'Data','_REAL','UPDATE',POINT2(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Change the propogated title.
      CALL NDF_CPUT('ESP - FASTMED Image',NDF2,'TITLE',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the background count value.
      CALL PAR_GET0R('BACK',BACK,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the standard deviation of the background count.
      CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (SIGMA.LE.0.0) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The sigma value is too'/
     :                /' small.',STATUS)
         GOTO 9999
      END IF

*   Determine the width of the median filter.
      CALL PAR_GET0I('WIDTH',WIDTH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (WIDTH.LT.2) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The width selected is too small.',STATUS)
         GOTO 9999
      END IF

*   Determine the radius of the filter to be used. If the size is too
*   large then display a message announcing the size used.
      RADIUS=WIDTH/2
      IF (RADIUS.GT.PRANGE(2)/2) THEN
          RADIUS=PRANGE(2)/2
          CALL MSG_SETI('SIZE',PRANGE(2))
          CALL MSG_OUT(' ','The size has been limited to ^SIZE'/
     :    /'x ^SIZE',STATUS)
          CALL MSG_BLANK(STATUS)
      END IF

*   Inform the user of what will be done, since the routine takes a
*   long while to run.
      CALL MSG_BLANK(STATUS)
      CALL NDF_MSG('FOUT',NDF1)
      CALL MSG_OUT(' ','Applying FASTMED to file: ^FOUT',STATUS)
      CALL NDF_MSG('FOUT',NDF2)
      CALL MSG_OUT(' ','Results file will be:     ^FOUT',STATUS)
      CALL MSG_BLANK(STATUS)

*   Carry out the operation to median filter the image.
      CALL FAS1_FILT(ELEMS,%VAL(CNF_PVAL(POINT1(1))),RADIUS,XMAX,YMAX,
     :               BACK,SIGMA,%VAL(CNF_PVAL(POINT2(1))),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   Un-map the NDF data arrays.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_UNMAP(NDF2,'Data',STATUS)

*   End the NDF context.
      CALL NDF_END(STATUS)

      END
