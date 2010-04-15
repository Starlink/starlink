      SUBROUTINE LOB1_FILL(ELEMS,ARRAY1,PRANGE,LBND,UBND,
     :                     X,Y,NUMP,WIDTH,NUMPS,ARRAY2,STATUS)
*+
*  Name:
*     LOB1_FILL

*  Purpose:
*     Given the co-ordinates of the region of the image in question, the
*     routine takes values from the pixels in a square of defined
*     width about the chosen co-ordinates and places them in an array for
*     subsequent use.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_FILL(ELEMS,ARRAY1,PRANGE,LBND,UBND,X,Y,NUMP,
*                    WIDTH,NUMPS,ARRAY2,STATUS)

*  Description:
*     Transfers the required pixels to the memory array that will be used
*     later to construct the pixel count histogram.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of pixels in the source image.
*     ARRAY1(ELEMS) = REAL (Given)
*        The source image array.
*     PRANGE(2) = INTEGER (Given)
*        Size of each image axis.
*     LBND(2) = INTEGER (Given)
*        Lower bound of the image.
*     UBND(2) = INTEGER (Given)
*        Upper bound of the image.
*     X = REAL (Given)
*        X co-ordinate to be used.
*     Y = REAL (Given)
*        Y co-ordinate to be used.
*     NUMP = INTEGER (Given)
*        Size of the array in which the pixel values may be stored.
*     WIDTH = INTEGER (Given)
*        Width of the square region of the image to be used.
*     NUMPS = INTEGER (Returned)
*        The number of pixels taken from the source image. Differs from NUMP
*        when the location requested is near the edge of the source image.
*     ARRAY2(NUMP) = REAL (Given and Returned)
*        The array into which values from the source image are placed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     29-JUN-1993 (GJP)
*     (Original version)

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER ELEMS                   ! The number of pixels in the source
                                      ! image
      INTEGER LBND(2)                 ! Lower bounds of image axes
      INTEGER NUMP                    ! Size of the array in which all the
                                      ! pixel counts are to be placed
      INTEGER PRANGE(2)               ! Size of each image axis
      INTEGER UBND(2)                 ! Upper bounds of image axes
      INTEGER WIDTH                   ! The width of the square region of
      REAL ARRAY1(ELEMS)              ! The source image array
      REAL X                          ! X co-ordinate of the image position
      REAL Y                          ! Y co-ordinate of the image position

*  Arguments Returned:
      INTEGER NUMPS                   ! The number of pixel taken from the
                                      ! source image
      REAL ARRAY2(NUMP)               ! The pixel count values to be used to
                                      ! construct the histogram
*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER ADD                     ! Address of the image pixel required
      INTEGER ADDP                    ! Temporary storage
      INTEGER XLOW                    ! Lowest X value for the box
      INTEGER XHIGH                   ! Highest X value for the box
      INTEGER XV                      ! Loop variable
      INTEGER YLOW                    ! Lowest Y value for the box
      INTEGER YHIGH                   ! Highest Y value for the box
      INTEGER YV                      ! Loop variable
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the values for the co-rdinates (in terms of data) of the
*   box surrounding the required image area.
      XLOW=INT(X)-WIDTH/2
      XHIGH=XLOW+WIDTH-1
      YLOW=INT(Y)-WIDTH/2
      YHIGH=YLOW+WIDTH-1

*   Adjust limits of the box to ensure the image edges are not exceeded.
      IF (XLOW.LT.1) XLOW=1
      IF (XHIGH.GT.PRANGE(1)) XHIGH=PRANGE(1)
      IF (YLOW.LT.1) YLOW=1
      IF (YHIGH.GT.PRANGE(2)) YHIGH=PRANGE(2)

*   Examine all pixels in that box and initialise the
*   pixels stored counter.
      NUMPS=0
      DO 10 YV=YLOW,YHIGH

*      Calculate part of the pixel array address required.
         ADDP=(YV-1)*PRANGE(1)

         DO 20 XV=XLOW,XHIGH

*         Calculate the pixel address required.
            ADD=ADDP+XV

*         Increment the pixel counter and store the value found.
            NUMPS=NUMPS+1
            ARRAY2(NUMPS)=ARRAY1(ADD)

 20      CONTINUE

 10   CONTINUE

 9999 CONTINUE

      END
