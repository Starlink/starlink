      SUBROUTINE ELF1_HILOW(ELEMS,ARRAY,PRANGE,XO,YO,SEARCH,
     :                      VMIN,VMAX,STATUS)
*+
*  Name:
*     ELF1_HILOW

*  Purpose:
*     Find the highest and lowest pixel counts in a given
*     circular chunk of the image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_HILOW(ELEMS,ARRAY,PRANGE,XO,YO,SEARCH,VMIN,VMAX,STATUS)

*  Description:
*     Scans through all of the pixels within the image bounds defined
*     by the input variables. Each is checked to see that it is within
*     the bounds of the image and also that it is not bad.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        Image pixel array.
*     PRANGE(2) = INTEGER (Given)
*        Size of the image x and y directions. Units pixels.
*     XO = REAL (Given)
*        X co-ordinate of galaxy centre. Units pixels.
*     YO = REAL (Given)
*        Y co-ordinate of galaxy centre. Units pixels.
*     SEARCH = REAL (Given)
*        Radius about the galaxy centre that is to be sampled. Units pixels.
*     VMIN = REAL (Returned)
*        Count of the least bright pixel. Units counts.
*     VMAX = REAL (Returned)
*        Count of the brightest pixel found. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     7-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER PRANGE(2)               ! Image size in each dimension
      REAL ARRAY(ELEMS)               ! Image pixel array
      REAL SEARCH                     ! Radius surrounding galaxy centre
      REAL XO                         ! X co-ordinate of galaxy centre
      REAL YO                         ! Y co-ordinate of galaxy centre

*  Arguments Returned:
      REAL VMIN                       ! Count of the faintest pixel
      REAL VMAX                       ! Count of brightest pixel

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER ADD                     ! Array element containing the
                                      ! count value for a given pixel
      INTEGER LOX                     ! Lower x co-ord. limit
      INTEGER LOY                     ! Lower y co-ord. limit
      INTEGER HIX                     ! Upper x co-ord. limit
      INTEGER HIY                     ! Upper y co-ord. limit
      INTEGER X                       ! Current X co-ordinate
      INTEGER Y                       ! Current Y co-ordinate
      REAL RSQ                        ! Square of pixel distance from origin
      REAL RSQLIM                     ! Square of max permitted origin
                                      ! to pixel distance
      REAL V                          ! Pixel count value

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the x axis limits for the part of the image to be examined.
      LOX=XO-SEARCH
      HIX=XO+SEARCH
      IF (LOX.LT.1) LOX=1
      IF (HIX.GT.PRANGE(1)) HIX=PRANGE(1)

*   Set the y axis limits for the part of the image to be examined.
      LOY=YO-SEARCH
      HIY=YO+SEARCH
      IF (LOY.LT.1) LOY=1
      IF (HIY.GT.PRANGE(2)) HIY=PRANGE(2)

*   Look at a given chunk of the image and determine the highest and lowest
*   pixel count values.
      RSQLIM=SEARCH*SEARCH
      VMAX=-ELF__VBIG
      VMIN=ELF__VBIG

      DO 10 X=LOX,HIX

         DO 20 Y=LOY,HIY

*         Check is not too far from the centre of the galaxy.
            RSQ=(X-XO)*(X-XO)+(Y-YO)*(Y-YO)
            IF (RSQ.LE.RSQLIM) THEN

*            Get the pixel value.
               ADD=(Y-1)*PRANGE(1)+X
               V=ARRAY(ADD)

*            Check the value found compared to the current maximum and minimum
*            values retained.
               IF (V.NE.VAL__BADR) THEN

*               Replace lowest value found or highest value found if necessary.
                  IF (V.LT.VMIN) VMIN=V
                  IF (V.GT.VMAX) VMAX=V

               END IF

            END IF

 20      CONTINUE

 10   CONTINUE

 9999 CONTINUE

      END
