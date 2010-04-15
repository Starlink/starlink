
*+  KFH_IMCOPY - Copies image data ready for sorting.
      SUBROUTINE KFH_IMCOPY(IMAGE,COPY,XDIM,YDIM,X1,Y1,X2,Y2)
*    Description :
*     This routine copies the 2-D image data of specified region
*     of an image into a 1-D array which is then used in a NAG
*     sorting routine. The image data elements are also converted
*     to double precision since NAG requires this concession.
*    Invocation :
*     CALL KFH_IMCOPY(IMAGE,COPY,XDIM,YDIM,X1,Y1,X2,Y2)
*    Parameters :
*     IMAGE(XDIM,YDIM) = REAL
*           The array containing the data of the whole image.
*     COPY(270000) = DOUBLE PRECISION
*           The 1-D array to hold the data elements of the
*           specified region.
*     XDIM = INTEGER
*           The X-dimension of the whole image.
*     YDIM = INTEGER
*           The Y-dimension of the whole image.
*     X1 = INTEGER
*           The lower X-coordinate of the specified region
*           of the image.
*     Y1 = INTEGER
*           The lower Y-coordinate of the specified region
*           of the image.
*     X2 = INTEGER
*           The upper X-coordinate of the specified region
*           of the image.
*     Y2 = INTEGER
*           The upper Y-coordinate of the specified region
*           of the image.
*    Method :
*     The region of 2-D image data is copied by assigning
*     the elements of the new 1-D array to the double
*     precision values of the elements of the 2-D array.
*    Author :
*     S.Chan (RGVAD::KFH)
*    History :
*     13 September 1983: Original(RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER XDIM                       ! X-dimension of the image.
      INTEGER YDIM                       ! Y-dimension of the image.
      DOUBLE PRECISION COPY(270000)      ! 1-D array holding the copy
*                                        ! of the image data of the
*                                        ! selected region.
      INTEGER I                          ! General variable.
      REAL IMAGE(XDIM,YDIM)              ! Array holding the data of the
*                                        ! whole image.
      INTEGER IND                        ! Count variable.
      INTEGER J                          ! General variable.
      INTEGER X1                         ! Lower X-coordinate of the
*                                        ! chosen region of the image.
      INTEGER X2                         ! Lower Y-coordinate of the
*                                        ! chosen region of the image.
      INTEGER Y1                         ! Upper X-coordinate of the
*                                        ! chosen region of the image.
      INTEGER Y2                         ! Upper Y-coordinate of the
*                                        ! chosen region of the image.
*-

*
*    This routine copies a 2-dimensional array into a
*    1-dimensional array ready to be used in a sorting
*    routine.
*

      IND = 1

      DO I = Y1,Y2,1

         DO J = X1,X2,1

            COPY(IND) = DBLE(IMAGE(J,I))
            IND = IND + 1

         END DO

      END DO

      END
