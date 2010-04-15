*+  KFH_HISTOGRAM - Calculates the histogram of a region of an image.
      SUBROUTINE KFH_HISTOGRAM(IMAGE,XDIM,YDIM,HGRAM,X1,Y1,X2,Y2,
     : NUMBIN,HMIN,HMAX)
*    Description :
*     This routine calculates the histogram of specified area
*     of an image.
*    Invocation :
*     CALL KFH_HISTOGRAM(IMAGE,XDIM,YDIM,HGRAM,X1,Y1,X2,Y2,NUMBIN,HMIN,HMAX)
*    Parameters :
*     IMAGE(XDIM,YDIM) = REAL
*           The array which holds the whole image data.
*     HGRAM(NUMBIN) = INTEGER
*           This is the array which holds the histogram.
*     X1 = INTEGER
*           The lower X-bound of the region.
*     Y1 = INTEGER
*           The lower Y-bound of the region.
*     X2 = INTEGER
*           The upper X-bound of the region.
*     Y2 = INTEGER
*           The upper Y-bound of the region.
*     NUMBIN = INTEGER
*           The number of bins in the histogram.
*     HMIN = REAL
*           The minimum value of the region.
*     HMAX = REAL
*           The maximum value of the region.
*    Method :
*     The bins to house the histogram are cleared
*     and a suitable scale for the histogram is
*     calculated. Each element of the image region
*     is transformed to determine the appropriate
*     bin that it belongs to. A bin is incremented
*     each time an element is found to belong to it.
*    Authors :
*     S.Chan (RGVAD::KFH)
*    History :
*     15 September 1983.
*    Type Definitions :
      IMPLICIT NONE
*    Global constants:
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NUMBIN                     ! Number of bins in the histogram.
      INTEGER XDIM                       ! X-dimension of the image.
      INTEGER YDIM                       ! Y-dimension of the image.
*      INTEGER HGRAM(NUMBIN)              ! Array to hold the histogram.
      INTEGER HGRAM(5000)
      REAL HMAX                          ! Maximum value of the region.
      REAL HMIN                          ! Minimum value of the region.
      INTEGER I                          ! General variable.
      REAL IMAGE(XDIM,YDIM)              ! Array which holds the image data.
      INTEGER J                          ! General variable.
      INTEGER K                          ! General variable.
      INTEGER L                          ! General variable.
      REAL SCALE                         ! Scale of the histogram.
      REAL V                             ! General variable.
      INTEGER X1                         ! Lower X-bound.
      INTEGER X2                         ! Upper X-bound.
      INTEGER Y1                         ! Lower Y-bound.
      INTEGER Y2                         ! Upper Y-bound.
*-

*
*    Calculate the histogram.
*

      DO I = 1,NUMBIN

         HGRAM(I) = 0

      END DO

      SCALE = (NUMBIN-1)/MAX(HMAX-HMIN,1E-18)

      DO J = Y1,Y2,1

         DO K = X1,X2,1

            V = IMAGE(K,J)
            L = (V-HMIN)*SCALE + 1
            IF (1.LE.L.AND.L.LE.NUMBIN) THEN

               HGRAM(L) = HGRAM(L) + 1

            ENDIF

         END DO

      END DO

      END
