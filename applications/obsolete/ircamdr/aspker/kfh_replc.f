*+  KFH_REPLC - Replaces the patch with the original data.
      SUBROUTINE KFH_REPLC(PATCH,PXDIM,PYDIM,SCRTCH,XDIM,
     : YDIM,XLO,YLO)
*    Description :
*     The array containing the patch is loaded with the data
*     that was originally in the area now covered by the
*     patch.
*    Invocation :
*     CALL KFH_REPLC(PATCH,PXDIM,PYDIM,SCRTCH,XDIM,YDIM,XLO,
*    : YLO)
*    Parameters :
*     PATCH(XDIM,YDIM) = INTEGER
*           The array containing the patch.
*     PXDIM = INTEGER
*           The x dimension of the patch.
*     PYDIM = INTEGER
*           The y dimension of the patch.
*     SCRTCH(XDIM,YDIM) = INTEGER
*           The scaled version of the image used for display.
*     XDIM = INTEGER
*           The x dimension of the image.
*     YDIM = INTEGER
*           The y dimension of the image.
*     XLO = INTEGER
*           The x coordinate of the bottom left hand corner of
*           the patch.
*     YLO = INTEGER
*           The y coordinate of the bottom left hand corner of
*           the patch.
*    Method :
*     The data is copied straight across from the original
*     image data to the patch, bearing in mind that they
*     are both inverted with respect to the raw image data.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     23 August 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Local variables :
      INTEGER PXDIM			! The x dimension of
*					! the patch.
      INTEGER PYDIM			! The y dimension of
*					! the patch.
      INTEGER PATCH(PXDIM,PYDIM)	! The array containing
*					! the patch.
      INTEGER XDIM			! The x dimension of
*					! the image.
      INTEGER YDIM			! The y dimension of
*					! the image.
      INTEGER SCRTCH(XDIM,YDIM)		! The array containing
*					! the scaled image.
      INTEGER XLO			! The x coordinate of
*					! the bottom left hand
*					! corner of the patch.
      INTEGER YLO			! The y coordinate of
*					! the bottom left hand
*					! corner of the patch.
      INTEGER I				! General variable.
      INTEGER J				! General variable.
*-

      DO I = 1,PYDIM,1
         DO J = 1,PXDIM,1
            PATCH(J,I) = SCRTCH(J+XLO-1,YDIM-YLO-PYDIM+I+1)
         ENDDO
      ENDDO

      END
