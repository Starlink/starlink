*+  KFH_INTERPOLATE - Interpolates X,Y pairs of points.
      SUBROUTINE KFH_INTERPOLATE(LINDAT,NPTS,DATA,NX,NY,LINE)
*    Description :
*     This routine interpolates the X,Y pairs of points generated
*     by the KFH_LINSET subroutine. Each point is interpolated
*     and stored in an array which can then be used for plotting.
*    Invocation :
*     CALL KFH_INTERPOLATE(LINDAT,NPTS,DATA,NX,NY,LINE)
*    Parameters :
*     LINDAT(0:1023,2) = REAL
*           This array contains the X,Y pairs of points to
*           be interpolated.
*     NPTS = INTEGER
*           The number of X,Y points to be interpolated.
*     DATA(0:NX-1,0:NY-1) = REAL
*           The array containing the image data.
*     NX = INTEGER
*           The X-dimension of the image.
*     NY = INTEGER
*           The Y-dimension of the image.
*     LINE(0:1023) = REAL
*           The array which will contain the interpolated
*           values ready for plotting.
*    Method :
*     For each point to be interpolated the four surrounding
*     points in the data or image array are obtained. The
*     fractional pixel displacements of the interpolation
*     point are also calculated. Bilinear interpolation is
*     then applied. The results are stored in an array which
*     can then be used for plotting.
*    Authors :
*     C.D.Pike (and others)
*     S.Chan
*    History :
*     23 February 1981
*     26 September 1983
*    Type Definitions :
      IMPLICIT NONE
*    Local variables :
      INTEGER NX                         ! The X-dimension of the image.
      INTEGER NY                         ! The Y-dimension of the image.
      REAL DATA(0:NX-1,0:NY-1)           ! Array containing the image
*                                        ! data.
      REAL F                             ! Fractional pixel displacement
*                                        ! of the interpolation point.
      REAL G                             ! Fractional pixel displacement
*                                        ! of the interpolation point.
      INTEGER I                          ! General variable.
      INTEGER IX1                        ! General variable.
      INTEGER IX2                        ! General variable.
      INTEGER IY1                        ! General variable.
      INTEGER IY2                        ! General variable.
      REAL LINDAT(0:1023,2)              ! Array containing the X,Y pairs
*                                        ! of points for interpolation.
      REAL LINE(0:1023)                  ! Array containing the interpolated
*                                        ! values.
      INTEGER NPTS                       ! The number of X,Y pairs of points.
      REAL VAL1                          ! Bottom left point surrounding
*                                        ! the point to be interpolated.
      REAL VAL2                          ! Bottom right point.
      REAL VAL3                          ! Top left point.
      REAL VAL4                          ! Top right point.
*-

*
*    LINDAT contains the X,Y pairs of points to be interpolated.
*

      DO I = 0,NPTS-1

          IX1 = LINDAT(I,1)
          IY1 = LINDAT(I,2)
          IX2 = IX1 + 1
          IY2 = IY1 + 1

*
*       Get the four surrounding points in the data array.
*

         VAL1 = DATA(IX1,IY1)
         VAL2 = DATA(IX2,IY1)
         VAL3 = DATA(IX1,IY2)
         VAL4 = DATA(IX2,IY2)

*
*       F & G are the fractional pixel displacements of the
*       interpolation point.
*

         F = LINDAT(I,1) - IX1
         G = LINDAT(I,2) - IY1

*
*       On exit , LINE will contain the array of interpolated
*       values ready for plotting. Bilinear interpolation is used.
*

         LINE(I) = F*(VAL2-VAL1) + F*G*(VAL1+VAL4-VAL2-VAL3)
     :             + G*(VAL3-VAL1) + VAL1

      ENDDO

      END
