*+  KFH_NWSLICE - Calculates the slice between two points.
      SUBROUTINE KFH_NWSLICE(X1,Y1,X2,Y2,DATA,NX,NY,LINE,NPTS,
     :                       STATUS)
*    Description :
*     This subroutine calculates the slice through an image.
*     The slice taken is that between the two points given
*     by the user , the points being the end points.
*     Interpolation is used to ensure that the spacing of
*     samples in the slice is always at the pixel spacing ,
*     irrespective of the angle.
*    Invocation :
*     CALL KFH_NWSLICE(X1,Y1,X2,Y2,DATA,NX,NY,LINE,NPTS,STATUS)
*    Parameters :
*     X1 = INTEGER
*           The X-coordinate of the first end-point of
*           the slice.
*     Y1 = INTEGER
*           The Y-coordinate of the first end-point of
*           the slice.
*     X2 = INTEGER
*           The X-coordinate of the second end-point of
*           the slice.
*     Y2 = INTEGER
*           The Y-coordinate of the second end-point of
*           the slice.
*     DATA(0:NX-1,0:NY-1) = REAL
*           The array holding the image data.
*     NX = INTEGER
*           The X-dimension of the image.
*     NY = INTEGER
*           The Y-dimension of the image.
*     LINE(0:1023) = REAL
*           The array holding the interpolated points
*           of the slice.
*     NPTS = INTEGER
*           The number of points to be interpolated.
*     STATUS = INTEGER
*           The status value on entry to this subroutine.
*    Method :
*     The X,Y positions of the points at the radii of
*     integral pixel spacings from the first point is
*     determined. These points are then interpolated
*     giving an array of data ready for plotting.
*    Authors :
*     C.D.Pike (and others)
*     S.Chan
*    History :
*     23 February 1981
*     26 September 1983
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NX                         ! X-dimension of the image.
      INTEGER NY                         ! Y-dimension of the image.
      REAL DATA(0:NX-1,0:NY-1)           ! Array containing the image data.
      REAL LINDAT(0:1023,2)              ! Array containing the X,Y pairs
*                                        ! of points to be interpolated.
      REAL LINE(0:1023)                  ! Array holding the interpolated
*                                        ! values ready for plotting.
      INTEGER NPTS                       ! The number of points to be
*                                        ! interpolated.
      REAL X1                            ! X-coordinate of the first
*                                        ! end-point of the slice.
      REAL X2                            ! X-coordinate of the second
*                                        ! end-point of the slice.
      REAL Y1                            ! Y-coordinate of the first
*                                        ! end-point of the slice.
      REAL Y2                            ! Y-coordinate of the second
*                                        ! end-point of the slice.
*-

*
*    If the status is bad, then return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Compute the slice between the specified points.
*

         CALL KFH_LINSET(X1,Y1,X2,Y2,LINDAT,NPTS)
         CALL KFH_INTERPOLATE(LINDAT,NPTS,DATA,NX,NY,LINE)

      ENDIF

      END
