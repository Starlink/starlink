*+  KFH_LINSET - Calculates the X,Y positions of points
*                ready for interpolation.
      SUBROUTINE KFH_LINSET(X1,Y1,X2,Y2,LINDAT,NPTS)
*    Description :
*     This routine calculates the X,Y positions of points
*     at radii of integral pixel spacings from the first
*     point.
*    Invocation :
*     CALL KFH_LINSET(X1,Y1,X2,Y2,LINDAT,NPTS)
*    Parameters :
*     X1 = REAL
*           The X-coordinate of the first point from
*           which the slice will be taken.
*     Y1 = REAL
*           The Y-coordinate of the first point from
*           which the slice will be taken.
*     X2 = REAL
*           The X-coordinate of the second point to
*           which the slice will be taken.
*     Y2 = REAL
*           The Y-coordinate of the second point to
*           which the slice will be taken.
*     LINDAT(0:1023,2) = REAL
*           The array which will contain the X,Y
*           positions of points.
*     NPTS = INTEGER
*           The number of points in the array.
*    Method :
*     The arc tangent of the difference between the
*     Y-coordinates divided by the difference between
*     the X-coordinates is calculated. The sine and
*     cosine of this result is then found. The X,Y
*     positions of points at radii of integral pixel.
*     spacings from the first point are then calculated
*    Author :
*     C.D.Pike (and others)
*     S.Chan
*    History :
*     23 February 1981
*     26 September 1983
*    Type Definitions :
      IMPLICIT NONE
*    Local variables :
      REAL A                             ! The difference between the
*                                        ! Y-coordinates.
      REAL B                             ! The difference between the
*                                        ! X-coordinates.
      REAL CT                            ! Cosine of the THETA.
      REAL EXPR                          ! The sum of the squares of the
*                                        ! differences between the
*                                        ! X-coordinates and the differences
*                                        ! between the Y-coordinates.
      INTEGER I                          ! General variable.
      REAL LINDAT(0:1023,2)              ! The array which is to contain
*                                        ! the X,Y positions of points
*                                        ! for interpolation.
      INTEGER NPTS                       ! The number of X,Y positions of
*                                        ! points for interpolation.
      REAL ST                            ! Sine of the THETA.
      REAL SEP                           ! The square root of EXPR.
      REAL THETA                         ! The angle calculated from taking
*                                        ! the arc tan of the differences
*                                        ! between the X-coordinates and
*                                        ! the Y-coordinates.
      REAL X1                            ! The X-coordinate of the first
*                                        ! point about which the slice is
*                                        ! to be taken.
      REAL X2                            ! The Y-coordinate of the first
*                                        ! point about which the slice is
*                                        ! to be taken.
      REAL Y1                            ! The X-coordinate of the second
*                                        ! point.
      REAL Y2                            ! The Y-coordinate of the second
*                                        ! point.
*-

*
*    Calculate the X,Y positions of points at radii of
*    integral pixel spacings from the first point.
*

      A = Y2-Y1
      B = X2-X1
      THETA = ATAN2(A,B)
      CT = COS(THETA)
      ST = SIN(THETA)
      EXPR = (X2-X1)**2 + (Y2-Y1)**2
      SEP = SQRT(EXPR)
      NPTS = NINT(SEP)

      DO I = 0,NPTS-1

        LINDAT(I,1) = I*CT + X1
        LINDAT(I,2) = I*ST + Y1

      ENDDO

      END
