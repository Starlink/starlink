
*+  OLDCOORDS - calculates coords of original point before rotation

      SUBROUTINE OLDCOORDS ( X, Y, CANGLE, SANGLE, XP, YP,STATUS )

*    Description :
*
*     This routine takes the real coordinates of a point (X,Y)
*     in an image which has been rotated from the original by ANGLE
*     degrees clockwise, as given by the sine and cosine of the angle,
*     SANGLE and CANGLE ) and returns the real coordinates of the
*     point (XP,YP) from which X,Y was transformed in the rotation.
*
*    Invocation :
*
*     CALL OLDCOORDS( X, Y, CANGLE, SANGLE, XP, YP, STATUS )
*
*    Method :
*
*     Just uses a standard rotation matrix approach to calculate the
*     x and y distances ( XP,YP )of a point from the origin prior to
*     a rotation of ANGLE degrees clockwise from the x and y distances
*     ( X,Y ) of the transformed point from the origin. Obviously may
*     be considered as evaluating the transformed coordinates of a point
*     after an anticlockwise rotation of the same amount. Matrix used is
*     inverse of one used in NEWCOORDS :
*
*         (  cos ANGLE  -sin ANGLE  ) ( X )     ( X' )
*         (                         ) (   )  =  (    )
*         (  sin ANGLE   cos ANGLE  ) ( Y )     ( Y' )
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Konrad Flamm UoE ( REVA::MJM )
*
*    History :
*
*     14-11-1985 :  First implementation for ROTNRS
*                :  (REVA::MJM)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      REAL
     :    X,          ! real x distance of point from
                      ! origin in transformed array
     :    Y,          ! real y distance of point from
                      ! origin in transformed array
     :    CANGLE,     ! cosine of rotation angle applied
     :    SANGLE      ! sine of rotation angle applied

*    Export :

      REAL
     :    XP,         ! real x distance of point from origin in
                      ! original frame
     :    YP          ! real y distance of point from origin in
                      ! original frame

*    Status :

      INTEGER  STATUS             ! global status parameter

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    just evaluate the pre-rotation distances XP,YP from the post-rotation
*    distances X,Y and the clockwise rotation ANGLE using a matrix transform

      XP  =  ( X * CANGLE )  - ( Y * SANGLE )
      YP  =  ( X * SANGLE )  + ( Y * CANGLE )


*    that's it - return.

      END
