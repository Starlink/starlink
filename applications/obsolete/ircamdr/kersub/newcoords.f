*+  NEWCOORDS - calculates transformed coordinates after a rotation

      SUBROUTINE NEWCOORDS ( X, Y, CANGLE, SANGLE, XP, YP, STATUS )

*    Description :
*
*     This subroutine takes the input distances X and Y and
*     rotates them through ANGLE degrees clockwise (as given
*     by the sine and cosine SANGLE and CANGLE) to their new
*     values XP and YP using a rotation matrix.
*
*    Invocation :
*
*     CALL NEWCOORDS( X, Y, CANGLE, SANGLE, XP, YP, STATUS )
*
*    Method :
*
*     Just uses a standard rotation matrix approach to calculate the
*     transformed distances of a point from the origin 0.0 after an
*     angular rotation of ANGLE degrees clockwise i.e.
*
*               (  cos ANGLE  sin ANGLE  ) ( X )     ( X' )
*               (                        ) (   )  =  (    )
*               ( -sin ANGLE  cos ANGLE  ) ( Y )     ( Y' )
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
*     14-11-1985 :  First implementation for ROTSIZE
*                :  (REVA::MJM)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      REAL
     :    X,          ! x distance of point from origin 0.0
     :    Y,          ! y     "     "   "     "     "    "
     :    CANGLE,     ! cosine of the rotation angle to be applied
     :    SANGLE      ! sine of the rotation angle to be applied

*    Export :

      REAL
     :    XP,         ! x distance of point from origin after rotation
     :    YP          ! y     "     "   "     "     "     "       "

*    Status :

      INTEGER  STATUS             ! global status parameter

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    just apply the standard rotation matrix, noting that the rotation
*    is clockwise

      XP  =  ( X * CANGLE ) + ( Y * SANGLE )
      YP  =  ( Y * CANGLE ) - ( X * SANGLE )


*    that's it

      END
