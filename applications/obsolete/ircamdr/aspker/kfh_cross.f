*+  KFH_CROSS -Draws a cross at the specified pixel reading.
      SUBROUTINE KFH_CROSS(X,Y,STATUS)
*    Description :
*     This routine accepts the coordinates of a point and
*     draws a cross 5 pixels wide centred at that point.
*    Invocation :
*     CALL KFH_CROSS(X,Y,STATUS)
*    Parameters :
*     X = INTEGER
*           The X-coordinate of the point.
*     Y = INTEGER
*           The Y-coordinate of the point.
*     STATUS = INTEGER
*           The status on entry to this subroutine.
*    Method :
*     The coordinates of the arms of the cross are calculated,
*     and once these have been found , they can be plotted.
*    Authors :
*     S.Chan (RGVAD::KFH)
*    History :
*     26 October 1983:
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER X                          ! The X-coordinate of the
*                                        ! point.
      INTEGER Y                          ! The Y-coordinate of the
*                                        ! point.
      INTEGER LX                         ! The X-coordinate of the
*                                        ! left arm of the cross.
      INTEGER RX                         ! The X-coordinate of the
*                                        ! right arm of the cross.
      INTEGER TY                         ! The Y-coordinate of the
*                                        ! upper arm of the cross.
      INTEGER BY                         ! The Y-coordinate of the
*                                        ! lower arm of the cross.
*-

*
*    If the status is bad on entry, then return to the main
*    program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Calculate the extent of the cross.
*

         LX = X-2
         RX = X+2
         TY = Y+2
         BY = Y-2

*
*       Plot the cross.
*

         CALL SGS_LINE(LX,Y,RX,Y)
         CALL SGS_LINE(X,TY,X,BY)
         CALL SGS_FLUSH

      ENDIF

      END
