
*+  KFH_RDBOX - Routine to sample cursor and trackerball box.
      SUBROUTINE KFH_RDBOX(BUTTON,NEWX,NEWY,WKID)
*    Description :
*     This subroutine detects which (if any) of the four
*     buttons on the trackerball box has been pressed and
*     returns a map of the buttons in the array BUTTON where
*     True means the button was pressed and False means that
*     it was not. This routine also samples the ARGS to find
*     the current cursor position.
*    Invocation :
*     CALL KFH_RDBOX(BUTTON,NEWX,NEWY,WKID)
*    Parameters :
*     BUTTON(4) = _LOGICAL		! Map of buttons.
*     NEWX = _REAL			! X-coordinate of
*					! present cursor
*					! position.
*     NEWY = _REAL			! Y-coordinate of
*					! present cursor
*					! position.
*     WKID = _INTEGER			! Work station
*					! identification.
*    Method :
*     This routine is almost self-explanatory. The cursor
*     position is sampled by the routine SGS_SAMCU and the
*     box is read by the routine GKS_SMCH.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     18 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Local variables :
      LOGICAL BUTTON(4)			! Map of buttons.
      INTEGER I				! General variable.
      REAL NEWX				! X-coordinate of cursor
*					! position.
      REAL NEWY				! Y-coordinate of cursor
*					! position.
      INTEGER WKID			! Work station
*					! identifier.
*-

*
*    Get the position of the cursor in world coordinates.
*

      CALL SGS_SAMCU(NEWX,NEWY)

*
*    Reset buttons map.
*

      DO I = 1,4,1
         BUTTON(I) = .FALSE.
      ENDDO

*
*    Test to see which button has been pressed. The number
*    of the pressed button is returned in I. If no button
*    has been pressed then I is zero.
*
*    The button numbers are:
*    1    Green - Reset table
*    2    White - Advance to next colour set
*    3    White - Not used
*    4    Red   - Terminate program
*

      CALL GKS_SMCH(WKID,1,I)

      IF (I.NE.0) THEN

*
*       Update button map.
*

         BUTTON(I) = .TRUE.

*
*       Clear button, ready for next read.
*

         CALL GKS_DSCH(WKID,1)
         CALL GKS_ENCH(WKID,1,2)

      ENDIF

*
*    Empty all input and output buffers.
*

      CALL SGS_FLUSH

      END
