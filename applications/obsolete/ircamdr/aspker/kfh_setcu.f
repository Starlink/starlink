*+  KFH_SETCU - Cursor setting routine.
      SUBROUTINE KFH_SETCU(X,Y)
*    Description :
*     This routine places the cursor at the position on the
*     ARGS screen that has world coordinates (x,y).
*    Invocation :
*     CALL KFH_SETCU(X,Y)
*    Parameters :
*     X = _REAL				! X-coordinate for
*					! cursor.
*     Y = _REAL				! Y-coordinate for
*					! cursor.
*    Method :
*     The sample mode for the cursor is disabled (as the
*     cursor setting routine does not function with it
*     enabled), then the SGS routine for set cursors is
*     called up, and finally the sample mode is re-enabled.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     18 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Local variables :
      REAL X				! X-coordinate for
*					! cursor.
      REAL Y				! Y-coordinate for
*					! cursor.
*-

*
*    Disable cursor sample mode.
*

      CALL SGS_DISCU

*
*    Set cursor on screen.
*

      CALL SGS_SETCU(X,Y)

*
*    Re-enable cursor sample mode.
*

      CALL SGS_ENSCU

      END
