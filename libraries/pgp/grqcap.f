      SUBROUTINE GRQCAP (STRING)
*+
*     - - - - - - - -
*       G R Q C A P      (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Inquire device capabilities
*
*   Returns the "device capabilities" string for the current device.
*
*   Outputs:
*
*       STRING    c    receives the device capabilities string.
*
*   The fields are defined as follows:
*
*     STRING(1:1) = `H' if the device is a hardcopy device, `I' if it is an
*     interactive device. On an interactive device, the image is visible as
*     it is being drawn, while on a hardcopy device it cannot be viewed until
*     the workstation is closed.
*
*     STRING(2:2) = `C' if a cursor is available, `X' if a cursor is available
*     and opcode 27 is accepted by the driver, `N' if there is no cursor.
*     PGPLOT cannot emulate a cursor if none is available.
*
*     STRING(3:3) = `D' if the hardware can draw dashed lines, `N' if it
*     cannot. PGPLOT emulates dashed lines by drawing line segments. Software
*     emulation is usually superior to hardware dashed lines, and not much
*     slower, so CHR(3:3) = `N' is recommended.
*
*     STRING(4:4) = `A' if the hardware can fill arbitrary polygons with solid
*     color, `N' if it cannot. PGPLOT emulates polygon fill by drawing
*     horizontal or vertical lines spaced by the pen diameter (see OPCODE = 3).
*
*     STRING(5:5) = `T' if the hardware can draw lines of variable width,  N'
*     if it cannot. PGPLOT emulates thick lines by drawing multiple strokes
*     spaced by the pen diameter. Note that thick lines are supposed to have
*     rounded ends, as if they had been drawn by a circular nib of the
*     specified diameter.
*
*     STRING(6:6) = `R' if the hardware can fill rectangles with solid
*     color, `N' if it cannot. If this feature is not available, PGPLOT
*     will treat the rectangle as an arbitrary polygon. In this context, a
*     `rectangle' is assumed to have its edges parallel to the
*     device-coordinate axes.
*
*     STRING(7:7) = `P' if the driver understands the pixel primitives (opcode
*     26), `N' otherwise.
*
*     STRING(8:8) = `V' if PGPLOT should issue an extra prompt to the user
*     before closing the device (in PGEND), `N' otherwise. Use `V' for
*     devices like X-window workstations where the PGPLOT window is deleted
*     from the screen when the device is closed.
*
*     STRING(9:9) = `N' (reserved for future use).
*
*     STRING(10:10) = `M' if the device handler accepts opcode 28 to draw graph
*     markers; `N' otherwise.
*
*     The only fields used by PGPLOT as opposed to GRPCKG is are 2 and 8
*     (as of 5.1.1).
*     For all workstations supported by RAL-GKS the string is set to Ns
*     except for field 2.
*
*   D.L.Terrett  Starlink  Aug 1997
*-
      IMPLICIT NONE
      CHARACTER*(*) STRING

      INCLUDE 'grecom.inc'

      CHARACTER*10 TYPE
      LOGICAL INTER

      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQCAP - no graphics device is active.')
          STRING = 'NNNNNNNNNN'
      ELSE
          STRING = 'NNNNNNNNNN'

*  Inquire if device is interactive
          CALL GRQTYP(TYPE, INTER)
          IF (INTER) STRING(2:2) = 'Y'
      END IF
      END
