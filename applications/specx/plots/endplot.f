*-----------------------------------------------------------------------

      SUBROUTINE ENDPLOT (DEVICE, INTERACTIVE, SENDHARD)

*  Routine to finish off a plot.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   DEVICE
      LOGICAL   INTERACTIVE
      LOGICAL   SENDHARD

*  Ok, go...

*     Switch terminals back to alpha mode

      IF (DEVICE.LT.20) CALL SXGTIDLE

*     For VT240/VT330, do not clear without checking...

      IF (DEVICE.EQ.10 .AND. .NOT.INTERACTIVE) THEN
        CALL SXGCLEAR
      END IF

*     Idle terminal or send hardcopy to device

      IF (DEVICE.LT.20) THEN               ! Terminal: set to idle
        CALL SXGVT220
      ELSE                                 ! Hardcopy: send to device
        IF (SENDHARD) CALL SXGDEVEND
      END IF

      RETURN
      END

*-----------------------------------------------------------------------
