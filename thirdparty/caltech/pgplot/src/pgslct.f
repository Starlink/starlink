C*PGSLCT -- select an open graphics device
C%void cpgslct(int id);
C+
      SUBROUTINE PGSLCT(ID)
      INTEGER ID
C
C Select one of the open graphics devices and direct subsequent
C plotting to it. The argument is the device identifier returned by
C PGOPEN when the device was opened. If the supplied argument is not a
C valid identifier of an open graphics device, a warning message is
C issued and the current selection is unchanged.
C
C [This routine was added to PGPLOT in Version 5.1.0.]
C
C Arguments:
C
C ID (input, integer): identifier of the device to be selected.
C--
C 22-Dec-1995 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      IF (ID.LT.1 .OR. ID.GT.PGMAXD) THEN
         CALL GRWARN('PGSLCT: invalid argument')
      ELSE IF (PGDEVS(ID).NE.1) THEN
         CALL GRWARN('PGSLCT: requested device is not open')
      ELSE
C        -- Select the new device
         PGID = ID
         CALL GRSLCT(PGID)
      END IF
C
      END
