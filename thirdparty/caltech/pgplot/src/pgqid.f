C*PGQID -- inquire current device identifier
C%void cpgqid(int *id);
C+
      SUBROUTINE PGQID (ID)
      INTEGER  ID
C
C This subroutine returns the identifier of the currently
C selected device, or 0 if no device is selected.  The identifier is
C assigned when PGOPEN is called to open the device, and may be used
C as an argument to PGSLCT.  Each open device has a different
C identifier.
C
C [This routine was added to PGPLOT in Version 5.1.0.]
C
C Argument:
C  ID     (output) : the identifier of the current device, or 0 if
C                    no device is currently selected.
C--
C 22-Dec-1995 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
C
      ID = PGID
      END
