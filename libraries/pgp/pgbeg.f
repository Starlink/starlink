C*PGBEG -- begin PGPLOT, open output device
C%int cpgbeg(int unit, const char *file, int nxsub, int nysub);
C+
      INTEGER FUNCTION PGBEG (UNIT, FILE, NXSUB, NYSUB)
      INTEGER       UNIT
      CHARACTER*(*) FILE
      INTEGER       NXSUB, NYSUB
C
C Begin PGPLOT, open the plot file.  A call to PGBEG is
C required before any other calls to PGPLOT subroutines.  If a plot
C file is already open for PGPLOT output, it is closed before the new
C file is opened.
C
C Returns:
C  PGBEG         : a status return value. A value of 1 indicates
C                    successful completion, any other value indicates
C                    an error. In the event of error a message is
C                    written on the standard error unit.
C                    To test the return value, call
C                    PGBEG as a function, eg IER=PGBEG(...); note
C                    that PGBEG must be declared INTEGER in the
C                    calling program.
C Arguments:
C  UNIT  (input)   : this argument is ignored by PGBEG (use zero).
C  FILE  (input)   : the "device specification" for the plot device.
C                    Device specifications are installation dependent,
C                    but usually have the form "device/type" or
C                    "file/type". If this argument is a
C                    question mark ('?'), PGBEG will prompt the user
C                    to supply a string. If the argument is a blank
C                    string (' '), PGBEG will use the value of
C                    environment variable PGPLOT_DEV.
C  NXSUB  (input)  : the number of subdivisions of the view surface in
C                    X (>0 or <0).
C  NYSUB  (input)  : the number of subdivisions of the view surface in
C                    Y (>0).
C                    PGPLOT puts NXSUB x NYSUB graphs on each plot
C                    page or screen; when the view surface is sub-
C                    divided in this way, PGPAGE moves to the next
C                    panel, not the  next physical page. If
C                    NXSUB > 0, PGPLOT uses the panels in row
C                    order; if <0, PGPLOT uses them in column order.
C--
C 21-Dec-1995 [TJP] - changed for multiple devices; call PGOPEN [TJP].
C-----------------------------------------------------------------------
      INTEGER       IER
      INTEGER       PGOPEN
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
C Close the plot-file if it is already open.
C
      CALL PGEND
C
C Call PGOPEN to open the device.
C
      IER = PGOPEN(FILE)
      IF (IER.GT.0) THEN
         CALL PGSUBP(NXSUB, NYSUB)
         PGBEG = 1
      ELSE
         PGBEG = IER
      END IF
C
      RETURN
      END
