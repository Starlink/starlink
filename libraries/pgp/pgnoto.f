C
      LOGICAL FUNCTION PGNOTO (RTN)
      CHARACTER*(*) RTN
C
C PGPLOT (internal routine): Test whether a PGPLOT device is open and
C print a message if not. Usage:
C     LOGICAL PGNOTO
C     IF (PGNOTO('routine')) RETURN
C
C Arguments:
C
C RTN (input, character): routine name to be include in message.
C
C Returns:
C     .TRUE. if PGPLOT is not open.
C--
C 11-Nov-1994
C 21-Dec-1995 - revised for multiple devices.
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      CHARACTER*80 TEXT
C
      CALL PGINIT
      PGNOTO = .FALSE.
      IF (PGID.LT.1 .OR. PGID.GT.PGMAXD) THEN
         PGNOTO = .TRUE.
         TEXT = RTN//': no graphics device has been selected'
         CALL GRWARN(TEXT)
      ELSE IF (PGDEVS(PGID).NE.1) THEN
         PGNOTO = .TRUE.
         TEXT = RTN//': selected graphics device is not open'
         CALL GRWARN(TEXT)
      END IF
      RETURN
      END
