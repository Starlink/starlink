C*GROPTX -- open input/output text file [Unix]
C+
      INTEGER FUNCTION GROPTX (UNIT, NAME, DEFNAM, MODE)
      INTEGER UNIT, MODE
      CHARACTER*(*) NAME, DEFNAM
C
C Input:
C  UNIT : Fortran unit number to use
C  NAME : name of file to create
C  DEFNAM : default file name (used to fill in missing fields for VMS)
C  MODE : 0 to open for reading, 1 to open for writing.
C
C Returns:
C  0 => success; any other value => error.
C-----------------------------------------------------------------------
      INTEGER IER
      IF (MODE.EQ.1) THEN
          OPEN (UNIT=UNIT, FILE=NAME, STATUS='UNKNOWN', IOSTAT=IER)
      ELSE
          OPEN (UNIT=UNIT, FILE=NAME, STATUS='OLD', IOSTAT=IER)
      END IF
      GROPTX = IER
C-----------------------------------------------------------------------
      END
