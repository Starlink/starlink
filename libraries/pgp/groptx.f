      INTEGER FUNCTION GROPTX (UNIT, NAME, DEFNAM, MODE)
*+
*     - - - - - - - -
*       G R O P T X      (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*  Open input/output text file
*
*  Given:
*     UNIT      i    Fortran unit number to use
*     NAME      c    Name of file to create
*     DEFNAM    c    Default file name (used to fill in missing fields for VMS)
*     MODE      i    0 to open for reading, 1 to open for writing.
*
*  Returns:
*     0 => success; any other value => error.
*
*   D.L.Terrett  Starlink  Sep 1993
*-
      IMPLICIT NONE
      INTEGER UNIT, MODE
      CHARACTER*(*) NAME, DEFNAM

      INTEGER IER
      IF (MODE.EQ.1) THEN
          OPEN (UNIT=UNIT, FILE=NAME, STATUS='UNKNOWN', IOSTAT=IER)
      ELSE
          OPEN (UNIT=UNIT, FILE=NAME, STATUS='OLD', IOSTAT=IER)
      END IF
      GROPTX = IER
      END
