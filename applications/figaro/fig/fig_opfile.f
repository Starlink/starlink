C+
      SUBROUTINE FIG_OPFILE (FILE,DEFAULT,LU,STATUS)
C
C     F I G _ O P F I L E
C
C     Opens an input file for a Figaro routine.  This routine is
C     designed for files such as calibration files, mask tables, etc.
C     - files that might normally be provided as part of the system,
C     but which may also be provided by the user.  The routine will
C     search six directories for the file, in the following order -
C     1) The current default directory
C     2) The user's Figaro directory (FIGARO_PROG_U)
C     3) The directory holding the current .EXE image.
C     4) The local Figaro directory (FIGARO_PROG_L)
C     5) The national Figaro directory (FIGARO_PROG_N)
C     6) The system Figaro directory (FIGARO_PROG_S)
C
C     Parameters -   (">" input, "<" output)
C
C     (>) FILE     (Character) The name of the file.  This may include
C                  a directory specification, in which case that will
C                  be the only directory the routine will look in.  It
C                  may also include an extension.  FILE may be padded
C                  with blanks at the end if necessary.
C     (>) DEFAULT  (Character) The default extension to use if FILE does
C                  not specify one explicitly.
C     (>) LU       (Integer) The logical unit to open the file on.
C     (<) STATUS   (Integer) Return status code.  0 => open OK,
C                  non-zero => failed to open file.  Note that the file
C                  will be opened 'readonly'.
C
C                                            KS / CIT 5th April 1984
C     Modified:
C
C     7th Sept 1988  Search path modified to include .EXE and
C                    national directories.  KS/AAO.
C     4th Sept 1992  Re-written as simple version of DSA_OPEN_TEXT_FILE.
C                    HME/UoE, Starlink.
C
C     Note:  Ideally, this routine would use DSA_OPEN_TEXT_FILE, but
C     it cannot because it is passed the logicl unit to use.  New
C     programs should use DSA_OPEN_TEXT_FILE in preference to this
C     routine, and should let it supply the logical unit.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER LU,STATUS
      CHARACTER*(*) FILE,DEFAULT
C
C     Local variables
C
      LOGICAL   DID_EXIST        ! True if the file did exist
      INTEGER   I, IDIR          ! Loop index and break out index
      CHARACTER ERROR*64         ! Error text
      CHARACTER LSTRING*32       ! Local string variable
C
C     Symbolic names of directories to search. ('$' is used by
C     DSAZ_TFOPEN to indicate the .EXE directory), and the blank string
C     will have the effect of referring to the default directory.
C
      CHARACTER PREFIX(6)*13
      DATA PREFIX/'             ','FIGARO_PROG_U','$            ',
     :            'FIGARO_PROG_L','FIGARO_PROG_N','FIGARO_PROG_S'/
C
C     Reset status.
C
      STATUS = 0
C
C     The file should exist, we have to look for it.  We try the
C     various directories one by one.  If we actually fail to
C     open an existing file, we exit the loop.
C
      LSTRING='.'//DEFAULT
      DO I = 1,6
         STATUS=0
         CALL DSAZ_TFOPEN (LU,PREFIX(I),FILE,LSTRING,
     :      .FALSE.,.FALSE.,DID_EXIST,ERROR,STATUS)
         IF (STATUS.EQ.0.OR.DID_EXIST) THEN
            IDIR=I
            GO TO 320    ! Break out of I loop
         END IF
      END DO
  320 CONTINUE
C
C     Exit.
C
  500 CONTINUE
C
      END
