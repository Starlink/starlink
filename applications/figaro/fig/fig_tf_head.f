C+
C                F I G _ T E X T _ F I L E _ H E A D E R
C
C  Routine name:
C     FIG_TEXT_FILE_HEADER
C
C  Function:
C     Writes some useful information at the top of a FIGARO text file.
C
C  Description:
C     This routine writes a brief header in a text file. Its purpose is
C     to provide a standard format for FIGARO text files so that the
C     reader will know which FIGARO program created the file, the date
C     and time when it was created, and the command line that was used to
C     invoke the FIGARO program.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_TEXT_FILE_HEADER (LU, PROGRAM)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LU        (integer, ref) The logical unit attached to the text file.
C     (>) PROGRAM   (character, desc) The name of the main PROGRAM.
C
C  Prior requirements:
C     The calling program must have opened a text file for output. The
C     file should have carriage-return carriage control. The easiest
C     way to do this is by calling DSA_OPEN_TEXT_FILE.
C
C  Support: Keith Shortridge, AAO
C
C  Author:  Michael Ashley, AAO
C
C  History:
C     30-Aug-1988  Original version. MCBA / AAO.
C     06-Oct-1992  HME / UoE, Starlink.  Uppercase code, no length
C                  specifiers (*4). Use PSX_ routines to get time rather
C                  than VAX specifics. CLINE now is only the programme
C                  name. Change variable name PROGRAM to APPLIC.
C+
C
      SUBROUTINE FIG_TEXT_FILE_HEADER (LU, APPLIC)
C
      IMPLICIT NONE
C
C  Arguments
C
      INTEGER LU
      CHARACTER APPLIC*(*)
C
C  Local variables
C
      INTEGER NTICKS
      INTEGER IGNORE
      CHARACTER TIME*32
C
      IGNORE = 0
      CALL PSX_TIME( NTICKS, IGNORE )
      CALL PSX_CTIME( NTICKS, TIME, IGNORE )
      IF ( IGNORE .NE. 0 ) CALL ERR_ANNUL( IGNORE )
C
      WRITE (LU,100,ERR=200) APPLIC, TIME
C
100   FORMAT (
     : 'This file contains output from the FIGARO program ',A,'.'/
     : 'It was created on ',A)
C
200   CONTINUE
      END
