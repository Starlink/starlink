      SUBROUTINE PAR_COMSTR (STRING)
C+
C                         P A R _ C O M S T R
C
C  Routine name:
C     PAR_COMSTR
C
C  Description:
C     A Figaro application routine may call this subroutine to
C     obtain the whole of the command string - excluding the
C     actual command name.  Since Figaro applications are all
C     supposed to use the PAR_ routines to parse the command
C     string, this is not a routine intended for general use.
C     It was originally written specially for the LET command.
C     This version has been modified for use in an ADAM environment
C     and has to treat the command as a character string, eg
C     LET "FILE1.X=FILE2.Z"
C
C  Language:
C     FORTRAN
C
C  Parameters:      ("<" output)
C     (<) STRING    (Fixed string, descr) The command string, as
C                   described above.
C
C  External variables used -  None
C
C  External subroutines / functions used -
C
C     PAR_DEF0C   (ADAM PAR_ routine) Set character parameter default
C     PAR_GET0C   ( "    "      "   ) Get character parameter
C
C  Author: Keith Shortridge, AAO
C
C  Date: 10th April 1987
C
C  Internal declaration:
C     SUBROUTINE PAR_COMSTR(STRING)
C     CHARACTER*(*) STRING
C-
C
C  Modified:
C     25th Nov 1983.  KS / CIT. Original version.
C     2nd  May 1986.  KS / AAO. Now gets string from common, rather than
C                     through LIB$GET_FOREIGN.
C     10th April 1987 KS / AAO. Modified for use with ADAM.
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) STRING
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'PARBLK'
C
C     Local variables
C
      INTEGER STATUS
C
      IF ( ABORT ) RETURN
C
      STATUS=SAI__OK
      CALL PAR_DEF0C('COMMAND_STRING',' ',STATUS)
      CALL PAR_GET0C('COMMAND_STRING',STRING,STATUS)
C
      IF ( STATUS .NE. SAI__OK ) ABORT = .TRUE.
C
      END
