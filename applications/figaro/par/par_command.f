C+
C                       P A R _ C O M M A N D
C  Routine name:
C     PAR_COMMAND
C
C  Function:
C     Returns the name of the current Figaro command.
C
C  Description:
C     Makes the current command name available to a Figaro routine.
C     This is intended for use by Figaro routines such as ADDSUB which
C     service more than one command, and so need to know which command
C     has invoked them.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL PAR_COMMAND (COMMAND)
C
C  Parameters:     (">" input  "<" output)
C     (<) COMMAND  (Fixed string, descr) The current command.  This
C                  is returned blank filled and in upper case.
C
C  Prior requirements:
C     PAR_INIT must have been called (by the Figaro main routine).
C
C  Author: Keith Shortridge, CIT
C          Horst Meyerdierks, UoE, Starlink
C
C  Internal declaration:
C     SUBROUTINE PAR_COMMAND (COMMAND)
C     CHARACTER*(*) COMMAND
C
C  Modified:
C     29th Nov 1982  KS/CIT. Original version.
C     8th Dec 1989   KS/AAO. Comments reformatted.
C     12th Aug 1992  HME/UoE, Starlink. New include file, avoid VAX
C                    specific routine.
C-
      SUBROUTINE PAR_COMMAND (COMMAND)
C
      IMPLICIT NONE
C
C     Parameter
C
      CHARACTER*(*) COMMAND
C
C     Parameter common block
C
      INCLUDE 'PARBLK'
C
      IF ( ABORT ) RETURN
C
      COMMAND = COM
      CALL CHR_UCASE( COMMAND )
C
      END
