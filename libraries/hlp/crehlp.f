      PROGRAM CREHLP
*+
*  - - - - - - -
*   C R E H L P
*  - - - - - - -
*
*  Create random access form of help file.
*
*
*  I/O units:
*     LUIN    input    help source
*     LUOUT   output   direct access form
*     LUCMD   input    command input
*     LUMES   output   prompts and error messages
*
*  Called:  hlp_NAMETR, hlp_CREH, hlp_ERRMES
*
*  P.T.Wallace   Starlink   13 Junly 1995
*-

      IMPLICIT NONE

*  Help library name to filename translation routine
      EXTERNAL hlp_NAMETR

*  I/O unit numbers and filenames
      INTEGER LUIN,LUOUT,LUCMD,LUMES
      CHARACTER*80 SOURCE,LIB

*  End-of-string character
      CHARACTER EOS

      INTEGER JSTAT
      CHARACTER MES*80


*-----------------------------------------------------------------------

*  Machine-dependent setup

*  I/O unit numbers
      LUIN=1
      LUOUT=2
      LUCMD=5
      LUMES=6

*  Form the "end of string" character
      EOS=CHAR(0)                         !!! ASCII-specific !!!

*  Filenames.
      SOURCE='fort.1'
      LIB='fort.2'

*-----------------------------------------------------------------------

*  Create the help file.
      CALL hlp_CREH(hlp_NAMETR,LUIN,SOURCE,LUOUT,LIB,LUMES,EOS,JSTAT)

*  Report status if bad, and exit.
      IF (JSTAT.NE.0) THEN
         CALL hlp_ERRMES(JSTAT,MES)
         WRITE (LUMES,'(1X,A)') MES
      END IF

      END
