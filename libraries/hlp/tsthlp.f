      PROGRAM TSTHLP
*+
*  - - - - - - -
*   T S T H L P
*  - - - - - - -
*
*  Testbed for hlp_HELP routine.
*
*  I/O unit:
*     LULIB   input    help library
*
*  Called:  hlp_HELP, hlp_ERRMES and the user-replaceable
*           routines hlp_INSUB, hlp_OUTSUB and hlp_NAMETR
*
*  P.T.Wallace   Starlink   13 June 1995
*-

      IMPLICIT NONE

*  I/O unit number and filename
      INTEGER LULIB
      CHARACTER*80 LIB

*  External routines, to output a line of help text, to read the
*  response, and to translate a library name into a filename.
      INTEGER hlp_OUTSUB,hlp_INSUB,hlp_NAMETR
      EXTERNAL hlp_OUTSUB,hlp_INSUB,hlp_NAMETR

*  Maximum length of help lines to be output
      INTEGER LOUT
      PARAMETER (LOUT=79)

*  Buffer to receive lines of help text
      CHARACTER*(LOUT) INLINE

*  Error message string
      CHARACTER MES*50

*  Interactive help routine
      INTEGER hlp_HELP

*  Status
      INTEGER JSTAT

*  Length of input response string.
      INTEGER L



*-----------------------------------------------------------------------

*  Possibly machine-dependent setup

*  I/O unit number
      LULIB=1

*-----------------------------------------------------------------------

*  Make an announcement.
      JSTAT=hlp_OUTSUB(' ')
      JSTAT=hlp_OUTSUB(
     : 'This is the TSTHLP program, which runs a Portable Help session')
      JSTAT=hlp_OUTSUB(
     : 'using a nominated library.  To leave TSTHLP, enter a period at')
      JSTAT=hlp_OUTSUB(
     : 'the colon prompt.')
      JSTAT=hlp_OUTSUB(' ')
      JSTAT=hlp_OUTSUB(
     : 'Please note that TSTHLP is merely a simple demonstration, not')
      JSTAT=hlp_OUTSUB(
     : 'a fully-fledged help utility!  It lacks sophisticated,')
      JSTAT=hlp_OUTSUB(
     : 'platform-specific features such as screen management, quick')
      JSTAT=hlp_OUTSUB(
     : 'exits via control characters, and so on.  These capabilities')
      JSTAT=hlp_OUTSUB(
     : 'are provided by the various application packages which use')
      JSTAT=hlp_OUTSUB(
     : 'the Portable Help system.')
      JSTAT=hlp_OUTSUB(' ')

*  Get the name of the help library.
      JSTAT=hlp_INSUB(LIB,'Name of help library? ',L)

*  Initialize the command string to spaces.
      INLINE=' '

*  Loop until bad status or terminate request (period).
      DO WHILE (JSTAT.EQ.1.AND.INLINE.NE.'.')

*     Get a command line.
         JSTAT=hlp_INSUB(INLINE,': ',L)

*     Examine the status.
         IF (JSTAT.NE.1) THEN

*        Bad status: issue error report.
            CALL hlp_ERRMES(JSTAT,MES)
            JSTAT=hlp_OUTSUB(MES)

*     Period?
         ELSE IF (INLINE.NE.'.') THEN

*        No: begin an interactive help session.
            JSTAT=hlp_HELP(hlp_OUTSUB,LOUT,INLINE,LULIB,LIB,1,hlp_INSUB,
     :                                                       hlp_NAMETR)

*        If bad status issue error report.
            IF (JSTAT.NE.1) THEN
               CALL hlp_ERRMES(JSTAT,MES)
               JSTAT=hlp_OUTSUB(MES)
            END IF
         END IF
      END DO

      END
