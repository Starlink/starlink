      PROGRAM LSTHLP
*+
*  - - - - - - -
*   L S T H L P
*  - - - - - - -
*
*  List a Starlink HLP help library file for diagnostic purposes.
*
*  I/O unit:
*     LULIB   input    help library file (direct-access form)
*
*  Called:  hlp_HINIT, hlp_HOPENR, hlp_HDREAD, hlp_LENGTH, hlp_ERRMES
*  and the user-supplied hlp_NAMETR, hlp_INSUB and hlp_OUTSUB.
*
*  P.T.Wallace   Starlink   9 September 1995
*-

      IMPLICIT NONE

*  Help library name to filename translation routine
      EXTERNAL hlp_NAMETR

*  I/O unit number and filename
      INTEGER LULIB
      CHARACTER LIB*80

*  Buffers
      CHARACTER B*200,LINE*209

*  End-of-string character
      CHARACTER EOS

      INTEGER hlp_INSUB,hlp_OUTSUB,hlp_LENGTH

      INTEGER J,L,JSTAT,I,IW,NC

*-----------------------------------------------------------------------

*  Machine-dependent setup

*  I/O unit number
      LULIB=1

*  Form the "end of string" character
      EOS=CHAR(0)                         !!! ASCII-specific !!!

*-----------------------------------------------------------------------

*  Get the help library name.
      J=hlp_INSUB(LIB,'Name of help library to be read? ',L)

*  Open the help library file.
      CALL hlp_HINIT(LULIB,LIB(:L),EOS)
      CALL hlp_HOPENR(hlp_NAMETR,JSTAT)
      IF (JSTAT.NE.0) GO TO 9000

*  Initialize the starting address.
      I=0

*  Loop while OK status.
      DO WHILE (JSTAT.EQ.0)

*     Save the current address for the report.
         IW=I

*     Read a record.
         CALL hlp_HDREAD(I,B,NC,JSTAT)

*     Report the record, unless there is a fatal error.
         IF (JSTAT.EQ.0) THEN
            WRITE (LINE,'(I6,3X,A)') IW,B(:NC)
            J=hlp_OUTSUB(LINE(:hlp_LENGTH(LINE)))
         ELSE IF (JSTAT.LT.0.AND..NOT.(JSTAT.EQ.-7.AND.NC.EQ.0)) THEN
            GO TO 9000
         END IF
      END DO
      GO TO 9999

*  Errors
 9000 CONTINUE
      CALL hlp_ERRMES(JSTAT,LINE)
      J=hlp_OUTSUB(LINE(:hlp_LENGTH(LINE)))

*  Exit
 9999 CONTINUE

      END
