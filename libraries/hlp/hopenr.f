      SUBROUTINE hlp_HOPENR (NAMETR, J)
*+
*  - - - - - - -
*   H O P E N R
*  - - - - - - -
*
*  Open or re-open the help library file for reading.
*
*  Specified in INCLUDE:
*     LHBUF     i       length of help buffer
*
*  Given (argument):
*     NAMETR    x       user-supplied name translation routine (note 1)
*
*  Given (in COMMON):
*     JHELP     i       state of HLP system: -1=initialized
*     LUHL      i       unit number for help library file
*     HLNEXT    c*()    name of help library
*     LOFFNU    i       logical level for next help library
*
*  Returned (in COMMON):
*     JHELP     i       state of HLP system: 2=open/read
*     HLOPEN    c*()    name of open help library
*     NEXTX     i       index address for 1st hlp_HREADX access
*     NEXTD     i       data address for 1st hlp_HREAD access
*     LEVOFF    i       logical level for open help library
*     IHBUF     i       address of HBUF within help file
*     NCHH      i       size of file (characters)
*
*  Returned (argument)
*     J         i           status:  0 = OK
*                                   -1 = HLP system in illegal state
*                                   -2 = open error (or bad header)
*                                   -3 = write error
*
*  Notes:
*
*  1)  The user-supplied name translation routine NAMETR is a subroutine
*      subprogram.  It has arguments COMMAND, STRING1, STRING2, STATUS.
*      COMMAND (given) is an integer which the HLP system always sets
*      to zero;  the application may use other values of COMMAND, for
*      example to perform initializations or enquiries.  STRING1 (given)
*      is, for COMMAND=0, the help library name to be translated;  this
*      name can be the value of the LIB argument to the present routine
*      (identifying the root library) or the name following the '@' symbol
*      in a help source file (identifying another library grafted onto
*      the current library).  STRING2 (returned) is, for COMMAND=0, the
*      translation of STRING1 into a filename for use in Fortran OPEN
*      statements.  STATUS (returned) is zero to show success, -16 to
*      show that a string has overflowed and -17 to show any other kind
*      of failure.
*
*  2)  Repeated calls to this routine are permitted during reading
*      without an error status, producing a "rewind" effect.
*
*  3)  NEXTX is initialized to point to the first index record.
*      NEXTD is initialized to point to the final end marker.
*
*  Called:  hlp_FOPR, hlp_DREAD, hlp_DEC
*
*  P.T.Wallace   Starlink   31 August 1995
*-

      IMPLICIT NONE

      EXTERNAL NAMETR
      INTEGER J

      INCLUDE 'helpic'

      INTEGER IADR,NC,JSTAT,I,N
      CHARACTER BUFFER*20



*  Check that the system is in the right state.
      IF (JHELP.NE.-1.AND.JHELP.NE.2) GO TO 9000

*  Open (or re-open) the help library file.
      CALL hlp_FOPR(NAMETR,LUHL,HLNEXT,LHBUF,J)
      IF (J.NE.0) GO TO 9010

*  Note its name and logical level.
      HLOPEN=HLNEXT
      LEVOFF=LOFFNU

*  Initialize buffer address within file to force immediate read.
      IHBUF=-1

*  Set HLP state.
      JHELP=2

*  Read the header record to get the file size.
      NCHH=LHBUF
      IADR=0
      CALL hlp_HDREAD(IADR,BUFFER,NC,JSTAT)
      IF (JSTAT.NE.0.OR.NC.LT.1) GO TO 9010
      I=1
      CALL hlp_DEC(BUFFER,I,N)
      IF (N.LT.0) GO TO 9010
      NCHH=N

*  Store the initial index and data addresses.
      NEXTX=IADR
      NEXTD=N-1

*  Set status and exit.
      J=0
      GO TO 9999

*  HLP system in the wrong state
 9000 CONTINUE
      J=-1
      GO TO 9999

*  Error on open
 9010 CONTINUE
      J=-2

*  Exit.
 9999 CONTINUE

      END
