      SUBROUTINE hlp_HOPENW (NAMETR, NCHARS, J)
*+
*  - - - - - - -
*   H O P E N W
*  - - - - - - -
*
*  Open the help library file for writing.
*
*  Specified in INCLUDE:
*     LHBUF     i           length of help buffer
*
*  Given (in COMMON):
*     JHELP     i           state of HLP system: -1=initialized
*     LUHL      i           unit number for help library file
*     HLNEXT    c*()        name of help library
*     HEOS      c*1         end-of-string character
*
*  Given (arguments):
*     NAMETR    x           user-supplied name translation routine (note 1)
*     NCHARS    i           size the file will be (characters)
*
*  Returned (in COMMON):
*     JHELP     i           state of HLP system: 1=open/write
*     NEXTX     i           index address for spurious sequential read
*     NEXTD     i           data address for 1st hlp_HDWRIT access
*     IHBUF     i           address of HBUF within help file (reset)
*     NCHH      i           size of file (characters)
*     HLOPEN    c*()        name of help library
*     HBUF      c*(LHBUF)   help buffer (filled with end-of-strings)
*
*  Returned (argument)
*     J         i           status:  0 = OK
*                                   -1 = HLP system in illegal state
*                                   -2 = open error
*                                   -3 = write error
*                                 else = errors from called routines
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
*  2)  NEXTX and NEXTD are initialized to zero (just for tidiness in
*      the case of NEXTX - a subsequent attempt to perform a sequential
*      read with hlp_HREADX will be rejected due to the system being in
*      the wrong state).
*
*  3)  The library name is not used as a filename directly, but is
*      subjected to any required translation by means of the NAMETR
*      routine.
*
*  Called:  NAMETR (user-supplied)
*
*  P.T.Wallace   Starlink   9 September 1995
*-

      IMPLICIT NONE

      EXTERNAL NAMETR
      INTEGER NCHARS,J

      INCLUDE 'helpic'

      INTEGER I,IREC
      CHARACTER FILE*200



*  Check that the system is in the right state.
      IF (JHELP.NE.-1) GO TO 9000

*  Translate the help library name and open the file.
      CALL NAMETR(0,HLNEXT,FILE,J)
      IF (J.NE.0) GO TO 9999
      OPEN (UNIT=LUHL,STATUS='UNKNOWN',ACCESS='DIRECT',RECL=LHBUF,
     :         FORM='FORMATTED',ERR=9010,FILE=FILE)

*  Note its name.
      HLOPEN=HLNEXT

*  Initialize the buffer.
      DO I=1,LHBUF
         HBUF(I:I)=HEOS
      END DO

*  Write every record to the file.
      DO IREC=1,1+NCHARS/LHBUF
         WRITE (UNIT=LUHL,FMT='(A)',REC=IREC,ERR=9020) HBUF
      END DO

*  Initialize buffer address within file to "block unavailable".
      IHBUF=-1

*  Initialize addresses for next sequential access.
      NEXTX=0
      NEXTD=0

*  Set HLP state.
      JHELP=1

*  Store the file size.
      NCHH=NCHARS

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
      GO TO 9999

*  Error on write
 9020 CONTINUE
      J=-3

*  Exit.
 9999 CONTINUE

      END
