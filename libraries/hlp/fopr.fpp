#include <config.h>
      SUBROUTINE hlp_FOPR (NAMETR, LU, FILE, LREC, J)
*+
*  - - - - -
*   F O P R
*  - - - - -
*
*  Open a help library file for reading - sequential or direct access.
*
*  !!!  VAX/VMS version  !!!
*
*  Given:
*     NAMETR    x       user-supplied name translation routine (note 1)
*     LU        i       I/O unit number
*     FILE      c*()    help library name
*     LREC      i       length of buffer: > 0 = direct-access
*
*  Returned:
*     J         i           status:  0 = OK
*                                   -1 = open error
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
*      name can be is the value of the LIB argument to the present routine
*      (identifying the root library) or the name following the '@' symbol
*      in a help source file (identifying another library grafted onto
*      the current library).  STRING2 (returned) is, for COMMAND=0, the
*      translation of STRING1 into a filename for use in Fortran OPEN
*      statements.  STATUS (returned) is zero to show success.
*
*  2)  This routine provides the opportunity to interface to
*      non-standard features in different Fortran implementations, in
*      particular the ability to open a file for reading without having
*      write access to it.
*
*  3)  If LREC is less than or equal to zero, the file is opened for
*      sequential access.  Otherwise the file is opened for direct
*      access, with the given record length.
*
*  4)  The help library name is not used directly as a filename, but is
*      subjected to any required translation by means of the NAMETR
*      routine specified in the call.
*
*  P.T.Wallace   Starlink   28 August 1995
*-

      IMPLICIT NONE

      EXTERNAL NAMETR
      INTEGER LU
      CHARACTER*(*) FILE
      INTEGER LREC,J

      CHARACTER FILETR*200



*  Translate the help library name into a filename.
      CALL NAMETR(0,FILE,FILETR,J)
      IF (J.NE.0) GO TO 9999

*  Sequential or direct access?
      IF (LREC.LE.0) THEN

*     Open for sequential access.
         OPEN (LU,STATUS='UNKNOWN',FILE=FILETR,
#if HAVE_FC_OPEN_READONLY
     :        READONLY,
#endif
     :        ERR=9000)
      ELSE

*     Open for direct access.
         OPEN (UNIT=LU,STATUS='OLD',ACCESS='DIRECT',RECL=LREC,
#if HAVE_FC_OPEN_READONLY
     :         READONLY,
#endif
     :         FORM='FORMATTED',ERR=9000,FILE=FILETR)
      END IF

*  Set OK status and exit.
      J=0
      GO TO 9999

*  Error on open.
 9000 CONTINUE
      J=-1

*  Exit.
 9999 CONTINUE

      END
