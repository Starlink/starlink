      SUBROUTINE hlp_HLEAP (NAMETR, STRING, FNAME, IADR, LOGL, J)
*+
*  - - - - - -
*   H L E A P
*  - - - - - -
*
*  Satisfy any pending switch to another help library.  If a switch is
*  made, return the context information for the first entry in the new
*  library and skip that entry.
*
*  Given (in COMMON):
*     HLOPEN     c*()   name of open help library
*     HLNEXT     c*()   name of next help library
*     LUHL       i      I/O unit number for help library file
*     NEXTX      i      index address for this sequential access
*     LOFFNU     i      logical level for the next help library
*
*  Given (argument):
*     NAMETR     x      user-supplied name translation routine (note 1)
*
*  Returned (in COMMON):
*     NEXTX      i      index address for this sequential access
*     NEXTD      i      data address for next sequential access
*     LEVOFF     i      logical level for the current help library
*
*  Returned (arguments)
*     STRING     c*(*)  workspace to receive index entry
*     FNAME      c*(*)  new help library name
*     IADR       i      address for first index entry
*     LOGL       i      logical level number for new library
*     J          i      status:  0 = OK
*                              -14 = illegal index record
*                             else = status from called routines
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
*  2)  If no library switch occurs, STRING, FNAME, IADR and LOGL are
*       unchanged.
*
*  Called:  hlp_HOPENR, hlp_HTELLX, hlp_HDREAD, hlp_DEC
*
*  P.T.Wallace   Starlink   9 September 1995
*-

      IMPLICIT NONE

      EXTERNAL NAMETR
      CHARACTER*(*) STRING,FNAME
      INTEGER IADR,LOGL,J

      INCLUDE 'helpic'

      INTEGER JSTAT,NC,IPTR



*  Preset the status to OK.
      JSTAT=0

*  Is a switch to another library pending?
      IF (HLOPEN.NE.HLNEXT) THEN

*     Yes: close the old help library file.
         CLOSE (LUHL)

*     Open the new help library file and set its logical level number.
         CALL hlp_HOPENR(NAMETR,JSTAT)
         IF (JSTAT.NE.0) GO TO 9999
         LEVOFF=LOFFNU

*     Return the position of the first index entry and read it.
         CALL hlp_HTELLX(FNAME,IADR,LOGL)
         CALL hlp_HDREAD(NEXTX,STRING,NC,JSTAT)
         IF (JSTAT.NE.0) GO TO 9999
         IF (NC.EQ.0) GO TO 9000

*     Decode the "data" address.
         IPTR=1
         CALL hlp_DEC(STRING,IPTR,NEXTD)
      END IF

*  Exit.
      GO TO 9999

*  Invalid record
 9000 CONTINUE
      JSTAT=-14

*  Return the status.
 9999 CONTINUE
      J=JSTAT

      END
