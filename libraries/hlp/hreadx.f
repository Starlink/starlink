      SUBROUTINE hlp_HREADX (NAMETR, NAVIG, STRING, NC, J)
*+
*  - - - - - - -
*   H R E A D X
*  - - - - - - -
*
*  Read the help library index, with end-of-file detection, leaving
*  the sequential-access addresses pointing to the indexed record.
*
*  Given (in COMMON):
*     LUHL       i      I/O unit number for help library file
*     HLOPEN     c*()   name of open help library
*     HLNEXT     c*()   name of next help library
*     NEXTX      i      index address for this sequential access
*     LEVOFF     i      logical level for the current help library
*
*  Given (arguments):
*     NAMETR     x      user-supplied name translation routine (note 1)
*     NAVIG      c*(*)  navigation option ('D' or 'A' - note 2)
*
*  Returned (in COMMON):
*     HLNEXT     c*()   help library name for next access
*     NEXTX      i      index address for next sequential access
*     NEXTD      i      data address for next sequential access
*     LEVOFF     i      logical level for the current help library
*     LOFFNU     i      logical level for the next help library
*
*  Returned (arguments):
*     STRING     c*(*)  input record (not including the address of data
*                       record and the end-of-string character)
*     NC         i      length of record (0 or more)
*     J          i      status: +1 = quasi-end-of-file (see note 3)
*                                0 = OK
*                               -1 = HLP system in wrong state
*                               -4 = read error
*                               -7 = attempt to read outside file
*                               -8 = record overflows STRING (see note 4)
*                              -14 = illegal index record
*                              -15 = attempted switch to current file
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
*  2)  The character NAVIG controls what index record will be input on
*      the next call to the present routine.  NAVIG = 'D' or 'd' means
*      that the next entry to be read will be one level further down in
*      the same subtree if such a record exists.  Any other NAVIG value
*      ('A' is suggested) means that the next entry to be read will be
*      the next member of the current subtree at the same level as the
*      current record, if such a record exists.  In either case, if the
*      entry just described does not exist, the file is positioned at
*      the next entry in the order of the data region of the help file,
*      which is in the same order as the original help source.  (This
*      behaviour is controlled by address values which are embedded in
*      the help index records themselves, and is thus determined by the
*      help library creation process rather than the present routine.)
*      If NAVIG is more than one character in length only the first
*      character is tested.  NAVIG values of 'Down' and 'Along' would
*      thus be acceptable.
*
*  3)  The condition "quasi-end-of-file" means that the read was
*      OK but that the record length was zero.  Help files have
*      such zero-length records at the end of the index and the end
*      of the help records.  If the condition arises, the file is
*      is "backspaced" - repositioned at the zero-length record - so
*      that any subsequent call will also report quasi-end-of-file.
*
*  4)  If the record overflows STRING, the first LEN(STRING)
*      characters are stored in STRING, NC is set to LEN(STRING)
*      and J is set to -8.
*
*  5)  Following a call to this routine, a sequential read will
*      input the data record which the index record points to.
*
*  6)  See the source for hlp_HDREAD and hlp_HCLOSE for side-effects
*      involving locations in COMMON.
*
*  7)  This routine is responsible for opening help libraries, when it
*      detects that the currently-open file is not the next one to be
*      requested.  When it opens a new library and it is not the first
*      one, it skips the top-level record, noting en passant the level
*      for use elsewhere (see hlp_HCHKL).
*      
*  8)  This routine handles pointers to other help libraries, signalled
*      by an extra field, beginning with the character '@', after the
*      three addresses and before the level number and topic name.
*
*  Called:  hlp_HOPENR, hlp_HDREAD, hlp_HOPENR, hlp_DEC
*
*  P.T.Wallace   Starlink   9 September 1995
*-

      IMPLICIT NONE

      EXTERNAL NAMETR
      CHARACTER*(*) NAVIG,STRING
      INTEGER NC,J

      INCLUDE 'helpic'

*  Character signifying a pointer to another help library
      CHARACTER POINTR
      PARAMETER (POINTR='@')

      INTEGER IADRX,IADRD,JSTAT,IPTR,IDOWN,IALONG,IFROM,L,ITO
      LOGICAL OLD
      CHARACTER C



*  Check that the HLP system is in the right state.
      IF (JHELP.NE.2) GO TO 9000

*  Is a switch to another library pending?
      IF (HLOPEN.NE.HLNEXT) THEN

*     Yes: save the address.
         IADRX=NEXTX

*     Close any open help library file.
         OLD=HLOPEN.NE.' '
         IF (OLD) CLOSE(LUHL)

*     Open the new help library file and set its logical level number.
         CALL hlp_HOPENR(NAMETR,JSTAT)
         IF (JSTAT.NE.0) GO TO 9999
         LEVOFF=LOFFNU

*     Restore the address.
         NEXTX=IADRX

*     Are we switching to the start of a new help library?
         IF (OLD.AND.NEXTX.LE.0) THEN

*        Yes: it will have contained an entry for the current level and
*        keyword, so read and ignore the top-level entry in the new
*        library.
            CALL hlp_HDREAD(NEXTX,STRING,NC,JSTAT)
            IF (JSTAT.NE.0) GO TO 9999
         END IF
      ELSE IF (NEXTX.LE.0) THEN

*     The right library is already open, but we are trying to read the
*     first record: reopen.
         CALL hlp_HOPENR(NAMETR,JSTAT)
         IF (JSTAT.NE.0) GO TO 9999
      END IF

*  Remember the current positions.
      IADRX=NEXTX
      IADRD=NEXTD

*  Read the next index entry.
      CALL hlp_HDREAD(NEXTX,STRING,NC,JSTAT)
      IF (JSTAT.NE.0) GO TO 9999

*  Quasi-end-of-file?
      IF (NC.EQ.0) THEN

*     Yes: backspace and set the status.
         NEXTX=IADRX
         NEXTD=IADRD
         JSTAT=1
      ELSE

*     No: decode the "data", "down" and "along" addresses.
         IPTR=1
         CALL hlp_DEC(STRING,IPTR,NEXTD)
         CALL hlp_DEC(STRING,IPTR,IDOWN)
         CALL hlp_DEC(STRING,IPTR,IALONG)
         IF (IPTR.GE.NC) GO TO 9005

*     Set IPTR and IFROM to the start of the next field.
         IPTR=IPTR+1
         IFROM=IPTR

*     The next field is usually a level number but might be a pointer to
*     another library.  If it is a pointer, find out where it ends.
         IF (STRING(IFROM:IFROM).EQ.POINTR) THEN
            IF (IFROM.GE.NC) GO TO 9005
            IPTR=INDEX(STRING(IFROM:),' ')+IFROM-1
            IF (IPTR.LT.IFROM.OR.IPTR.GE.NC) GO TO 9005
            IPTR=IPTR+1
         END IF

*     IFROM points to the level number or to the special character
*     preceding a pointer to another help library.  IPTR points to the
*     level number.

*     Look at NAVIG to decide where to leave the file positioned.
         C=NAVIG(:1)
         IF (C.NE.'D'.AND.C.NE.'d'.AND.IALONG.GT.0) THEN

*        "Along" case: we have the address ready.
            NEXTX=IALONG
         ELSE

*        "Down" case: is the current entry a pointer to another library?
            IF (STRING(IFROM:IFROM).NE.POINTR) THEN

*           No: we have the address ready.
               NEXTX=IDOWN
            ELSE

*           Yes: make sure we aren't trying to switch to the current
*           library.
               IF (STRING(IFROM+1:IPTR-2).EQ.HLOPEN) GO TO 9010

*           Store the library name, causing library switch on next read,
*           and set the address to indicate the first entry.
               HLNEXT=STRING(IFROM+1:IPTR-2)
               NEXTX=0

*           Decode the level number to determine the logical level of
*           the new library.
               CALL hlp_DEC(STRING,IPTR,LOFFNU)
               IF (LOFFNU.LT.0) GO TO 9005
               LOFFNU=LOFFNU+LEVOFF

*           Point IPTR back at the level number.
               IPTR=IPTR-1
            END IF
         END IF

*     Squeeze out the part of the record preceding the level number
*     and keyword.
         L=LEN(STRING)
         DO ITO=1,L
            IFROM=IPTR+ITO-1
            IF (IFROM.LE.L) THEN
               C=STRING(IFROM:IFROM)
            ELSE
               C=' '
            END IF
            STRING(ITO:ITO)=C
         END DO
         NC=NC-IPTR
      END IF

*  Exit.
      GO TO 9999

*  HLP system in wrong state
 9000 CONTINUE
      J=-1
      GO TO 9999

*  Invalid record
 9005 CONTINUE
      JSTAT=-14
      GO TO 9999

*  Attempted switch to existing library
 9010 CONTINUE
      JSTAT=-15

*  Return the status.
 9999 CONTINUE
      J=JSTAT

      END
