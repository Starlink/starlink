      SUBROUTINE hlp_REPSUB (NAMETR, OUTSUB, LOUT, LEVCUR, NAME, OUTBUF,
     :                                                     HLPBUF,ISTAT)
*+
*  - - - - - - -
*   R E P S U B
*  - - - - - - -
*
*  Report subtopic names.
*
*  Given:
*     NAMETR      x      user-supplied name translation routine (note 1)
*     OUTSUB      x      user-supplied output routine (note 2)
*     LOUT        i      maximum record length accepted by OUTSUB
*     LEVCUR      i      current level number
*
*  Workspace:
*     NAME        c*(*)  keyword
*     OUTBUF      c*(*)  output buffer
*     HLPBUF      c*(*)  input buffer
*
*  Returned:
*     ISTAT       i      status:    0 = OK
*                                 -12 = OUTSUB error
*                                else = help file I/O status
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
*  2)  OUTSUB is the user-supplied output routine.  The call is
*      J=OUTSUB(STRING), where STRING is a character string of maximum
*      length LOUT.  A returned value of +1 for J means success.
*
*  3)  Prior to calling the present routine, the help file index must be
*      positioned so that the index entry for the first subtopic will be
*      input next.  On leaving the routine, the help file index is
*      positioned so that the next record to be input is the first index
*      entry that is not part of the current subtree.
*
*  Called:  hlp_LINOUT, hlp_HREADX, hlp_HCHKL, hlp_LENGTH, hlp_HTELLX,
*           hlp_HCHKL, hlp_HSEEKX
*
*  P.T.Wallace   Starlink   15 September 1995
*-

      IMPLICIT NONE

      EXTERNAL NAMETR
      INTEGER OUTSUB
      EXTERNAL OUTSUB
      INTEGER LOUT
      INTEGER LEVCUR
      CHARACTER*(*) NAME,OUTBUF,HLPBUF
      INTEGER ISTAT

      INTEGER hlp_LENGTH

*  Increment between topic names on the output line.
      INTEGER IPITCH
      PARAMETER (IPITCH=11)

      INTEGER J,INDENT,MAXIND,NC,LEVEL,LEVSUB,IC,IADR,LOGL
      CHARACTER FNAME*100



*  Output a heading for the subtopics.
      J=OUTSUB(' ')
      IF (J.NE.1) GO TO 9000
      INDENT=2*MAX(LEVCUR,1)
      MAXIND=MIN(LOUT,LEN(OUTBUF))-INDENT
      OUTBUF='Additional information available:'
      CALL hlp_LINOUT(OUTSUB,LOUT,INDENT,OUTBUF,J)
      IF (J.NE.1) GO TO 9000
      J=OUTSUB(' ')
      IF (J.NE.1) GO TO 9000

*  Read the first subtopic name from the index and get its name.  Set
*  the file pointer so that the next item to be read will be the next
*  index entry at the current level.
      CALL hlp_HREADX(NAMETR,'A',HLPBUF,NC,ISTAT)
      IF (ISTAT.NE.0) GO TO 9999
      CALL hlp_HCHKL(HLPBUF,LEVEL,NAME)

*  Continue reading the help library index entries for the current level
*  and for the current subtree until we are returned to a higher level
*  or reach end-of-index.
      LEVSUB=LEVCUR+1
      LEVEL=LEVSUB
      OUTBUF=' '
      IC=1
      DO WHILE (ISTAT.EQ.0.AND.LEVEL.GE.LEVSUB)

*     Is the current subtopic at the right level?
         IF (LEVEL.EQ.LEVSUB) THEN

*        Yes: load the output buffer with the current subtopic's name.

*        First check if there will be room for it.
            IF (IC+hlp_LENGTH(NAME).GE.MAXIND) THEN

*           There is not: display the current contents of the buffer.
               CALL hlp_LINOUT(OUTSUB,LOUT,INDENT,OUTBUF,J)
               IF (J.NE.1) GO TO 9000
               IC=1
            END IF

*        Now load the current name into the buffer.
            OUTBUF(IC:)=NAME

*        Work out where the next name is to go.
            IC=hlp_LENGTH(OUTBUF)+3
            IF (MOD(IC-1,IPITCH).NE.0) IC=((IC-1)/IPITCH+1)*IPITCH+1
         END IF

*     Read the next index record, leaving the file pointer set so that
*     the next item to be read is at the current level or is the first
*     item of the next subtree.
         CALL hlp_HTELLX(FNAME,IADR,LOGL)
         CALL hlp_HREADX(NAMETR,'A',HLPBUF,NC,ISTAT)
         IF (ISTAT.LT.0) GO TO 9999

*     If not EOF get the keyword name and level.
         IF (ISTAT.EQ.0) CALL hlp_HCHKL(HLPBUF,LEVEL,NAME)

*     Load the new level's name (if any).
      END DO

*  Output.
      CALL hlp_LINOUT(OUTSUB,LOUT,INDENT,OUTBUF,J)
      IF (J.NE.1) GO TO 9000

*  Reposition the file so that the next index record to be read is the
*  first one that is nothing to do with the topic just reported.
      CALL hlp_HSEEKX(FNAME,IADR,LOGL)
      GO TO 9999

*  Bad hlp_LINOUT status
 9000 CONTINUE
      ISTAT=-12
 9999 CONTINUE

      END
