      INTEGER FUNCTION hlp_HELP (OUTSUB, LOUT, INLINE, LULIB, LIB,
     :                                            JFLAGS, INSUB, NAMETR)
*+
*  - - - - -
*   H E L P
*  - - - - -
*
*  Output help text to a user-supplied output routine.
*
*  Given:
*     OUTSUB      x     user-supplied output subroutine (note 2)
*     LOUT        i     maximum record length accepted by OUTSUB
*     INLINE      c*(*) string specifying required help text (note 3)
*     LULIB       i     I/O unit number for reading help library file
*     LIB         c*(*) name of help library (note 4)
*     JFLAGS      i     flags (note 5)
*     INSUB       x     user-supplied interactive input routine (note 6)
*     NAMETR      x     user-supplied name translation routine (note 7)
*
*  Returned:
*     hlp_HELP    i     status:  +1 = OK
*                               -11 = illegal current level
*                               -12 = OUTSUB reported error
*                               -13 = INSUB reported error
*                              else = other errors (note 8)
*
*  Notes:
*
*  1)  This routine is similar, though not identical, in its arguments
*      and function to the VAX/VMS LBR$OUTPUT_HELP routine.
*
*  2)  The user-supplied OUTSUB routine is an integer function which
*      accepts one argument, the string to be output, and returns a
*      status of +1 if OK.  OUTSUB is responsible for knowing where to
*      write the information, how to handle pagination, and so on.
*
*  3)  The INLINE string contains the series of help keywords,
*      separated by spaces.  It may contain leading as well as
*      embedded and trailing spaces.  The command itself, for example
*      'HELP', is not included in this string, nor any command
*      qualifiers.  A maximum of LEVMAX (defined below) keywords is
*      accepted;  any more are ignored.  Keywords longer than LKWMAX
*      (defined below) are truncated.
*
*  4)  The library specified by LIB is in a special format, produced by
*      the CREHLP program.  The hlp_ routines are not compatible with
*      VMS .HLB files, though the source form of the library is very
*      similar.  The name is subject to environment-dependent
*      translation at open time.
*
*  5)  At present, JFLAGS consists of one flag only, though more options
*      may be added in the future.  An odd JFLAGS value, the usual case,
*      means that once the topic specified in the INLINE string has been
*      reported, an interactive session ensues, with prompted input and
*      the opportunity to explore the help tree.  If JFLAGS is even the
*      output is unaffected, but control is passed back to the caller
*      immediately the topic specified in the INLINE string has been
*      reported, without any ensuing interactive phase.
*
*  6)  The user-supplied interactive input routine INSUB is an integer
*      function which has arguments STRING, PROMPT, L, where STRING
*      receives the line input, PROMPT is the string to output prior to
*      reading the line, and L the number of characters input.  If the
*      call is successful, a function value of +1 is returned.  (Note
*      that INSUB is never called if JFLAGS is even.)
*
*  7)  The user-supplied name translation routine NAMETR is a subroutine
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
*  8)  The status values returned by this routine may be translated into
*      text by means of the hlp_ERRMES routine.
*
*  Called:  hlp_HINIT, hlp_HOPENR, hlp_HTELLX, hlp_HREADX, hlp_HCHKL,
*           hlp_HSEEKX, hlp_SPLIT, hlp_HLEAP, hlp_LENGTH, hlp_UPCASE,
*           hlp_COMSTR, hlp_LINOUT, hlp_HREADD, hlp_REPSUB, hlp_HCLOSE
*
*  P.T.Wallace   Starlink   3 July 1996
*-
 
      IMPLICIT NONE
       
      INTEGER OUTSUB
      EXTERNAL OUTSUB
      INTEGER LOUT
      CHARACTER*(*) INLINE
      INTEGER LULIB
      CHARACTER*(*) LIB
      INTEGER JFLAGS
      INTEGER INSUB
      EXTERNAL INSUB
      EXTERNAL NAMETR
 
      LOGICAL hlp_COMSTR
      INTEGER hlp_LENGTH

*  Wildcard and ellipsis codes
      CHARACTER WILDN*1,ELLIPS*3
      PARAMETER (WILDN='*',ELLIPS='...')
 
*  Maximum length of a keyword
      INTEGER LKWMAX
      PARAMETER (LKWMAX=64)
 
*  Help text output buffer
      INTEGER LOBUF
      PARAMETER (LOBUF=132)
      CHARACTER OUTBUF*(LOBUF)
 
*  Help library file input buffer
      INTEGER LHBUF
      PARAMETER (LHBUF=132)
      CHARACTER HLPBUF*(LHBUF)
 
*  Command-line input buffer
      INTEGER LIBUF
      PARAMETER (LIBUF=132)
      CHARACTER INBUF*(LIBUF)
 
*  Level numbers:  the top of the hierarchy is level zero;  deeper
*  levels are 1 to LEVMAX inclusive.  LEVCUR is the current level in the
*  tree-walk.  LEVEL is temporary storage for a level number.
      INTEGER LEVMAX
      PARAMETER (LEVMAX=9)
      INTEGER LEVCUR,LEVEL

*  The tree-walk context:  help library names, index address, logical
*  level number, keyword for the latest match at each level, and a flag
*  which shows if a perfect match has yet been found at this level (in
*  which case any further matches are ignored).  Apart from the keyword
*  and exact-match entries, each entry is double, because the given point
*  in the help tree may be where a subsidiary help library has been
*  grafted on, thereby requiring two sets of information.  The first
*  value refers to the host and the second to the graft;  in the most
*  common case, where no graft exists, both sets of information are the
*  same.  Note that although the possibility exists that the keyword may
*  differ across a graft this does not yield useful results and should
*  be avoided when preparing the source files.
      CHARACTER*100 HLNAME(2,0:LEVMAX)
      INTEGER LEVPOS(2,0:LEVMAX),
     :        LOGLEV(2,0:LEVMAX)
      CHARACTER*(LKWMAX) LEVELS(0:LEVMAX)
      LOGICAL EXACT(0:LEVMAX)
*  Scratch entries for temporary storage of context.
      CHARACTER*100 FNAME
      INTEGER IADR,LOGL
 
*  Number of entries completely satisfying the latest input
      INTEGER NFOUND
 
*  Work strings for containing level names
      CHARACTER*(LKWMAX) NAME1,NAME2
 
*  List of keywords supplied and how many (note space for '?' endmark)
      CHARACTER PARS(LEVMAX+1)*(LKWMAX)
      INTEGER NPARS
 
*  Status:  <0 = fatal error
*            0 = proceeding normally
*           +1 = no more help at this level
      INTEGER ISTAT
 
      INTEGER MAXOUT,I,NC,IC,J,LIPSIS,IP,MAXBUF
      CHARACTER NAME*(LKWMAX),CUE*9
      LOGICAL IACTIV,OUTPUT,WILD,EOS,FOUND,SUBTOP
 
 

*
*  Preliminaries
*  -------------
 
*  Extract flags.
      IACTIV=MOD(JFLAGS,2).NE.0
 
*  Maximum output record length including indent.
      MAXOUT=MIN(LOUT,LOBUF)
 
*  Initialize the HLP system and open the top-level help library.
      CALL hlp_HINIT(LULIB,LIB,CHAR(0))
      CALL hlp_HOPENR(NAMETR,ISTAT)
      IF (ISTAT.NE.0) GO TO 9900
 
*  Initialize the context.
 
*  Level 0 topic has just been read: note the library name and the
*  address.
      DO I=1,2
         CALL hlp_HTELLX(HLNAME(I,0),LEVPOS(I,0),LOGLEV(I,0))
      END DO
 
*  Read the index entry to get the top-level topic name and check that
*  the level is zero.
      CALL hlp_HREADX(NAMETR,'D',HLPBUF,NC,ISTAT)
      IF (ISTAT.NE.0) GO TO 9900
      CALL hlp_HCHKL(HLPBUF,LEVCUR,LEVELS(0))
 
*  Copy the command string, substituting '?' if null.
      IF (INLINE.EQ.' ') THEN
         INBUF='?'
      ELSE
         INBUF=INLINE
      END IF
 
*
*  Main loop: respond to input until we run out of levels
*  ------------------------------------------------------
 
      DO WHILE (LEVCUR.GE.0)
 
*     Any response?
         IF (INBUF.NE.' ') THEN
 
*        Yes: enable output.
            OUTPUT=.TRUE.
         ELSE
 
*        No: disable output and up a level.
            OUTPUT=.FALSE.
            LEVCUR=LEVCUR-1
         END IF
 
*     Position the help file index at the last match at this level.
         CALL hlp_HSEEKX(HLNAME(2,LEVCUR),LEVPOS(2,LEVCUR),
     :                                                 LOGLEV(2,LEVCUR))

*     Skip the already-matched record.  Leave the file pointing to the
*     next entry down the branch.
         CALL hlp_HREADX(NAMETR,'D',HLPBUF,NC,ISTAT)
         IF (ISTAT.LT.0) GO TO 9900
 
*     Split the input line into separate, uppercase parameters.
 
*     Initialize the string and parameter pointers, and the parameter
*     count.
         IC=1
         I=1
         NPARS=0
 
*     Copy the parameters one by one.
         DO WHILE(I.GT.0.AND.NPARS.LT.LEVMAX)
 
*        Find the start and end of the next parameter.
            CALL hlp_SPLIT(INBUF,IC,I,J)
 
*        Was there a parameter?
            IF (I.GT.0) THEN
 
*           Yes: increment the count.
               NPARS=NPARS+1
 
*           Update the INBUF pointer.
               IC=J+1
 
*           Copy the keyword and fold to uppercase.
               PARS(NPARS)=INBUF(I:J)
               CALL hlp_UPCASE(PARS(NPARS))
            END IF
 
*        Next parameter.
         END DO
 
*     Endmark the list with a question mark.
         PARS(NPARS+1)='?'
 
*     Reset the wildcard/ellipsis flag.
         WILD=.FALSE.
 
*     Point to the first parameter.
         NPARS=1
 
*     Loop until we reach the end marker.
         DO WHILE (PARS(NPARS).NE.'?')
 
*        Pick up the current parameter.
            NAME=PARS(NPARS)

*        Wildcard?
            IF (NAME.EQ.WILDN) THEN
 
*           Yes: set the wildcard/ellipsis flag.
               WILD=.TRUE.
 
*           Next parameter.
               NPARS=NPARS+1
            ELSE
 
*           Not a wildcard: ellipsis?
               I=hlp_LENGTH(NAME)
               IF (I.GE.3.AND.NAME(I-2:).EQ.ELLIPS) THEN
 
*              Yes: set the wildcard/ellipsis flag.
                  WILD=.TRUE.
 
*              Is the ellipsis tagged onto the previous parameter?
                  IF (I.GT.3) THEN
 
*                 Yes: remove it and make it the next parameter.
                     PARS(NPARS)=NAME(:I-3)
                     NPARS=NPARS+1
                     PARS(NPARS)=ELLIPS
                  END IF
 
*              Make the next parameter the end marker.
                  NPARS=NPARS+1
                  PARS(NPARS)='?'
               ELSE
 
*              Neither a wildcard nor an ellipsis: next parameter.
                  NPARS=NPARS+1
               END IF
            END IF
         END DO
 
*     Initialize the ellipsis flag (the level at which the ellipsis was
*     first detected).
         LIPSIS=-1
 
*     Reset the count of topics output as a result of the latest input.
         NFOUND=0
 
*     Initialize the pointer to the list of nominated topics.
         IP=1
 
*     Loop until the whole subtree from the existing unique match at the
*     level one above the current starting level has been searched.
         DO WHILE (IP.GT.0)
 
*        Pick up the name of the topic (uppercase).
            NAME=PARS(IP)
 
*        Ellipsis?
            IF (NAME.EQ.ELLIPS) THEN
 
*           Yes: has it already been seen?
               IF (LIPSIS.LT.0) THEN
 
*              No: note the current level, and interpret the ellipsis
*              on this first occasion as if it were the question mark
*              which follows it.
                  LIPSIS=LEVCUR
                  IP=IP+1
                  NAME='?'
               ELSE
 
*              We are already handling the ellipsis: interpret it as a
*              wildcard.
                  NAME=WILDN
               END IF
 
*           Not an ellipsis: if it isn't the question mark, switch off
*           any ellipsis handling.
            ELSE IF (NAME.NE.'?') THEN
               LIPSIS=-1
            END IF
 
*        Question mark?
            IF (NAME.NE.'?') THEN
 
*           No: search for the requested topic at the next level down.
 
*           Down a level.
               IP=IP+1
               IF (LEVCUR.GE.LEVMAX) GO TO 9020
               LEVCUR=LEVCUR+1

*           Reset the "perfect match" flag
               EXACT(LEVCUR)=.FALSE.
 
*           Initialize the "end of subtree" and "subtopic found" flags.
               EOS=.FALSE.
               FOUND=.FALSE.
               DO WHILE (.NOT.FOUND.AND..NOT.EOS)
 
*           Satisfy any pending switch to another help library.
               CALL hlp_HLEAP(NAMETR,HLPBUF,HLNAME(2,LEVCUR),
     :                          LEVPOS(2,LEVCUR),LOGLEV(2,LEVCUR),ISTAT)
               IF (ISTAT.NE.0) GO TO 9900

*              Save the context.
                  CALL hlp_HTELLX(FNAME,IADR,LOGL)

*              Read the next help library index entry, leaving the file
*              positioned at the next entry for the current level.
                  CALL hlp_HREADX(NAMETR,'A',HLPBUF,NC,ISTAT)
                  IF (ISTAT.LT.0) GO TO 9900
                  EOS=ISTAT.GT.0
 
*              Unless we have reached end-of-subtree, get the level and
*              name for the latest index entry.
                  IF (.NOT.EOS) THEN
                     CALL hlp_HCHKL(HLPBUF,LEVEL,NAME1)
 
*                 Examine the level just encountered.
                     IF (LEVEL.LT.LEVCUR) THEN
 
*                    Higher level: stop looking.
                        EOS=.TRUE.
 
                     ELSE IF (LEVEL.EQ.LEVCUR) THEN
 
*                    Check if the name of the level matches the
*                    requested name, ignoring any further matches once
*                    a perfect match has been detected.
                        NAME2=NAME1
                        CALL hlp_UPCASE(NAME2)
                        IF ((.NOT.EXACT(LEVCUR)).AND.
     :                       hlp_COMSTR(NAME2,NAME)) THEN
 
*                       Match: if perfect, set the flag.
                           IF (NAME2.EQ.NAME) EXACT(LEVCUR)=.TRUE.

*                       Store the context for this level.
                           DO I=1,2
                              HLNAME(I,LEVCUR)=FNAME
                              LEVPOS(I,LEVCUR)=IADR
                              LOGLEV(I,LEVCUR)=LOGL
                           END DO
                           LEVELS(LEVCUR)=NAME1

*                       Set flag to show we found the topic.
                           FOUND=.TRUE.
                        END IF
                     END IF
                  END IF
               END DO
 
*           If the topic has not been found, up two levels,
*           resetting the most recent "exact match" flag.
               IF (.NOT.FOUND) THEN
                  EXACT(LEVCUR)=.FALSE.
                  IP=IP-2
                  LEVCUR=LEVCUR-2
 
*              Unless about to terminate, reposition the file.
                  IF (LEVCUR.GE.0) CALL hlp_HSEEKX(HLNAME(1,LEVCUR+1),
     :                            LEVPOS(1,LEVCUR+1),LOGLEV(1,LEVCUR+1))

*              Skip the already-matched record, leaving the file
*              positioned at the next entry at the current level.
                  CALL hlp_HREADX(NAMETR,'A',HLPBUF,NC,ISTAT)
                  IF (ISTAT.LT.0) GO TO 9900
 
*              If we are handling an ellipsis, and if we have not yet
*              finished doing so, correct the parameter pointer so that
*              the search continues.
                  IF (LIPSIS.GE.0.AND.LEVCUR.GE.LIPSIS) IP=IP+1
               ELSE
 
*              We have found the topic, but the index is now positioned
*              at the start of the next subtree.  Reposition the index
*              one entry down from the latest find, with the data
*              pointer set to the keyword record for the latest find.
                  CALL hlp_HSEEKX(HLNAME(2,LEVCUR),LEVPOS(2,LEVCUR),
     :                                                 LOGLEV(2,LEVCUR))
                  CALL hlp_HREADX(NAMETR,'D',HLPBUF,NC,ISTAT)
                  IF (ISTAT.LT.0) GO TO 9900
 
*              Satisfy any pending switch to another help library.
                  CALL hlp_HLEAP(NAMETR,HLPBUF,HLNAME(2,LEVCUR),
     :                          LEVPOS(2,LEVCUR),LOGLEV(2,LEVCUR),ISTAT)
                  IF (ISTAT.NE.0) GO TO 9900
               END IF
            ELSE
 
*           We have a match to report.
 
*           Increment the count of topics/subtopics output.
               NFOUND=NFOUND+1
 
*           If output is enabled, display the topic.
               IF (OUTPUT) THEN
 
*              Output the heading (n.b. level 0 only at the very start).
                  DO LEVEL=0,LEVCUR
                     IF (LEVEL.NE.0.OR.(LEVEL.EQ.0.AND.LEVCUR.EQ.0))
     :               THEN
                        J=OUTSUB(' ')
                        IF (J.NE.1) GO TO 9030
                        OUTBUF=LEVELS(LEVEL)(:MIN(LOUT,LKWMAX))
                        CALL hlp_LINOUT(OUTSUB,LOUT,2*(LEVEL-1),OUTBUF,
     :                                                                J)
                        IF (J.NE.1) GO TO 9030
                     END IF
                  END DO
                  J=OUTSUB(' ')
                  IF (J.NE.1) GO TO 9030
 
*              Display the current level's text and subtopics.
 
*              The next record to be read from the data section of the
*              help library is the keyword record for the current level:
*              skip it.
                  CALL hlp_HREADD(HLPBUF,NC,ISTAT)
                  IF (ISTAT.LT.0) GO TO 9900
 
*              Reset the "there were subtopics" flag.
                  SUBTOP=.FALSE.
 
*              Now read from the data section of the help library and
*              output until we come across a new level (or end of text
*              at this level).
                  EOS=.FALSE.
                  LEVEL=-1
                  DO WHILE (.NOT.EOS.AND.LEVEL.LT.0)
 
*                 Read the current line from the help library.
                     CALL hlp_HREADD(HLPBUF,NC,ISTAT)
                     IF (ISTAT.LT.0) GO TO 9900
 
*                 If not EOF, start of new level?
                     IF (ISTAT.EQ.0) THEN
                        CALL hlp_HCHKL(HLPBUF,LEVEL,NAME1)
                        IF (LEVEL.GE.0) THEN
 
*                       Yes: examine the level.
                           IF (LEVEL.LE.LEVCUR) THEN
 
*                          Same or higher level: finished outputting
*                          text.
                              EOS=.TRUE.
                           ELSE
 
*                          Deeper: set the "there are subtopics" flag.
                              SUBTOP=.TRUE.
                           END IF
                        ELSE
 
*                       Not a new level: output the line.
                           CALL hlp_LINOUT(OUTSUB,LOUT,2*MAX(LEVCUR,1),
     :                                                       HLPBUF,J)
                           IF (J.NE.1) GO TO 9030
                        END IF
                     ELSE
 
*                    EOF: counts as end of text at this level.
                        EOS=.TRUE.
                     END IF
 
*                 Read the next record.
                  END DO
 
*              Unless wildcarding or handling an ellipsis, list any
*              subtopics.
                  NAME=PARS(MAX(IP-1,1))
                  IF (NAME.NE.WILDN.AND.NAME.NE.ELLIPS.AND.SUBTOP) THEN
                     CALL hlp_REPSUB(NAMETR,OUTSUB,LOUT,LEVCUR,NAME1,
     :                                              OUTBUF,HLPBUF,ISTAT)
                     IF (ISTAT.LT.0) GO TO 9900
                  END IF
 
*              If EOF, flag no more to be found.
                  FOUND=ISTAT.EQ.0

*              Position the help library index at the last match at this
*              level.
                  CALL hlp_HSEEKX(HLNAME(1,LEVCUR),LEVPOS(1,LEVCUR),
     :                                                 LOGLEV(1,LEVCUR))

*              Ellipsis?
                  IF (LIPSIS.LT.0) THEN

*                 No: skip the already-matched record, leaving the file
*                 pointing to the next entry for the current level.
                     CALL hlp_HREADX(NAMETR,'A',HLPBUF,NC,ISTAT)
                     IF (ISTAT.LT.0) GO TO 9900
                  ELSE

*                 Yes: skip the already-matched record, leaving the file
*                 pointing to the next entry down the branch.
                     CALL hlp_HREADX(NAMETR,'D',HLPBUF,NC,ISTAT)
                     IF (ISTAT.LT.0) GO TO 9900
                  END IF
               ELSE
 
*              Output is disabled.  This means that we are returning
*              through successively higher levels, and so we know there
*              are subtopics.
                  SUBTOP=.TRUE.
               END IF
 
*           Output a blank line.
               J=OUTSUB(' ')
               IF (J.NE.1) GO TO 9030
 
*           Point to the previous parameter.
               IP=IP-1
 
*           Unless we are handling an ellipsis, up one level.
               IF (LIPSIS.LT.0) LEVCUR=LEVCUR-1
            END IF
         END DO
 
*     The current level is now one above where it was when the last
*     input line was supplied; down a level to where it was.
         LEVCUR=LEVCUR+1

*     React to how many matches were found.
         IF (NFOUND.LT.1) THEN

*        Nothing found: issue error report.

*        Output the heading.
            DO LEVEL=1,LEVCUR
               J=OUTSUB(' ')
               IF (J.NE.1) GO TO 9030
               OUTBUF=LEVELS(LEVEL)(:MIN(LOUT,LKWMAX))
               CALL hlp_LINOUT(OUTSUB,LOUT,2*(LEVEL-1),OUTBUF,J)
               IF (J.NE.1) GO TO 9030
            END DO
            J=OUTSUB(' ')
            IF (J.NE.1) GO TO 9030

*        Load the output record with the level names.
            MAXBUF=LOBUF-hlp_LENGTH(NAME)-1
            OUTBUF='Sorry, no documentation on'
            IC=hlp_LENGTH(OUTBUF)+2

*        First batch of level names from the help library.
            I=1
            DO WHILE (I.LE.LEVCUR.AND.IC.LE.MAXBUF)

*           Is there room for the level name?
               IF (IC+hlp_LENGTH(LEVELS(I)).LE.MAXBUF) THEN

*              There is: load it and convert it to uppercase.
                  OUTBUF(IC:)=LEVELS(I)
                  CALL hlp_UPCASE(OUTBUF(IC:))

*              Advance the pointer.
                  IC=hlp_LENGTH(OUTBUF)+2
               END IF

*           Do the next level.
               I=I+1
            END DO

*         Second batch of level names as supplied in input line.
            I=1
            DO WHILE (I.LT.NPARS.AND.IC.LE.MAXBUF)

*           Is there room for the level name?
               IF (IC+hlp_LENGTH(PARS(I)).LE.MAXBUF) THEN

*              There is: load it and convert it to uppercase.
                  OUTBUF(IC:)=PARS(I)
                  CALL hlp_UPCASE(OUTBUF(IC:))

*              Advance the pointer.
                  IC=hlp_LENGTH(OUTBUF)+2
               END IF

*           Do the next level.
               I=I+1
            END DO

*        Output the warning.
            CALL hlp_LINOUT(OUTSUB,LOUT,2*MAX(LEVCUR,1),OUTBUF,J)
            IF (J.NE.1) GO TO 9030

*        Position the help file index at the first subtopic.
            CALL hlp_HSEEKX(HLNAME(2,LEVCUR),LEVPOS(2,LEVCUR),
     :                                              LOGLEV(2,LEVCUR))
            CALL hlp_HREADX(NAMETR,'D',HLPBUF,NC,ISTAT)
            IF (ISTAT.NE.0) GO TO 9900

*        List subtopics at the prompt level.
            CALL hlp_REPSUB(NAMETR,OUTSUB,LOUT,LEVCUR,NAME1,OUTBUF,
     :                                                  HLPBUF,ISTAT)
            IF (ISTAT.LT.0) GO TO 9900
            J=OUTSUB(' ')
            IF (J.NE.1) GO TO 9030

*        Single match (except as the result of wildcard)?
         ELSE IF (NFOUND.EQ.1.AND..NOT.WILD) THEN

*        Yes: adjust the current level to that of the match.
            LEVCUR=LEVCUR+NPARS-1

*        Up a level if no subtopics found.
            IF (.NOT.SUBTOP) LEVCUR=LEVCUR-1
         END IF

*     Interactive prompting in effect?
         IF (IACTIV) THEN

*        Yes: get next input.
 
*        Load the output buffer with the level names.
            I=1
            IC=1
            DO WHILE (I.LE.LEVCUR.AND.IC.LE.LOBUF)
 
*           Is there room for the level name?
               IF (IC+hlp_LENGTH(LEVELS(I)).LE.LOBUF) THEN
 
*              There is: load it.
                  OUTBUF(IC:)=LEVELS(I)
               ELSE
 
*              There isn't: overflow to stop the loop.
                  I=LOBUF
               END IF
 
*           Place for next level name.
               IC=hlp_LENGTH(OUTBUF)+2
 
*           Do the next level.
               I=I+1
            END DO
 
*        Complete the prompt string.
            IF (LEVCUR.LT.1) THEN
               CUE='Topic?'
            ELSE
               CUE='Subtopic?'
            END IF
            IF (IC+hlp_LENGTH(CUE).GT.LOBUF) IC=1
            OUTBUF(IC:)=CUE
 
*        Ask for the next topic.
            J=INSUB(INBUF,OUTBUF(:MIN(hlp_LENGTH(OUTBUF)+1,LOBUF)),NC)
            IF (J.NE.1) GO TO 9040
 
*        If we are about to search from the top, terminate instead.
            IF (LEVCUR.LE.0.AND.INBUF.EQ.' ') LEVCUR=-1
         ELSE
 
*        Interactive prompting not in effect: trigger termination.
            LEVCUR=-1
         END IF
 
*     Handle the latest input line if any.
      END DO
 
*  Set OK status and wrap up.
      ISTAT=1
      GO TO 9900
 
*
*  Errors
*  ------
 
*  Illegal LEVCUR
 9020 CONTINUE
      ISTAT=-11
      GO TO 9900
 
*  Bad OUTSUB call
 9030 CONTINUE
      ISTAT=-12
      GO TO 9900
 
*  Bad INSUB call
 9040 CONTINUE
      ISTAT=-13
 
*
*  Wrap up
*  -------
 
 9900 CONTINUE
 
*  Close the current help library (unchecked).
      CALL hlp_HCLOSE(J)
 
*  Return the status.
      hlp_HELP=ISTAT
 
      END
