      SUBROUTINE hlp_CREH (NAMETR, LUIN, SOURCE, LUOUT, LIB, LUERR, EOS,
     :                                                            JSTAT)
*+
*  - - - - -
*   C R E H
*  - - - - -
*
*  Translate file of help source into direct-access help library file.
*
*  Given:
*     NAMETR    x       user-supplied name translation routine (see note)
*     LUIN      i       I/O unit number for reading help source
*     SOURCE    c*(*)   filename for help source, or spaces
*     LUOUT     i       I/O unit number for writing help library
*     LIB       c*(*)   filename for help library, or spaces
*     LUERR     i       I/O unit number for error messages
*     EOS       c*1     character to use as end-of-string
*
*  Returned:
*     JSTAT     i       status: 0=OK, -9=fail
*
*  Note:  The user-supplied name translation routine NAMETR is a
*  subroutine subprogram with arguments COMMAND, STRING1, STRING2, STATUS.
*  COMMAND (given) is an integer which the HLP system always sets to zero;
*  the application may use other values of COMMAND, for example to perform
*  initializations or enquiries.  STRING1 (given) is, for COMMAND=0, the
*  help library name to be translated;  this name can be the value of
*  the LIB argument to the present routine (identifying the root library)
*  or the name following the '@' symbol in a help source file (identifying
*  another library grafted onto the current library).  STRING2 (returned)
*  is, for COMMAND=0, the translation of STRING1 into a filename for use
*  in Fortran OPEN statements.  STATUS (returned) is zero to show success,
*  -16 to show that a string has overflowed and -17 to show any other kind
*  of failure.
*
*  FORMAT OF INPUT FILE:
*
*     Formatted alphanumeric records;  the maximum length is defined in
*     the present module.  Non-printing characters are converted to
*     spaces.
*
*     There are four sorts of record;  COMMENT, KEYWORD, TEXT and END
*     records.
*
*     COMMENT records have '!' as their first character, and are
*     ignored.
*
*     KEYWORD records are of two types: with and without a file pointer.
*     They consist of several fields separated by spaces, but without
*     leading spaces.
*
*     Those without a file pointer begin with a single numeric digit in
*     the range 0-9, indicating the hierarchical level, followed by the
*     keyword.  The keyword is ended by any embedded or trailing spaces.
*     The system is case-insensitive.  A new level must be no more than
*     1 greater than the current level.  The first keyword in the file
*     is the only one allowed to have that level number, which by
*     convention will be zero but need not be.  This top-level keyword
*     will never be matched against an input topic name and is thus
*     arbitrary;  it should be something that describes the whole help
*     library.  A keyword cannot be all spaces.
*
*     The file pointer variant of a keyword record is just the same as
*     the variant without a pointer, except that it begins with the name
*     of a help library to be attached at that point, prefixed by an '@'
*     character.  This file pointer, complete with the '@' prefix is
*     embedded in the direct-access help library produced by the present
*     routine, and is accessed at run-time.  Thus, a level 2 topic for
*     the keyword EXAMPLE contained in the help library LIB_EXAMPLE
*     would require a help source record like '@LIB_EXAMPLE 2 EXAMPLE'.
*     Note that LIB_EXAMPLE is a direct-access help library, not a file
*     of help source.  In summary, the '@' pointer allows branches of
*     help tree to be independently maintained and copied, and for
*     different versions to be substituted without rebuilding the whole
*     help tree.
*
*     TEXT records are just plain text, and apply to the keyword
*     immediately preceding.  Any that precede the first keyword are
*     ignored.  Blank lines before and after each group of TEXT records
*     are ignored.  Note that the rules for recognizing other sorts of
*     record mean that text records cannot begin with '0'-'9', '@' or
*     'END' etc.
*
*     The optional END record consists of the three characters 'END', in
*     any mixture of upper/lowercase.
*
*  FORMAT OF OUTPUT FILE:
*
*     Direct access, formatted;  see hlp_HELP module for record length.
*
*     Consists of logical records terminated with a special character -
*     see the present module.  There are no trailing spaces.  The
*     record terminators do not have to be provided when writing, nor
*     are they seen when reading.
*
*     The first record is the HEADER record.  This gives the size of the
*     file, in characters, as a decimal number in I12.12 format.
*
*     The next set of records is an INDEX;  each record corresponds to
*     one keyword.
*
*     The index is followed by an empty record.
*
*     The next set of records are the TEXT lines interspersed with
*     versions of the KEYWORD records consisting of the effective
*     level number and keyword proper.  Trailing spaces are eliminated.
*
*     The file is terminated by an empty record.
*
*     Each INDEX record is formatted as follows:
*
*       'ttttttttt ddddddddd aaaaaaaaa [@pppp....] n kkkk....'
*
*     The fields are separated by spaces.  There are no trailing blanks.
*
*     The fields ttttttttt, ddddddddd and aaaaaaaaa are all character
*     addresses in I9.9 format.  The first character in the file has
*     address zero.  The address ttttttttt is the address of the
*     corresponding keyword record in the TEXT section of the file.
*
*     The addresses ddddddddd and aaaaaaaaa point to further index
*     entries.  The address ddddddddd ("down") is the entry which is
*     next in sequence when conducting a recursive search down each
*     branch of the tree.  The address aaaaaaaaa ("along") is the entry
*     which is next in sequence when searching for all subtopics at a
*     given level.
*
*     The optional field @pppp.... is present when a file pointer was
*     supplied at this point in the help source.  The string pppp.... is
*     the name of the direct-access help library which is logically
*     attached to the help tree at this point.
*
*     The field n is the level number, a single numeric character in the
*     range 0-9.  It is automatically adjusted so that all help
*     libraries start at level 0.
*
*     The field kkkk.... is the keyword, in uppercase, and in length up
*     to 130 characters.
*
*     Each TEXT record is the help text, exactly as supplied except
*     that keywords are placed following the level number with a single
*     intervening space and text following a keyword is ignored.
*
*  Called:  hlp_FOPR, hlp_HINIT, hlp_HOPENW, hlp_HDWRIT, hlp_LENGTH,
*           hlp_HCLOSE, hlp_ERRMES
*
*  P.T.Wallace   Starlink   9 September 1995
*-

      IMPLICIT NONE

      EXTERNAL NAMETR
      INTEGER LUIN
      CHARACTER*(*) SOURCE
      INTEGER LUOUT
      CHARACTER*(*) LIB
      INTEGER LUERR
      CHARACTER EOS
      INTEGER JSTAT

      INTEGER hlp_LENGTH

*  Maximum keyword depth
      INTEGER LEVMAX
      PARAMETER (LEVMAX=9)

*  Header record
      INTEGER NHEAD
      PARAMETER (NHEAD=12)
      CHARACTER HEADER*(NHEAD)

*  Number of digits in formatted addresses
      INTEGER NDIGS
      PARAMETER (NDIGS=9)

*  Overheads in index: space for 3 addresses, 2 spaces and 1 EOS
      INTEGER NOVERH
      PARAMETER (NOVERH=3*NDIGS+3)

*  Input buffer length
      INTEGER LIN
      PARAMETER (LIN=132)

*  Output buffer length
      INTEGER LOUT
      PARAMETER (LOUT=LIN+NDIGS)

*  Buffers
      CHARACTER IOBUF*(LIN),OUTBUF*(LOUT)

*  Character counts for each chapter (=level) of the index
      INTEGER NCXLEV(0:LEVMAX)

*  Start address for each chapter
      INTEGER ICXLEV(0:LEVMAX)

*  Most recent index entries at the given level and their addresses
      CHARACTER JUMPOB(0:LEVMAX)*(LOUT)
      INTEGER JUMPAD(0:LEVMAX)

*  Last index entry, its level and its address (0=none)
      CHARACTER LASTOB*(LOUT)
      INTEGER LASTLV,LASTAD

*  Collating-sequence value of character zero
      INTEGER IZERO

*  State:  0 = expecting first keyword
*          1 = current record is keyword
*          2 = expecting new batch of help text
*          3 = processing help text
      INTEGER KSTATE

      LOGICAL MORE,NEED
      CHARACTER C1,MES*50
      INTEGER IPASS,LEVOLD,NHIND,NINDEX,NHELP,J,IADR,LEVEL,I,NFP,LEVTOP,
     :        NPBL,IFROM,ITO,ND,NX,K,L


*  Obtain the character code for '0'.
      IZERO=ICHAR('0')

*  Initialize the HLP system.
      CALL hlp_HINIT(LUOUT,LIB,EOS)

*  Open the input file for the first pass.
      CALL hlp_FOPR(NAMETR,LUIN,SOURCE,0,J)
      IF (J.NE.0) GO TO 9000

*  Two passes of source file:  (1) find index size, (2) write the file.
      DO IPASS=1,2

*     Which pass?
         IF (IPASS.EQ.1) THEN

*        Pass 1:  initialize the current level number.
            LEVOLD=-1
         ELSE

*        Pass 2: save size of index plus header plus terminators.
            NHIND=NHEAD+NINDEX+2

*        Reopen input.
            REWIND (UNIT=LUIN)

*        Open the output file (length allows for EOS characters).
            CALL hlp_HOPENW(NAMETR,NHIND+NHELP+1,J)
            IF (J.NE.0) GO TO 9020

*        Write the end-of-index record.
            IADR=NHIND-1
            CALL hlp_HDWRIT(EOS,IADR,J)
            IF (J.NE.0) GO TO 9020

*        Flag no buffer waiting to be output.
            LASTAD=0

*        Reset the deferred index entry addresses.
            DO LEVEL=0,LEVMAX
               JUMPAD(LEVEL)=0
            END DO
         END IF

*     Initialize the counts of index and help characters.
         NINDEX=0
         NHELP=0
         DO LEVEL=0,LEVMAX
            NCXLEV(LEVEL)=0
         END DO

*     State = expecting first keyword.
         KSTATE=0

*     Read the input records.
         MORE=.TRUE.
         DO WHILE (MORE)

*        Read a source record, ignoring comments.
            NEED=.TRUE.
            DO WHILE (NEED)
               READ (LUIN,'(A)',END=90) IOBUF
               IF (IOBUF(1:1).NE.'!') NEED=.FALSE.
            END DO

*        Replace anything funny with spaces.
            DO I=1,LIN
               C1=IOBUF(I:I)
               IF (C1.LT.' '.OR.
     :             C1.GT.'~') IOBUF(I:I)=' '     !!! ASCII-specific !!!
            END DO

*        Look for END.
            IF (IOBUF(4:).EQ.' '.AND.
     :          (IOBUF(1:1).EQ.'E'.OR.IOBUF(1:1).EQ.'e').AND.
     :          (IOBUF(2:2).EQ.'N'.OR.IOBUF(2:2).EQ.'n').AND.
     :          (IOBUF(3:3).EQ.'D'.OR.IOBUF(3:3).EQ.'d')) GO TO 90

*        Keyword or file pointer?
            C1=IOBUF(1:1)
            IF ((C1.GE.'0'.AND.C1.LE.'9'.AND.IOBUF(2:2).EQ.' ').OR.
     :          C1.EQ.'@') THEN

*           Yes: count the number of characters in any file pointer
*           plus the subsequent space (NFP) and then point to and pick
*           up the level number, leaving I pointing to the level number.
               NFP=0
               IF (C1.EQ.'@') THEN
                  DO WHILE (IOBUF(NFP+1:NFP+1).NE.' ')
                     NFP=NFP+1
                     IF (NFP.GT.LIN-4) GO TO 9005
                  END DO
                  I=NFP+2
                  C1=IOBUF(I:I)
                  IF (C1.LT.'0'.OR.C1.GT.'9'.OR.
     :                IOBUF(I+1:I+1).NE.' ') GO TO 9005
               ELSE
                  I=1
               END IF

*           Get the level number.
               LEVEL=ICHAR(C1)-IZERO

*           If the first keyword, note its level.
               IF (KSTATE.EQ.0) LEVTOP=LEVEL

*           State = current record is keyword.
               KSTATE=1

*           Reset pending blank lines count.
               NPBL=0

*           Correct the level number so that the top level is zero.
               LEVEL=LEVEL-LEVTOP

*           If pass 1, validate level.
               IF (IPASS.EQ.1) THEN
                  IF (LEVEL-LEVOLD.GT.1) GO TO 9010
                  LEVOLD=LEVEL
               END IF

*           Find where the keyword starts (IFROM) and ends (ITO).
               IFROM=I+2
               DO WHILE (IOBUF(IFROM:IFROM).EQ.' ')
                  IFROM=IFROM+1
                  IF (IFROM.GT.LIN) GO TO 9005
               END DO
               ITO=IFROM
               DO WHILE (IOBUF(ITO:ITO).NE.' '.AND.ITO.LE.LIN)
                  ITO=ITO+1
               END DO
               ITO=ITO-1

*           Length of data record including overheads.
               ND=ITO-IFROM+4

*           Length of index record including overheads.
               NX=NOVERH+ND
               IF (NFP.GT.0) NX=NX+NFP+1

*           Pass 2?
               IF (IPASS.EQ.2) THEN

*              Yes: clear the output buffer.
                  OUTBUF=' '

*              Format the effective level number.
                  WRITE (C1,'(I1)') LEVEL

*              Copy the file pointer, level and keyword into the index
*              record.
                  I=NOVERH+1
                  IF (NFP.GT.0) THEN
                     OUTBUF(I:I+NFP-1)=IOBUF(:NFP)
                     I=I+NFP+1
                  END IF
                  OUTBUF(I:)=C1//' '//IOBUF(IFROM:ITO)

*              Build the data record.
                  IOBUF(1:2)=C1
                  I=3
                  DO K=IFROM,ITO
                     IOBUF(I:I)=IOBUF(K:K)
                     I=I+1
                  END DO
                  IF (I.LE.LIN) IOBUF(I:)=' '

*              Format the "data" address for the current record.
                  WRITE (OUTBUF(:9),'(I9.9)') NHIND+NHELP

*              Compute the file address for the current index record.
                  IADR=ICXLEV(LEVEL)+NCXLEV(LEVEL)

*              The previous and current index records are as follows:
*
*                                 previous          current
*
*                 text             LASTOB           OUTBUF
*                 level            LASTLV           LEVEL
*                 address          LASTAD           IADR
*
*              In both cases, the text contains the "data" address but
*              neither the "down" nor the "along" addresses.  We now
*              leave the current record for next time and proceed to
*              process the previous index record, if any.

*              Is there a previous record?
                  IF (LASTAD.GT.0) THEN

*                 Yes: the address of the current record is the "down"
*                 address for the previous record: complete that record.
                     WRITE (LASTOB(11:19),'(I9.9)') IADR

*                 Look at the list of deferred records for the current
*                 level downwards.
                     DO L=LASTLV,LEVMAX

*                    Is there a record waiting to be completed and
*                    output?
                        IF (JUMPAD(L).GT.0) THEN

*                       Yes: insert the "along" pointer, write the
*                       record and reset the list entry.
                           WRITE (JUMPOB(L)(21:29),'(I9.9)') LASTAD
      CALL hlp_HDWRIT(JUMPOB(L)(:hlp_LENGTH(JUMPOB(L))),JUMPAD(L),J)
                           IF (J.NE.0) GO TO 9020
                           JUMPAD(L)=0
                        END IF
                     END DO

*                 Store the current partially-completed record and its
*                 future disc address, to be completed and output when
*                 the next entry at this or a higher level is available.
                     JUMPOB(LASTLV)=LASTOB
                     JUMPAD(LASTLV)=LASTAD
                  END IF

*              Store the current record, level and address for next
*              time.
                  LASTOB=OUTBUF
                  LASTLV=LEVEL
                  LASTAD=IADR
               END IF

*           Advance the characters-in-index counters.
               NINDEX=NINDEX+NX
               NCXLEV(LEVEL)=NCXLEV(LEVEL)+NX
            END IF

*        All input records from the first keyword on.
            IF (KSTATE.GT.0) THEN

*           Blank record?
               IF (IOBUF.EQ.' ') THEN

*              Yes: if we are processing text, increment the pending
*              blank lines counter.
                  IF (KSTATE.EQ.3) NPBL=NPBL+1

               ELSE

*              Not a blank record: keyword?
                  IF (KSTATE.NE.1) THEN

*                 Not a keyword: output any pending blank lines.
                     DO I=1,NPBL
                        IF (IPASS.EQ.2) THEN
                           IADR=NHIND+NHELP
                           CALL hlp_HDWRIT(' ',IADR,J)
                           IF (J.NE.0) GO TO 9020
                        END IF
                        NHELP=NHELP+2
                     END DO

*                 Zero the count.
                     NPBL=0

*                 Update the status.
                     KSTATE=3
                  END IF

*              Strip trailing blanks from current record and, if pass 2,
*              output it.
                  K=hlp_LENGTH(IOBUF)
                  IF (IPASS.EQ.2) THEN
                     IADR=NHIND+NHELP
                     CALL hlp_HDWRIT(IOBUF(:K),IADR,J)
                     IF (J.NE.0) GO TO 9020
                  END IF
                  NHELP=NHELP+K+1
               END IF

*           If we have just processed a keyword, update the status.
               IF (KSTATE.EQ.1) KSTATE=2
            END IF
            GO TO 100

*        End of input file: stop the loop.
 90         CONTINUE
            MORE=.FALSE.

*        Which pass?
            IF (IPASS.EQ.1) THEN

*           Pass 1: work out the index chapter start addresses.
               I=NHEAD+1
               DO LEVEL=0,LEVMAX
                  ICXLEV(LEVEL)=I
                  I=I+NCXLEV(LEVEL)
               END DO
            ELSE

*           Pass 2: output deferred index entries.

*           Is there a previous record?
               IF (LASTAD.GT.0) THEN

*              Yes: the address of the end of index is the "down"
*              address for the previous record: complete that record.
                  IADR=NHIND-1
                  WRITE (LASTOB(11:19),'(I9.9)') IADR

*              Look at the list of deferred records for the current
*              level downwards.
                  DO L=LASTLV,LEVMAX

*                 Is there a record waiting to be completed and output?
                     IF (JUMPAD(L).GT.0) THEN

*                    Yes: insert the "along" pointer, write the record
*                    and reset the list entry.
                        WRITE (JUMPOB(L)(21:29),'(I9.9)') LASTAD
          CALL hlp_HDWRIT(JUMPOB(L)(:hlp_LENGTH(JUMPOB(L))),JUMPAD(L),J)
                        IF (J.NE.0) GO TO 9020
                        JUMPAD(L)=0
                     END IF
                  END DO

*              Store the current partially-completed record and its
*              future disc address, to be completed and output when the
*              next entry at this or a higher level is available.
                  JUMPOB(LASTLV)=LASTOB
                  JUMPAD(LASTLV)=LASTAD

*              Look again at the list of deferred records.
                  DO L=0,LASTLV

*                 Is there a record waiting to be completed and output?
                     IF (JUMPAD(L).GT.0) THEN

*                    Yes: insert the "along" pointer and write the
*                    record.
                        WRITE (JUMPOB(L)(21:29),'(I9.9)') IADR
          CALL hlp_HDWRIT(JUMPOB(L)(:hlp_LENGTH(JUMPOB(L))),JUMPAD(L),J)
                        IF (J.NE.0) GO TO 9020
                     END IF
                  END DO
               END IF
            END IF

*        Next input record if any.
 100        CONTINUE
         END DO

*     Next pass.
      END DO

*  Endmark, write header, close the output file, and exit.
      IADR=NHIND+NHELP
      CALL hlp_HDWRIT(EOS,IADR,J)
      IF (J.NE.0) GO TO 9020
      WRITE (HEADER,'(I12.12)') NHIND+NHELP+1
      IADR=0
      CALL hlp_HDWRIT(HEADER,IADR,J)
      IF (J.NE.0) GO TO 9020
      CALL hlp_HCLOSE(J)
      IF (J.NE.0) GO TO 9020
      GO TO 9999

*  Error opening source file
 9000 CONTINUE
      WRITE (LUERR,'(1X,''Unable to open help source file'')')
      GO TO 9990

*  Invalid level
 9005 CONTINUE
      IF (IOBUF(40:).NE.' ') IOBUF(40:)='.....'
      WRITE (LUERR,'(1X,''Invalid level: '',A)') IOBUF(:45)
      GO TO 9990

*  No keyword
 9010 CONTINUE
      WRITE (LUERR,'(1X,''Keyword absent at level'',I2)') LEVEL
      GO TO 9990

*  HLP system error status
 9020 CONTINUE
      CALL hlp_ERRMES(J,MES)
      WRITE (LUERR,'(1X,A)') MES

*  Run aborted
 9990 CONTINUE
      WRITE (LUERR,'(1X,''Abort!'')')
      J=-9

*  Wrap up.
 9999 CONTINUE
      CLOSE (UNIT=LUIN)
      JSTAT=J

      END
