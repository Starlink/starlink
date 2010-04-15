      INTEGER FUNCTION ICH_WORD(STRING,IST,DELIMS,QUOTES,WORD,
     :                                                  LWORD,CHAR)
C+
C                          I C H _ W O R D
C
C.    Routine name: ICH_WORD
C
C.    Function: Returns the next word in a string.
C
C.    Description:
C     Returns the next 'word' in a string.  A word is defined here
C     as anything separated by any of a number of specified separating
C     delimiters, or enclosed between any of a number of specified
C     pairs of bracketing delimiters.
C
C.    Language: Fortran
C
C.    Call:  POSN=ICH_WORD(STRING,IST,DELIMS,QUOTES,WORD,LWORD,CHAR)
C
C.    Parameters:   (">" input, "<" output)
C
C     (>) STRING    (Fixed string, dscr) The string in question.
C     (>) IST       (Integer, ref) The number of the first character in
C                   the string to be examined.  (The first character
C                   is 1, not 0)
C     (>) DELIMS    (Fixed string, descr) The possible delimiters - space
C                   is not automatically assumed to be a delimiter, it
C                   has to be specified explicitly.
C     (>) QUOTES    (Fixed string, descr) The 'bracketing delimiters' -
C                   pairsd of characters that can enclose a word.  Setting
C                   QUOTES to blank disables this feature.
C     (<) WORD      (Fixed string, descr) The delimited word.
C     (<) LWORD     (Integer, descr) The number of characters in WORD.
C     (<) CHAR      (Fixed string, descr) If the word was bracketed, CHAR
C                   is returned with the single character used for the
C                   first bracket.  Otherwise, CHAR will be blank.
C.    Returns:
C
C     (<) POSN      (Integer, function value) The position in STRING
C                   of the delimiting character.  See notes for fuller
C                   details.  If POSN is positive, a call to ICH_WORD
C                   with IST=POSN+1 will pick up the next word in the
C                   string.  POSN<0 => end of string reached.
C
C.    External variables used: None
C
C.    Subroutines / functions used: None
C
C.    Notes:
C
C     In a little more detail:  Words are delimited by any of a number
C     of delimiting characters.  If the space character is included in
C     the list, then a series of spaces including up to one of the
C     other delimiting characters is treated as a single delimiter.
C     ICH_WORD starts at a given position (IST) in a character string
C     (STRING) and works along until it comes to the start of a delimited
C     word.  Then it continues until it finds the end of the word and
C     the end of the delimiter that terminates that word.  Optionally,
C     if the word begins with any of a number of bracketing delimiters
C     (QUOTES - a typical example might be the [] pair, or "") then
C     ICH_WORD will include in the word anything between the two bracketing
C     delimiters (but not the delimiters themselves.)
C
C     For example, if the delimiters are specified as ';., ' and
C     quotes as '{}""' then the string
C
C     A "quoted string", a {Pascal comment}; and nothing else.
C
C     contains the following words -
C
C     A
C     quoted string
C     a
C     Pascal comment
C     and
C     nothing
C     else
C
C     The way POSN (the function value) is set is as follows: If the word
C     was delimited by a combination of blanks and a delimiting character,
C     POSN will point to the delimiting character. If it was delimited
C     only by blanks, it will point to the final blank.  If the word was
C     delimited by the end of the string, POSN will be set to zero.  If
C     application of these rules would make POSN point to the final
C     character in the string, it is set to -1.  This is so the calling
C     program can test POSN and, so long as POSN was positive, can always
C     call ICH_WORD again with IST=POSN+1.  An exceptional condition occurs
C     when ICH_WORD reaches the end of the string before it finds the end
C     of a bracketed string, and in this case POSN is returned as -2.
C
C.    Internal declaration:
C
C     INTEGER FUNCTION ICH_WORD(STRING,IST,DELIMS,QUOTES,WORD,LWORD,CHAR)
C     INTEGER IST, LWORD
C     CHARACTER*(*) STRING, DELIMS, QUOTES, WORD, CHAR
C
C.    Author: Keith Shortridge, AAO
C
C.    Date: 26th June 1985
C+
C     History:
C        26th June 1985.  Original version.  KS/AAO
C        3rd  Sept 1987.  Minor bug fix.  Quotes were not being recognised
C                         if they appeared immediately after text, eg
C                         if [] were a quote pair, they were missed in
C                         'DATA[1524]', but spotted in 'DATA [1524]'.
C                         KS / AAO.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IST, LWORD
      CHARACTER*(*) STRING, DELIMS, QUOTES, WORD, CHAR
C
C     Local variables
C
      LOGICAL BLANK
      INTEGER ACTION, CHTYPE, I, IPTR, LSTR, POSN, STATE
      CHARACTER CHR*1, QUOTER
C
C     ICH_WORD is a table-driven, multi-state routine.  The possible
C     states are as follows:
C
C     LB    In leading blanks, before a word.
C     QW    In a quoted word (ie one started by a quote character).
C     IW    In a word (not a quoted word).
C     EQW   Immediately after a quote in a quoted word.
C     TB    In trailing blanks.
C     EX    Quit the search.
C
C     Characters are divided into the following sets:
C
C     DEL   A delimiter
C     QUO   A quote - once a quoted word begins, only the terminating
C           quote character counts as QUO.
C     BLA   A blank - if a blank is a delimiter.
C     OTH   Anything else.
C     EN    The end of the string.
C
C     With each state and character type is associated a) the next
C     state to enter, and b) an optional action.  The actions are
C
C     POS   Set POSN (the function value) to the current character.
C     POS0  Set POSN to zero.
C     WRD   Add this character to WORD.
C     POSM1 Set POSN to point to the previous character.
C     ERR   Set POSN to -2 to indicate unterminated quote.
C
C     States, types and actions are defined as -
C
      INTEGER LB, QW, IW, EQW, TB, EX
      INTEGER DEL, QUO, BLA, OTH, EN
      INTEGER POS, POS0, WRD, POSM1, ERR
      PARAMETER (LB=1, QW=2, IW=3, EQW=4, TB=5, EX=6)
      PARAMETER (DEL=1, QUO=2, BLA=3, OTH=4, EN=5)
      PARAMETER (POS=1, POS0=2, WRD=3, POSM1=4, ERR=5)
C
C     The decision table is as follows -
C
C            DEL       QUO      BLA       OTH       EN
C          --------   -------   ------   -------   -------
C     LB |  EX+POS,  QW,       LB,      IW+WRD,   EX+POS0
C     QW |  QW+WRD,  EQW,      QW+WRD,  QW+WRD,   EX+ERR
C     IW |  EX+POS,  EX+POSM1, TB,      IW+WRD,   EX+POS0
C     EQW|  EX+POS,  QW+WRD,   TB,      EX+POSM1, EX+POS0
C     TB |  EX+POS,  EX+POSM1, TB,      EX+POSM1, EX+POS0
C

      INTEGER STATES(EN,EX-1), ACTS(EN,EX-1)
      DATA STATES/
     :      EX,      QW,       LB,      IW,       EX,
     :      QW,      EQW,      QW,      QW,       EX,
     :      EX,      EX,       TB,      IW,       EX,
     :      EX,      QW,       TB,      EX,       EX,
     :      EX,      EX,       TB,      EX,       EX/
      DATA ACTS/
     :      POS,     0,        0,       WRD,      POS0,
     :      WRD,     0,        WRD,     WRD,      ERR,
     :      POS,     POSM1,    0,       WRD,      POS0,
     :      POS,     WRD,      0,       POSM1,    POS0,
     :      POS,     POSM1,    0,       POSM1,    POS0/
C
C     Initial values and check on IST value
C
      LSTR=LEN(STRING)
      WORD=' '
      CHAR=' '
      LWORD=0
      IF ((IST.LT.0).OR.(IST.GT.LSTR)) THEN
         POSN=0
         GO TO 500
      END IF
C
C     See if blank is a delimiter
C
      BLANK=INDEX(DELIMS,' ').NE.0
C
C     Loop is: get char, classify, change state, perform action.
C
      IPTR=IST
      STATE=LB
      DO WHILE (STATE.NE.EX)
C
C        Get char, if end of string not reached.
C
         IF (IPTR.GT.LSTR) THEN
            CHTYPE=EN
         ELSE
            CHR=STRING(IPTR:IPTR)
C
C           Char is probably 'other'.  Set to that, then check
C           the other possibilities.
C
            CHTYPE=OTH
C
C           Check for a blank
C
            IF (BLANK.AND.(CHR.EQ.' ')) THEN
               CHTYPE=BLA
            ELSE
C
C              Check for a delimiter
C
               DO I=1,LEN(DELIMS)
                  IF (CHR.EQ.DELIMS(I:I)) THEN
                     CHTYPE=DEL
                     GO TO 340        ! Break loop
                  END IF
               END DO
  340          CONTINUE
               IF (CHTYPE.NE.DEL) THEN
C
C                 Check for a quote - note that once state is no longer
C                 LB or IW then we are not interested in QUOTES.  Similarly,
C                 we are only interested in the quote terminator if
C                 we may be within a quoted string (states QW, EQW)
C
                  IF ((STATE.EQ.IW).OR.(STATE.EQ.LB)) THEN
                     IF (QUOTES.NE.' ') THEN
                        DO I=1,LEN(QUOTES),2
                           IF (CHR.EQ.QUOTES(I:I)) THEN
                              CHAR=CHR
                              CHTYPE=QUO
                              IF (I.LT.LEN(QUOTES)) THEN
                                 QUOTER=QUOTES(I+1:I+1)
                              ELSE
                                 QUOTER=CHR
                              END IF
                              GO TO 360       ! Break loop
                           END IF
                        END DO
  360                   CONTINUE
                     END IF
                  ELSE IF ((STATE.EQ.QW).OR.(STATE.EQ.EQW)) THEN
                     IF (CHR.EQ.QUOTER) CHTYPE=QUO
                  END IF
               END IF
            END IF
         END IF
C
C        Set new state, given character type and present state,
C        and get action code, if any.
C
         ACTION=ACTS(CHTYPE,STATE)
         STATE=STATES(CHTYPE,STATE)
C
C        Perform any required action
C
         IF (ACTION.GT.0) THEN
            IF (ACTION.EQ.POS) THEN
               POSN=IPTR
            ELSE IF (ACTION.EQ.POS0) THEN
               POSN=0
            ELSE IF (ACTION.EQ.POSM1) THEN
               POSN=IPTR-1
            ELSE IF (ACTION.EQ.ERR) THEN
               POSN=-2
            ELSE IF (ACTION.EQ.WRD) THEN
               LWORD=LWORD+1
               IF (LWORD.GT.LEN(WORD)) THEN
                  LWORD=LEN(WORD)
               ELSE
                  WORD(LWORD:LWORD)=CHR
               END IF
            END IF
         END IF
         IPTR=IPTR+1
      END DO
C
C     Exit.  Check POSN doesn't point to the end of STRING, and
C     set the function value.
C
  500 CONTINUE
      IF (POSN.GE.LEN(STRING)) POSN=-1
      ICH_WORD=POSN
C
      END
