      LOGICAL FUNCTION hlp_COMSTR (FULSTR, STR)
*+
*  - - - - - - -
*   C O M S T R
*  - - - - - - -
*
*  This routine returns .TRUE. if the string STR is a valid
*  abbreviation of the string FULSTR.  The rules defining what is
*  a valid abbreviation are as follows:
*
*  1)  The strings can be made up of one or more "words" separated
*  by the character WDSEP, defined below.  Each word in STR can be
*  abbreviated individually.
*
*  2)  The characters WILDA and WILDN (defined below) in the string
*  STR match, respectively, any single non-space character in FULSTR
*  or any sequence of such characters.
*
*  3)  The character ESCAPE (defined below) in STR is itself ignored
*  but causes the next character to be accepted literally.
*
*  4)  If a word from STR contains no "match n" wildcards it can be
*  truncated, so that for example "SAM" would be a valid match for
*  "SAMPLE".  However, if there are are such wildcards truncation is
*  not acceptable; "S*E" would match "SAMPLE" but not "S*PL".  This
*  feature allows abbreviations to be used which unambiguously specify
*  the end of the string as well as the beginning.
*
*  5)  Neither FULSTR nor STR may contain leading or embedded spaces.
*  However, if both are blank, a true result is returned.
*
*  6)  The test is case-sensitive.  If a case-insensitive test is
*  required, folding to lowercase or uppercase must be done outside
*  this routine.
*
*  Defined in INCLUDE:
*     WDSEP        c         word separator character
*     WILDA        c         "match 1" wildcard character
*     WILDN        c         "match n" wildcard character
*     ESCAPE       c         escape character
*
*  Given:
*     FULSTR       c*(*)     the full string
*     STR          c*(*)     the abbreviated string
*
*  Returned:
*     hlp_COMSTR   l         .TRUE. for a match
*
*  Called:  hlp_LENGTH
*
*  P.T.Wallace   Starlink   3 July 1996
*-

      IMPLICIT NONE

      CHARACTER*(*) FULSTR,STR

      INTEGER hlp_LENGTH

      INCLUDE 'comic'

*  Global "still matched" flag:  we give up as soon as this goes false
      LOGICAL MATCH

*  Lengths of FULSTR and STR, excluding trailing spaces
      INTEGER LENF,LENS

*  Pointers to FULSTR and STR
      INTEGER IF,IS

*  Pointers to the ends of the current words within FULSTR and STR
      INTEGER LWBF,LWBS

*  "Match n" wildcard flags: within this word, and last character
      LOGICAL MATCHN,WILD

*  Current and previous characters from STR
      CHARACTER CS,CL

*  The remaining declarations relate to the "moving substring" logic:
*  STR characters lying between two "match n" wildcards (or,
*  superfluously) between a "match n" wildcard and the end of the word,
*  constitute a "moving substring" which can be matched anywhere in the
*  region of FULSTR yet to be used.

*  STR pointers defining the beginning and end of the movable substring,
*  and their saved values.  MOVBEG=0 means "no movable substring".
      INTEGER MOVBEG,MOVEND,MB,ME

*  Movable substring not yet matched - keep searching FULSTR
      LOGICAL NOTFND

*  Movable substring partially matched at current FULSTR trial position
      LOGICAL SOFAR

*  Local FULSTR and STR pointers used during movable substring search
      INTEGER MIF,MIS

*  Current and previous STR characters
      CHARACTER MCL,MCS



*  Assume match to start with.
      MATCH=.TRUE.

*  Reset the "previous character was a match n wildcard" flag.
      WILD=.FALSE.

*  Check for all spaces.
      IF (FULSTR.NE.' '.OR.STR.NE.' ') THEN

*     Obtain the effective lengths of the two strings.
         LENF=hlp_LENGTH(FULSTR)
         LENS=hlp_LENGTH(STR)

*     Initially set the character pointers and result.
         IF=1
         IS=1

*     Now compare each word in turn.
         DO WHILE (MATCH.AND.IS.LE.LENS)

*        Locate the next word boundary in both strings.
            LWBF=INDEX(FULSTR(IF:),WDSEP)+IF-2
            LWBS=INDEX(STR(IS:),WDSEP)+IS-2
            
*        Handle the futile case of a word separator preceded by an
*        "escape".
            IF (IS.GT.1.AND.
     :          LWBS.GT.1.AND.
     :          LWBS.LT.LENS.AND.
     :          STR(LWBS:LWBS).EQ.ESCAPE)
     :                          LWBS=INDEX(STR(LWBS+2:),WDSEP)+LWBS

*        Test if there is a word boundary in STR which is not present in
*        FULSTR.
            IF (LWBF.LT.IF.AND.LWBS.GE.IS) THEN

*           There is:  the strings do not match.
               MATCH=.FALSE.
            ELSE

*           There is not: set the word boundaries to the end of the
*           strings if there are no following words.
               IF (LWBF.LT.IF) LWBF=LENF
               IF (LWBS.LT.IS) LWBS=LENS

*           Reset the "current word contained match n wildcards" flag.
               MATCHN=.FALSE.

*           Cancel the movable substring.
               MOVBEG=0
               MB=0
               
*           Initialize the "last character from STR".
               CL=' '

*           Compare the full and abbreviated versions of the current
*           word until the match fails or the abbreviated string is
*           used up and there is no outstanding movable substring to
*           search for.
               DO WHILE (MATCH.AND.
     :                   (IS.LE.LWBS.OR.MOVBEG.GT.0).AND.
     :                   IF.LE.LWBF)

*              Examine the next character from STR if any, handling any
*              "escape" characters.
                  IF (IS.LE.LWBS) THEN
                     CS=STR(IS:IS)
                     IF (CS.EQ.ESCAPE) THEN
                        IS=MIN(IS+1,LWBS)
                        CL=CS
                        CS=STR(IS:IS)
                     END IF
                  ELSE
                     CS=' '
                  END IF
                  IF ((CS.EQ.WILDN.AND.CL.NE.ESCAPE).OR.
     :                IS.GT.LWBS) THEN

*                 We either have a "match n" wildcard or we have run out
*                 of STR characters.  If the former, set the "latest STR
*                 character was a match n wildcard" and "match n
*                 wildcard during current word" flags.
                     WILD=CS.EQ.WILDN.AND.CL.NE.ESCAPE
                     IF (WILD) MATCHN=.TRUE.

*                 If there is a movable substring under construction,
*                 the "match n" wildcard or the exhaustion of STR means
*                 that it is now ready to be searched for?
                     IF (MOVBEG.GT.0) THEN

*                    Yes: search FULSTR for the movable substring,
*                    until the match succeeds or we reach the end of
*                    FULSTR.
                        NOTFND=.TRUE.
                        DO WHILE (NOTFND.AND.IF.LE.LWBF)

*                       Set "match so far" flag.
                           SOFAR=.TRUE.

*                       Initialize a local pointer to FULSTR.
                           MIF=IF

*                       Point to the beginning of the movable substring.
                           MIS=MOVBEG

*                       Initialize "last character from movable
*                       substring".
                           MCL=' '

*                       Compare until the match fails, or we reach the
*                       end of the movable substring.
                           DO WHILE (SOFAR.AND.MIS.LE.MOVEND)

*                          Next character from movable substring.
                              MCS=STR(MIS:MIS)

*                          Proceed unless "escape".
                              IF (MCS.NE.ESCAPE.OR.MCL.NE.ESCAPE) THEN

*                             Compare one character.
                                 SOFAR=MCS.EQ.WILDA.OR.
     :                                 MCS.EQ.FULSTR(MIF:MIF)

*                             Increment the local FULSTR pointer.
                                 MIF=MIF+1
                              END IF

*                          Increment the movable substring pointer.
                              MIS=MIS+1

*                          Next comparison.
                           END DO

*                       Has the match succeeded?
                           IF (SOFAR) THEN

*                          A match has been found for the movable
*                          substring: stop searching.
                              NOTFND=.FALSE.

*                          Remember then cancel the movable substring.
                              MB=MOVBEG
                              ME=MOVEND
                              MOVBEG=0

*                          Set the global FULSTR pointer to the
*                          character following the matched substring.
                              IF=MIF
                           ELSE

*                          No match was found at the current FULSTR
*                          position: increment the global FULSTR
*                          pointer.
                              IF=IF+1

*                          Fail if we've used up all of FULSTR.
                              MATCH=IF.LE.LWBF
                           END IF

*                       Continue the search if appropriate.
                        END DO
                     END IF
                  ELSE IF (IS.LE.LWBS) THEN

*                 We have either a "match 1" wildcard or an ordinary
*                 literal character, and we have not run out of STR
*                 characters.
                     IF (WILD) THEN

*                    Previous character was a "match n" wildcard: reset
*                    the flag that told us this and start a movable
*                    substring.
                        WILD=.FALSE.
                        MOVBEG=IS
                        MOVEND=IS
                     ELSE IF (MOVBEG.GT.0) THEN

*                    A movable substring is under construction: update
*                    the end pointer.
                        MOVEND=IS
                     ELSE

*                    No movable substring is under construction: do an
*                    immediate comparison.
                        MATCH=CS.EQ.WILDA.OR.
     :                        CS.EQ.FULSTR(IF:IF)

*                    Increment the FULSTR pointer.
                        IF=IF+1
                     END IF
                  END IF

*              Increment the STR pointer.
                  IS=IS+1

*              Remember the last character.
                  CL=CS

*              Next STR character if any.
               END DO

*           If we haven't used up all of the abbreviation, make sure all
*           that is left is one or more "match n" wildcards.
               IF (IS.LE.LWBS) THEN
                  DO WHILE (MATCH.AND.IS.LE.LWBS)
                     IF (STR(IS:IS).NE.WILDN) THEN
                        MATCH=.FALSE.
                     ELSE
                        IS=IS+1
                     END IF
                  END DO

               ELSE IF (IF.LE.LWBF.AND.CL.NE.WILDN.AND.MATCHN) THEN

*              We have used up all the abbreviation.  We haven't used up
*              all of the full string, and the final abbreviation
*              character isn't a "match n" wildcard.  However, there
*              were "match n" wildcards earlier.  This is acceptable
*              only if the most recent movable substring, which was
*              successfully matched with a sequence inside FULSTR, is a
*              case of mistaken identity and it exists also at the end
*              of the full string.  Search backwards from the ends of
*              the strings to verify that this is so.

*              Reset the last and current STR characters.
                  CL=' '
                  CS=' '

*              Point to the final character of the full string.
                  MIF=LWBF

*              Point to the final character of the movable substring.
                  MIS=ME

*              Loop until the match fails or we run out of moving
*              substring.
                  DO WHILE (MATCH.AND.MIS.GE.MB)

*                 Pick up the next character from movable substring.
                     MCS=STR(MIS:MIS)

*                 Also pick up the one before that (if any).
                     IF (MIS.GT.MB) THEN
                        MCL=STR(MIS-1:MIS-1)

*                    If it was an escape, correct the pointer.
                        IF (MCL.EQ.ESCAPE) MIS=MIS-1
                     ELSE
                        MCL=' '
                     END IF

*                 Compare the characters from each of the two strings.
                     IF (MCS.EQ.FULSTR(MIF:MIF).OR.
     :                   (MCS.EQ.WILDA.AND.MCL.NE.ESCAPE)) THEN

*                    Match so far: decrement the pointers.
                        MIF=MIF-1
                        MIS=MIS-1
                     ELSE

*                    The match fails.
                        MATCH=.FALSE.
                     END IF

*                 Next character.
                  END DO
               END IF

*           Set the character pointers to the start of the next word.
               IF=LWBF+2
               IS=LWBS+2
            END IF

*        Compare the next word.
         END DO
      END IF

*  Return the result.
      hlp_COMSTR=MATCH

      END
