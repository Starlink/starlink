*+  ECHOMOP - ECH_PAR_OBJECT
*
      SUBROUTINE ECH_PAR_OBJECT (STRING,IST,LIM1,LIM2,EQULS,TERM,NEXT)
*
*    Description :
*
*     Find next object in text string
*
*     Keywords : UTILITY
*
C     Delimits the next object in a string.  In this context, an
C     object is a string of non-blank characters, terminated either
C     by a blank, comma or '=', or a string of characters enclosed in
C     " marks or within brackets '()'.  This routine also looks past
C     the object to see where the next object starts, and to see if
C     the object is followed by an = sign.
C
C     Parameters  -  (">" input, "<" output)
C
C     (>) STRING    (Character) The string in question.
C     (>) IST       (Integer) The position of the character in the
C                   string at which the search is to start.  In
C                   successive calls to OBJECT, this will usually
C                   be the value of NEXT returned by the previous
C                   call. If IST is illegal (0=>IST) LIM1 and
C                   NEXT are both returned as zero.
C     (<) LIM1      (Integer) The first character of the object.
C                   If there is no object to be found, LIM1 is
C                   set to zero.  If the string is a character or
C                   bracketed string, LIM1 is the position of the first
C                   " mark or left bracket.
C     (<) LIM2      (Integer) The last character of the object.
C                   If no object was found, LIM2 is set to zero.
C     (<) EQULS     (Logical) True if the object is followed by
C                   an equals sign.
C     (<) TERM      (Logical) True if the object is followed by
C                   a comma or by the end of the string.
C     (<) NEXT      (Integer) The position of the first non-blank
C                   character following the object.  If the object
C                   is followed by a comma or an equals sign, NEXT
C                   is the position of the first non-blank
C                   character after that.  If there is no such
C                   character, NEXT is set to zero.
C
C     Subroutines / functions used -
C
C     ICH_VERIF  (ICH_ package) Find first char not in a given set
C     ICH_DELIM  ( "     "    ) Find first char in a given set
C
C                                     KS / CIT  13th Feb 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EQULS,TERM
      INTEGER IST,LIM1,LIM2,NEXT
      CHARACTER*(*) STRING
C
C     Functions
C
      INTEGER ICH_VERIF,ICH_DELIM,LEN
C
C     Local variables
C
      LOGICAL DELMT
      CHARACTER CHR,MATCHR
C
      EQULS=.FALSE.
      TERM=.FALSE.
C
C     Check for IST value OK
C
      IF (IST.LE.0) THEN
         LIM1=0
         LIM2=0
         NEXT=0
      ELSE
C
C        Find first non-blank char
C
         LIM1=ICH_VERIF(STRING,IST,' ')
         IF (LIM1.EQ.0) THEN
C
C           There isn't one
C
            LIM2=LEN(STRING)
         ELSE
C
C           There is one, is it a comma?
C
            CHR=STRING(LIM1:LIM1)
            IF (CHR.EQ.',') THEN
C
C              Yes, so we have a null string
C
               LIM2=LIM1-1
               LIM1=0
            ELSE
C
C              No, it's real, is it a character string or vector?
C
               IF ((CHR.EQ.'"').OR.(CHR.EQ.'(')) THEN
                  IF (LIM1.EQ.LEN(STRING)) THEN
C
C                    Single " or ( at end of string - a rather perverse case!
C
                     TERM=.TRUE.
                     LIM2=LIM1
                  ELSE
C
C                    Look for end of string
C
                     MATCHR=CHR
                     IF (CHR.EQ.'(') MATCHR=')'
                     LIM2=ICH_DELIM(STRING,LIM1+1,MATCHR)
                     IF (LIM2.EQ.0) LIM2=LEN(STRING)
                  END IF
               ELSE
C
C                 Not a char string, so look for delimiter
C
                  LIM2=LIM1
                  DELMT=.FALSE.
                  DO WHILE(.NOT.DELMT)
                     LIM2=ICH_DELIM(STRING,LIM2,' [=,')-1
                     IF (LIM2.LT.0) THEN
                        LIM2=LEN(STRING)
                        DELMT=.TRUE.
                     ELSE
C
C                       Be careful with [ ] pairs.. they could
C                       appear in an object name enclosing a comma.
C
                        IF (STRING(LIM2+1:LIM2+1).EQ.'[') THEN
                           LIM2=ICH_DELIM(STRING,LIM2,']')
                           IF (LIM2.EQ.0) THEN
                              LIM2=LEN(STRING)
                              DELMT=.TRUE.
                           END IF
                        ELSE
                           DELMT=.TRUE.
                        END IF
                     END IF
                  END DO
               END IF
            END IF
         END IF
C
C        Now see what comes after the object, if anything.
C
         IF (LIM2.GE.LEN(STRING)) THEN
C
C           Nothing
C
            NEXT=0
            TERM=.TRUE.
         ELSE
C
C           Might be something.  Look for non-blank character.
C
            NEXT=ICH_VERIF(STRING,LIM2+1,' ')
            IF (NEXT.EQ.0) THEN
C
C              There isn't one
C
               TERM=.TRUE.
            ELSE
C
C              There is one, see if it's = or ,
C
               CHR=STRING(NEXT:NEXT)
               IF ((CHR.EQ.'=').OR.(CHR.EQ.',')) THEN
C
C                 It is, so we don't really want this; we want the
C                 next non-blank char.
C
                  IF (NEXT.EQ.LEN(STRING)) THEN
                     NEXT=0
                  ELSE
                     NEXT=ICH_VERIF(STRING,NEXT+1,' ')
                  END IF
C
C                 Then set EQULS or TERM (note, if not =, must be comma)
C
                  IF (CHR.EQ.'=') THEN
                     EQULS=.TRUE.
                  ELSE
                     TERM=.TRUE.
                  END IF
               END IF
            END IF
         END IF
      END IF
C
      END
