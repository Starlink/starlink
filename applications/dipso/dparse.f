!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
      INTEGER FUNCTION DPARSE(STRING,SUBSTR,DELIMS,NDELIM)
C
C Subroutine to extract a substring SUBSTR from STRING where DELIMS contains
C a list of acceptable terminators for the substring.  NDELIM returns the
C number of the terminator used, 0 if no acceptable terminator was found or
C STRING is reduced to a blank after the substring is removed.
C
C Declaration of subroutine parameters
C
      CHARACTER STRING*(*), SUBSTR*(*), DELIMS*(*)
      INTEGER NDELIM
C
C Declaration of subroutine variables
C
      INTEGER LENSTR, LENDEL, ENDSUB, I
C
C Determine length of STRING and the number of possible delimiters
C Assume initially that no delimiter will be found.
C
      LENSTR=LEN(STRING)
      LENDEL=LEN(DELIMS)
      ENDSUB=LENSTR+1
      NDELIM=0
C
C Find first occurrence of each delimiter in STRING, and update ENDSUB to
C contain the first occurrence of any delimiter.
C
      DO 1 I=1,LENDEL
        DPOSN=INDEX(STRING,DELIMS(I:I))
        IF ( DPOSN.NE.0 .AND. DPOSN.LT.ENDSUB ) THEN
          NDELIM=I
          ENDSUB=DPOSN
        END IF
    1 CONTINUE
C
C Split STRING around the delimiter
C
      IF ( ENDSUB.EQ.1 ) THEN
        SUBSTR=' '
      ELSE
        SUBSTR=STRING(1:ENDSUB-1)
      END IF
      IF ( ENDSUB.LT.LENSTR ) THEN
        STRING=STRING(ENDSUB+1:)
      ELSE
        STRING=' '
      END IF
      IF ( STRING.EQ.' ' ) THEN
        NDELIM=0
      END IF
      DPARSE=ENDSUB-1
      END
