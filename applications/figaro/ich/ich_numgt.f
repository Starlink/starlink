C+
      SUBROUTINE ICH_NUMGT(STRING,IST,DELIMS,TERMS,NVALS,VALUES,
     :                                                ICODES,NEXT)
C
C     I C H _ N U M G T
C
C     Examines a character string containing a sequence of
C     free-format numbers and returns those numbers.  ICH_NUMGT
C     is really just a sequence of calls to ICH_NUMBR.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) STRING      (Character)  The string containing the
C                     numbers.  The numbers should be delimited
C                     by the characters in DELIMS and terminated
C                     either by the end of the string or one of
C                     the characters in TERMS.
C
C     (>) IST         (Integer) The number - from 1 - of the first
C                     character in STRING to be examined.
C
C     (>) DELIMS      (Character) The set of characters that are
C                     allowed to delimit numbers.  Note that
C                     a sequence of blanks will be treated as
C                     one blank, but a sequence of any other
C                     character will be treated as a sequence of
C                     delimiters - ie indicates null strings.
C
C     (>) TERMS       (Character) The set of characters that are
C                     allowed to terminate the string.  Note that
C                     TERMS should be a subset of DELIMS.
C
C     (>) NVALS       (Integer) The number of values expected in
C                     the string.
C
C     (<) VALUES      (Real) An array in which the values of the
C                     numbers are returned.  Null and error strings
C                     return a value of 0.
C
C     (<) ICODES      (Integer ICODES(NVALS)) in which the status
C                     of the decoding for each value is returned.
C                     0  => OK
C                     -1 => Null.  There was a null string in STRING.
C                          - ie there were two successive delimiters,
C                          or the end of the string was reached.
C                     1  => Error.  There was a non-numeric string
C                          found.
C
C     (<) NEXT        (Integer) The number of the character
C                     following the delimiter of the last value.
C                     If this is past the end of STRING, NEXT is
C                     returned as zero.
C
C     Example -
C
C     CALL ICH_NUMGT('1.2,,CRAP,3.4; ',1,' ,;',';',5,VALUES,ICODES,
C                                                             NEXT)
C
C     will return      NEXT=15
C                      VALUES(1)=1.2   ICODES(1)=0
C                      VALUES(2)=0.0   ICODES(2)=-1   Null
C                      VALUES(3)=0.0   ICODES(3)=1    Error
C                      VALUES(4)=3.4   ICODES(4)=0
C                      VALUES(5)=0.0   ICODES(5)=-1   Null
C
C                                            KS / UCL 25th June 1982
C     History:
C     25th June 1982  KS / UCL.  Original version.
C     26th Aug  1992  HME / UoE, Starlink.  Moved declaration of NVALS
C                     befor that of VALUES(NVALS).
C+
      CHARACTER*(*) STRING,DELIMS,TERMS
      INTEGER IST,NVALS,ICODES(NVALS),NEXT
      REAL VALUES(NVALS)
C
      INTEGER I,IPTR
      REAL VALUE
C
      DO I=1,NVALS
         VALUES(I)=0.
         ICODES(I)=-1
      END DO
C
      IPTR=IST
      DO NVAL=1,NVALS
         ICODES(NVAL)=ICH_NUMBR(STRING,IPTR,DELIMS,VALUE,NEXT)
         VALUES(NVAL)=VALUE
         IPTR=NEXT
         IF (NEXT.EQ.0)  GO TO 420
         IF (INDEX(TERMS,STRING(NEXT-1:NEXT-1)).NE.0)  GO TO 420
      END DO
  420 CONTINUE
C
      END
