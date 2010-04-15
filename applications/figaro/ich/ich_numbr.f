C+
      INTEGER FUNCTION ICH_NUMBR(STRING,IST,DELIMS,VALUE,NEXT)
C
C     I C H _ N U M B R
C
C     Evaluates a number encoded in a character string.
C
C     Parameters -    (">" input, ">" output)
C
C     (>) STRING      (Character) The string containing the number
C                     to be decoded.  This is generally free format
C                     decimal, but the number may be enclosed in
C                     single quotes and followed immediately by one
C                     of B (for binary), O (for octal) or X (for
C                     hex).  If hex is used, an exponent must be
C                     indicated by 'X' rather than 'E'.  Alternatively,
C                     the number may be preceeded by %X,%O or %B to
C                     indicate hex, octal or binary respectively.
C
C     (>) IST         (Integer) The position of the first character
C                     in STRING (starting from 1) at which the
C                     decoding is to start.
C
C     (>) DELIMS      (Character) A string giving the set of
C                     possible delimiters for the number.  The
C                     characters decoded are those from the first
C                     non-blank character after IST to the character
C                     before the first occurrence of a character in
C                     DELIMS (or the end of the string).
C
C     (<) VALUE       (Real) The value of the number decoded.
C
C     (<) NEXT        (Integer) The number in the string of the
C                     character following the character delimiting
C                     the number.  If the delimiting character was
C                     the last in the string, or if the number
C                     extends to the end of the string, NEXT returns
C                     as zero, as it does should the whole string
C                     be blank.
C
C     Returns -  (if called as a function)
C
C     (<) ICH_NUMBR   (Integer) Indicates the status of the
C                     evaluation.
C                     0  => OK. Number was given, and was valid
C                     -1 => Null.  There was a null string.
C                     1  => Error.  The string was not null, but it
C                          wasn't a valid number either.
C
C     Note:  Almost any sensible format is accepted.  The
C     range limits are set by the VAX hardware, and this routine
C     works in single precision.
C
C     Some examples of accepted formats -
C
C     27.3  -345E-8  -'345'O  +'1010.101'B  'ABCXD'X  '101E101'B
C
C     -%O345   %B1010.101  12345.6789  %xabcxd
C
C     Original version:                  KS / UCL  25th June 1982
C
C     Modified:
C
C     14th Feb 1984  KS / CIT  Extended to allow different bases.
C     29th Dec 1987  KS / AAO  Now accepts 'D' as well as 'E' for
C                    decimal exponents.
C
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) STRING,DELIMS
      INTEGER IST,NEXT
      REAL VALUE
C
C     Functions
C
      CHARACTER ICH_LOWER
      INTEGER ICH_VERIF,ICH_DELIM
C
C     Parameters connected with floating point dynamic range
C
      INTEGER NPLIM,NPLIMB,NPLIMO,NPLIMX
      REAL ALIM1,ALIM2
      PARAMETER (NPLIM=38,NPLIMB=126,NPLIMO=42,NPLIMX=31)
      PARAMETER (ALIM1=38.2,ALIM2=-38.5)
C
C     Local variables
C
      LOGICAL DECIMAL,HEX
      INTEGER I,IA,IBASE,IP,IPTR,IZERO,LAST,LIMN
      INTEGER N,NDIGIT,NDMAX,NPSGN,NPWR
      REAL BASE,FAC,FRACTN,POWER,RBASE,SIGN
      CHARACTER EXPCH,EXPCH2,EXPCHL,EXPCHL2,ICH
C
C     Set initial values
C
      HEX=.FALSE.
      DECIMAL=.TRUE.
      ICH_NUMBR=-1
      VALUE=0.
      FRACTN=0.
      SIGN=1.
      NPWR=0
      IZERO=ICHAR('0')
C
C     Delimit characters for number
C
      IPTR=ICH_VERIF(STRING,IST,' ')
      IF (IPTR.EQ.0) THEN
C
C        String is blank
C
         NEXT=0
         GO TO 470
      END IF
C
      LAST=ICH_DELIM(STRING,IPTR,DELIMS)
      IF (LAST.EQ.0) THEN
C
C        String is full up to end
C
         NEXT=0
         LAST=LEN(STRING)
      ELSE
C
C        String has a delimiter
C
         NEXT=LAST+1
         IF (NEXT.GT.LEN(STRING)) THEN
            NEXT=0
         END IF
         LAST=LAST-1
      END IF
      IF (LAST.LT.IPTR)  GO TO 470
C
      ICH_NUMBR=1
      ICH=STRING(IPTR:IPTR)
C
C     Check first character for a sign
C
  330 IF (ICH.EQ.'+')   GO TO 340
      IF (ICH.NE.'-')   GO TO 350
      SIGN=-1.
  340 IPTR=IPTR+1
      IF (IPTR.GT.LAST)  GO TO 470
      ICH=STRING(IPTR:IPTR)
C
C     Check for a base other than decimal
C
  350 CONTINUE
      IF (ICH.EQ.'''') THEN
         DECIMAL=.FALSE.
         IF (LAST.LT.IPTR+3) GO TO 470
         IF (STRING(LAST-1:LAST-1).NE.'''')  GO TO 470
         ICH=STRING(LAST:LAST)
         LAST=LAST-2
         IPTR=IPTR+1
      ELSE IF (ICH.EQ.'%') THEN
         DECIMAL=.FALSE.
         IF (LAST.LT.IPTR+2)  GO TO 470
         ICH=STRING(IPTR+1:IPTR+1)
         IPTR=IPTR+2
      END IF
      IF (.NOT.DECIMAL) THEN
         IF ((ICH.EQ.'B').OR.(ICH.EQ.ICH_LOWER('B'))) THEN
            BASE=2.
            LIMN=1
            NDMAX=NPLIMB
         ELSE IF ((ICH.EQ.'O').OR.(ICH.EQ.ICH_LOWER('O'))) THEN
            BASE=8.
            LIMN=7
            NDMAX=NPLIMO
         ELSE IF ((ICH.EQ.'X').OR.(ICH.EQ.ICH_LOWER('X'))) THEN
            BASE=16.
            LIMN=22
            NDMAX=NPLIMX
            HEX=.TRUE.
         ELSE
            GO TO 470
         END IF
         ICH=STRING(IPTR:IPTR)
      ELSE
         BASE=10.
         LIMN=9
         NDMAX=NPLIM
      END IF
C
C     Set other quantities that depend on base
C
      IBASE=BASE
      RBASE=1./BASE
      IF (HEX) THEN
         EXPCH='X'
         EXPCHL=ICH_LOWER('X')
         EXPCH2=EXPCH
         EXPCHL2=EXPCHL
      ELSE
         EXPCH='E'
         EXPCHL=ICH_LOWER('E')
         EXPCH2='D'
         EXPCHL2=ICH_LOWER('D')
      END IF
C
C     Find length of integer part, if any.
C
      DO 360 IP=IPTR,LAST
         ICH=STRING(IP:IP)
         IF (ICH.EQ.'.'.OR.ICH.EQ.EXPCH.OR.ICH.EQ.EXPCHL
     :                .OR.ICH.EQ.EXPCH2.OR.ICH.EQ.EXPCHL2) GO TO 370
  360 CONTINUE
      IP=LAST+1
C
C     Evaluate Integer part
C
  370 IA=IP
      FAC=1.
      NDIGIT=1
  380 IA=IA-1
      IF (IA.LT.IPTR)   GO TO 385
      ICH=STRING(IA:IA)
      IF (HEX) CALL ICH_FOLD(ICH)
      N=ICHAR(ICH)-IZERO
      IF (N.LT.0.OR.N.GT.LIMN)   GO TO 470
      IF (HEX.AND.(N.GT.9)) THEN
         IF (N.LT.17)   GO TO 470
         N=N-7
      END IF
      VALUE=VALUE+FLOAT(N)*FAC
      FAC=FAC*BASE
      NDIGIT=NDIGIT+1
      IF (NDIGIT.GT.NDMAX)  GO TO 470
      GO TO 380
C
C     Evaluate fractional part, if one exists
C
  385 ICH=STRING(IP:IP)
      IF (ICH.NE.'.')   GO TO 410
      FAC=RBASE
  400 IP=IP+1
      IF (IP.GT.LAST)   GO TO 440
      ICH=STRING(IP:IP)
      IF (ICH.EQ.EXPCH.OR.ICH.EQ.EXPCHL
     :           .OR.ICH.EQ.EXPCH2.OR.ICH.EQ.EXPCHL2)   GO TO 415
      IF (HEX) CALL ICH_FOLD(ICH)
      N=ICHAR(ICH)-IZERO
      IF (N.LT.0.OR.N.GT.LIMN)   GO TO 470
      IF (HEX.AND.(N.GT.9)) THEN
         IF (N.LT.17)   GO TO 470
         N=N-7
      END IF
      FRACTN=FRACTN+FLOAT(N)*FAC
      FAC=FAC*RBASE
      GO TO 400
C
C     Evaluate any exponent
C
  410 CONTINUE
      IF (ICH.NE.EXPCH.AND.ICH.NE.EXPCHL
     :         .AND.ICH.NE.EXPCH2.AND.ICH.NE.EXPCHL2)  GO TO 440
  415 IP=IP+1
      NPSGN=1
      ICH=STRING(IP:IP)
      IF (ICH.EQ.'+')   GO TO 420
      IF (ICH.NE.'-')   GO TO 430
      NPSGN=-1
  420 IP=IP+1
  430 CONTINUE
      NPWR=0
      DO I=IP,LAST
         ICH=STRING(I:I)
         IF (HEX) CALL ICH_FOLD(ICH)
         N=ICHAR(ICH)-IZERO
         IF (N.LT.0.OR.N.GT.LIMN) GO TO 470
         IF (HEX.AND.(N.GT.9)) THEN
            IF (N.LT.17)   GO TO 470
            N=N-7
         END IF
         NPWR=NPWR*IBASE+N
         IF (NPWR.GT.NDMAX)  GO TO 470
      END DO
C
C     Combine VALUE,FRACTN,SIGN,NPWR & NPSGN to get result
C
  440 CONTINUE
      VALUE=(VALUE+FRACTN)*SIGN
      IF (NPWR.EQ.0)   GO TO 460
      FAC=BASE
      IF (NPSGN.LT.0)   FAC=RBASE
      POWER=1.
      DO 450 I=1,NPWR
         POWER=POWER*FAC
  450 CONTINUE
      IF (VALUE.EQ.0.)   GO TO 470
      FAC=ALOG10(ABS(VALUE))+ALOG10(POWER)
      IF (FAC.GT.ALIM1.OR.FAC.LT.ALIM2)   GO TO 470
      VALUE=VALUE*POWER
C
C     Normal return
C
  460 CONTINUE
      ICH_NUMBR=0
      GO TO 500
C
C     Error return
C
  470 CONTINUE
      VALUE=0.
C
  500 CONTINUE
C
      END
