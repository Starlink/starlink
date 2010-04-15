      CHARACTER FUNCTION ICH_LOWER(CHR)
C
C     I C H _ L O W E R
C
C     Returns the lower-case character corresponding to an
C     upper-case character.
C
C     Parameter -     (">" input, "<" output)
C
C     (>) CHR         (Character) The character in question.
C                     If CHR is a string, only the first character
C                     is considered.
C
C     Returns -
C
C     (<) ICH_LOWER   (Character) If CHR is an upper-case character,
C                     the corresponding lower-case character is returned.
C                     Otherwise, the character as passed is returned.
C
C     Note that this version is for ASCII characters.
C
C                                           KS / UCL  17th June 1982
C+
      CHARACTER*(*) CHR
C
      IF ((CHR(1:1).GE.'A').AND.(CHR(1:1).LE.'Z')) THEN
        ICH_LOWER=CHAR(ICHAR(CHR(1:1))+32)
      ELSE
         ICH_LOWER=CHR(1:1)
      END IF
C
      END
      INTEGER FUNCTION ICH_FOLD(STRING)
C
C     I C H _ F O L D
C
C     Converts all lower case ASCII characters in a character
C     string to their upper case equivalents.
C
C     Parameters -   ("!" modified, "<" output)
C
C     (!) STRING    (Character) The string to be folded.
C
C     Returns -  (if called as a function)
C
C     (<) ICH_FOLD  (Integer) The number - starting from 1 -
C                   of the last non-blank character in the
C                   string.  In some cases this can be the
C                   logical length of the string.
C
C                                 KS / UCL  17th June 1982
C+
      CHARACTER*(*) STRING
C
      CHARACTER CHR,LITLA,LITLZ,ICH_LOWER
C
C     Setting little a and little z like this ensures that
C     the program can survive conversion into upper case.
C
      LITLA=ICH_LOWER('A')
      LITLZ=ICH_LOWER('Z')
C
      ICH_FOLD=0
      DO I=1,LEN(STRING)
         CHR=STRING(I:I)
         IF ((CHR.GE.LITLA).AND.(CHR.LE.LITLZ)) THEN
           STRING(I:I)=CHAR(ICHAR(CHR)-32)
            ICH_FOLD=I
         ELSE
            IF (CHR.NE.' ') THEN
               ICH_FOLD=I
            END IF
         END IF
      END DO
C
      END
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
      INTEGER ICH_VERIF,ICH_DELIM,ICH_FOLD
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
      INTEGER N,NDIGIT,NDMAX,NPSGN,NPWR,IFR
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
      IBASE=INT(BASE)
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
      IF (HEX) THEN
         IFR = ICH_FOLD(ICH)
      END IF
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
      IF (HEX) THEN
         IFR = ICH_FOLD(ICH)
      END IF
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
         IF (HEX) THEN
            IFR = ICH_FOLD(ICH)
         END IF
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
C+
      INTEGER FUNCTION ICH_DELIM(STRING,IST,CHARS)
C
C     I C H _ D E L I M
C
C     Returns the position of the first character in a string
C     that matches one of a specified set of characters.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) STRING     (Character) The string to be examined.
C
C     (>) IST        (Integer) The number of the character at
C                    which the search should start.  Characters
C                    are numbered from 1 up.  If IST is 0 or
C                    negative the search starts at the start of
C                    the string - as though IST were 1.
C
C     (>) CHARS      (Character) The specified set of characters.
C
C     Returns -
C
C     (<) ICH_DELIM  (Integer) The number of the first character
C                    in the string - from IST on - which is also
C                    one of the characters in CHARS.  If there
C                    is no such character, ICH_DELIM returns 0.
C
C     Note that the name comes from the use of this routine
C     to search for delimiters in a string.
C
C     Example:
C
C     ICH_DELIM('This is a string with a semi-colon ;  ',1,CHARS)
C
C     will return 5 if CHARS=' ',  36 if CHARS=';',
C     and 5 if CHARS='; '
C
C                                      KS / UCL  15th June 1982
C+
      CHARACTER*(*) STRING,CHARS
      INTEGER IST
C
      INTEGER NST,I
C
      IF ((IST.LT.1).OR.(IST.GT.LEN(STRING))) THEN
         NST=1
      ELSE
         NST=IST
      END IF
C
      ICH_DELIM=0
      DO I=NST,LEN(STRING)
         IF (INDEX(CHARS,STRING(I:I)).NE.0) THEN
            ICH_DELIM=I
            GO TO 340
         END IF
      END DO
  340 CONTINUE
C
      END
      INTEGER FUNCTION ICH_VERIF(STRING,IST,CHARS)
C
C     I C H _ V E R I F
C
C     Returns the position of the first character in a string
C     that is not one of a specified set of characters.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) STRING     (Character) The string to be examined.
C
C     (>) IST        (Integer) The number of the character at
C                    which the search should start.  Characters
C                    are numbered from 1 up.  If IST is 0 or
C                    negative the search starts at the start of
C                    the string - as though IST were 1.
C
C     (>) CHARS      (Character) The specified set of characters.
C
C     Returns -
C
C     (<) ICH_VERIF  (Integer) The number of the first character
C                    in the string - from IST on - which is not
C                    one of the characters in CHARS.  If there
C                    is no such character, ICH_VERIF returns 0.
C
C     Example:
C
C     ICH_IVERIF('1234567890ABCDE',1,'0123456789')
C
C     will return the value 11 (the position of the 'A')
C
C     ICH_IVERIF('     ABCDEFG  ',6,'ABCDEF')
C
C     will return the value 12 (the position of the 'G'
C     after the 'A' which is the 6th character)
C
C                                    KS / UCL  15th June 1982
C+
      CHARACTER*(*) STRING,CHARS
      INTEGER IST
C
      INTEGER NST,I
C
      IF ((IST.LT.1).OR.(IST.GT.LEN(STRING))) THEN
         NST=1
      ELSE
         NST=IST
      END IF
C
      ICH_VERIF=0
      DO I=NST,LEN(STRING)
         IF (INDEX(CHARS,STRING(I:I)).EQ.0) THEN
            ICH_VERIF=I
            GO TO 340
         END IF
      END DO
  340 CONTINUE
C
      END
C+
C                           I C H _ E N C O D E
C
C     Routine name:
C           ICH_ENCODE
C
C     Function:
C           Formats a REAL number into a character string
C
C     Decription:
C           This routine formats a real variable into a character string
C           in a nice way; that is, if the number is an integer (within
C           the integer 16 bit range), it will be printed as such, and
C           'E' format will only be used if the number is particularly
C           large or small.  Generally, an ordinary floating point format
C           will be used.
C
C     Language:
C           FORTRAN:
C
C     Call:
C           STATUS=ICH_ENCODE(FIELD,VALUE,IPTR,IDECC,NPTR)
C
C     Parameters:      (">" input, "<" output)
C
C          (>) FIELD   (Fixed string, descr) The character string into
C                      which VALUE is to be encoded.
C          (>) VALUE   (Real, ref) The number to be encoded
C          (>) IPTR    (Integer, ref) quantity giving the number of
C                      the first byte in FIELD (starting at 1) into
C                      which the ASCII encoded value is to be written.
C          (>) IDDEC   (Integer, ref) One less than the number of
C                      significant figures to be aimed at if a floating
C                      point output format is used.  This is the number of
C                      decimal places used in an E format (although the
C                      routine tries to avoid E format, and will print eg
C                      1234.5678 as 1234.6 for any value of IDECC less
C                      than 5).
C          (<) NPTR    (Integer, ref) Variable in which ENCODE
C                      returns the number of the next available character
C                      in FIELD.
C
C     Returns:         (if called as a function)
C
C          (<) STATUS  (Integer, function value) Return code.
C                      0 => OK,
C                      1 => Not enough character positions left in FIELD.
C
C     External variables used: None
C
C     External routines used: None
C
C     Author: K. Shortridge.
C
C     Date: 24th July 1986
C
C     Note:
C           This routine is not intended to give precise control over
C           formats.  The intention is that it will always produce a
C           sensibly formatted result, using IDECC as a guide to the
C           precision required.  Setting IDECC=6 gives a result at the
C           maximum precision available in single precision on the VAX.
C
C     Internal declarations:
C           INTEGER FUNCTION ICH_ENCODE(FIELD,VALUE,IPTR,IDECC,NPTR)
C           CHARACTER*(*) FIELD
C           INTEGER IPTR,NPTR,IDECC
C           REAL VALUE
C
C     Keywords:
C           Strings, free-format, encode
C-
C     Modifications:
C           19th July 1983.  Original VAX version.  KS / UCL, based on
C                            an Interdata program by Tony Hewer, UCL.
C           24th July 1986.  Complete re-write to use Fortran internal
C                            I/O.  Use of IDECC changed slightly - it
C                            used to be the number of decimal places,
C                            meaning that with IDECC=3 a number such as
C                            0.02345 would output as 0.023, whereas it
C                            is now output as 0.02345, and 0.002345 used
C                            to give 0.002 and now gives 2.345E-3
C+
      INTEGER FUNCTION ICH_ENCODE(FIELD,VALUE,IPTR,IDECC,NPTR)
C
      IMPLICIT NONE
C
      CHARACTER*(*) FIELD
      INTEGER IPTR,NPTR,IDECC
      REAL VALUE
C
C     Local variables
C
      INTEGER   I
*       Loop variable
      INTEGER   IGNORE
*       Dummy status value for write
      INTEGER   IST
*       First non-blank character
      INTEGER   ISTDIG
*       First digit of fractional part
      INTEGER   LASTCH
*       Last non-blank character
      INTEGER   LSTDIG
*       Last digit of fractional part
      INTEGER   LSTSIG
*       Last significant fract. digit
      INTEGER   NDEC
*       Formatted # of decimal places
      INTEGER   NLOG
*       Integer log of value
      CHARACTER WORK*13
*       Work string
      LOGICAL   USE_FLOAT
*       Floating point format needed
      REAL      VLOG
*       Log of value
C
C     See if we can get away with using an integer format.
C
      LASTCH=LEN(WORK)
      USE_FLOAT=.TRUE.
      IF ((VALUE.LE.65535.0).AND.(VALUE.GE.-65536.0)) THEN
         IF (FLOAT(INT(VALUE)).EQ.VALUE) THEN
            WRITE (WORK,'(I6)',IOSTAT=IGNORE) INT(VALUE)
            USE_FLOAT=.FALSE.
            LASTCH=6
         END IF
      END IF
      IF (USE_FLOAT) THEN
C
C        We need a floating point format.  See if we will need to
C        use an 'E' type format.
C
         IF (VALUE.EQ.0.0) THEN
            VLOG=0.0
         ELSE
            VLOG=LOG10(ABS(VALUE))
         END IF
         IF (VLOG.LT.0.0) THEN
            NLOG=INT(VLOG)-1
         ELSE
            NLOG=INT(VLOG)
         END IF
         IF ((NLOG.GT.5).OR.(NLOG.LT.-2)) THEN
C
C           Yes, we do need an 'E' format. The number of decimal
C           places is IDECC.
C
            NDEC=MAX(1,MIN(IDECC,6))
            WRITE (WORK,'(1PE13.'//CHAR(NDEC+ICHAR('0'))//')',
     :                                     IOSTAT=IGNORE) VALUE
            LSTDIG=9
            ISTDIG=10-NDEC
            IF (WORK(12:12).EQ.'0') THEN
               WORK(12:)=WORK(13:)
               LASTCH=12
            END IF
         ELSE
C
C           Not an 'E' format.  The number of decimal places depends
C           on the significance required (IDECC) and the value itself.
C
            NDEC=MAX(1,MIN(IDECC-NLOG,7-NLOG))
            WRITE (WORK,'(F13.'//CHAR(NDEC+ICHAR('0'))//')',
     :                                     IOSTAT=IGNORE) VALUE

            LSTDIG=13
            ISTDIG=14-NDEC
         END IF
C
C        See if any of the trailing decimal figures are zero
C
         LSTSIG=LSTDIG
         DO I=ISTDIG+1,LSTDIG
            IF (WORK(I:I).NE.'0') LSTSIG=I
         END DO
         IF (LSTSIG.LT.LSTDIG) THEN
            IF (LSTDIG.EQ.LEN(WORK)) THEN
               WORK(LSTSIG+1:)=' '
            ELSE
               WORK(LSTSIG+1:)=WORK(LSTDIG+1:)
            END IF
            LASTCH=LASTCH-(LSTDIG-LSTSIG)
         END IF
      END IF
C
C     Now find the first non-blank character.
C
      DO I=1,LASTCH
         IF (WORK(I:I).NE.' ') THEN
            IST=I
            GO TO 320
         END IF
      END DO
      IST=LASTCH
  320 CONTINUE
C
C     Now copy the result into the return string, if it will fit
C
      NPTR=IPTR+LASTCH-IST+1
      IF (NPTR.LE.LEN(FIELD)) THEN
         ICH_ENCODE=0
         FIELD(IPTR:)=WORK(IST:LASTCH)
      ELSE
         ICH_ENCODE=1
         NPTR=IPTR
      END IF
C
      END
C+
