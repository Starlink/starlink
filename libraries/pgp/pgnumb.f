C*PGNUMB -- convert a number into a plottable character string
C%void cpgnumb(int mm, int pp, int form, char *string, \
C% int *string_length);
C+
      SUBROUTINE PGNUMB (MM, PP, FORM, STRING, NC)
      INTEGER MM, PP, FORM
      CHARACTER*(*) STRING
      INTEGER NC
C
C This routine converts a number into a decimal character
C representation. To avoid problems of floating-point roundoff, the
C number must be provided as an integer (MM) multiplied by a power of 10
C (10**PP).  The output string retains only significant digits of MM,
C and will be in either integer format (123), decimal format (0.0123),
C or exponential format (1.23x10**5). Standard escape sequences \u, \d 
C raise the exponent and \x is used for the multiplication sign.
C This routine is used by PGBOX to create numeric labels for a plot.
C
C Formatting rules:
C   (a) Decimal notation (FORM=1):
C       - Trailing zeros to the right of the decimal sign are
C         omitted
C       - The decimal sign is omitted if there are no digits
C         to the right of it
C       - When the decimal sign is placed before the first digit
C         of the number, a zero is placed before the decimal sign
C       - The decimal sign is a period (.)
C       - No spaces are placed between digits (ie digits are not
C         grouped in threes as they should be)
C       - A leading minus (-) is added if the number is negative
C   (b) Exponential notation (FORM=2):
C       - The exponent is adjusted to put just one (non-zero)
C         digit before the decimal sign
C       - The mantissa is formatted as in (a), unless its value is
C         1 in which case it and the multiplication sign are omitted
C       - If the power of 10 is not zero and the mantissa is not
C         zero, an exponent of the form \x10\u[-]nnn is appended,
C         where \x is a multiplication sign (cross), \u is an escape
C         sequence to raise the exponent, and as many digits nnn
C         are used as needed
C   (c) Automatic choice (FORM=0):
C         Decimal notation is used if the absolute value of the
C         number is less than 10000 or greater than or equal to
C         0.01. Otherwise exponential notation is used.
C
C Arguments:
C  MM     (input)
C  PP     (input)  : the value to be formatted is MM*10**PP.
C  FORM   (input)  : controls how the number is formatted:
C                    FORM = 0 -- use either decimal or exponential
C                    FORM = 1 -- use decimal notation
C                    FORM = 2 -- use exponential notation
C  STRING (output) : the formatted character string, left justified.
C                    If the length of STRING is insufficient, a single
C                    asterisk is returned, and NC=1.
C  NC     (output) : the number of characters used in STRING:
C                    the string to be printed is STRING(1:NC).
C--
C 23-Nov-1983
C  9-Feb-1988 [TJP] - Use temporary variable to avoid illegal character
C                     assignments; remove non-standard DO loops.
C 15-Dec-1988 [TJP] - More corrections of the same sort.
C 27-Nov-1991 [TJP] - Change code for multiplication sign.
C 23-Jun-1994 [TJP] - Partial implementation of FORM=1 and 2.
C-----------------------------------------------------------------------
      CHARACTER*1 BSLASH
      CHARACTER*2 TIMES, UP, DOWN
      CHARACTER*20 WORK, WEXP, TEMP
      INTEGER M, P, ND, I, J, K, NBP
      LOGICAL MINUS
C
C Define backslash (escape) character and escape sequences.
C
      BSLASH = CHAR(92)
      TIMES  = BSLASH//'x'
      UP     = BSLASH//'u'
      DOWN   = BSLASH//'d'
C
C Zero is always printed as "0".
C
      IF (MM.EQ.0) THEN
          STRING = '0'
          NC = 1
          RETURN
      END IF
C
C If negative, make a note of that fact.
C
      MINUS = MM.LT.0
      M = ABS(MM)
      P = PP
C
C Convert M to a left-justified digit string in WORK. As M is a
C positive integer, it cannot use more than 10 digits (2147483647).
C
      J = 10
   10 IF (M.NE.0) THEN
          K = MOD(M,10)
          M = M/10
          WORK(J:J) = CHAR(ICHAR('0')+K)
          J = J-1
       GOTO 10
      END IF
      TEMP = WORK(J+1:)
      WORK = TEMP
      ND = 10-J
C
C Remove right-hand zeros, and increment P for each one removed.
C ND is the final number of significant digits in WORK, and P the
C power of 10 to be applied. Number of digits before decimal point
C is NBP.
C
   20 IF (WORK(ND:ND).EQ.'0') THEN
          ND = ND-1
          P = P+1
       GOTO 20
      END IF
      NBP = ND+MIN(P,0)
C
C Integral numbers of 4 or less digits are formatted as such.
C
      IF ((P.GE.0) .AND. ((FORM.EQ.0 .AND. P+ND.LE.4) .OR.
     :                    (FORM.EQ.1 .AND. P+ND.LE.10))) THEN
          DO 30 I=1,P
              ND = ND+1
              WORK(ND:ND) = '0'
   30     CONTINUE
          P = 0
C
C If NBP is 4 or less, simply insert a decimal point in the right place.
C
      ELSE IF (FORM.NE.2.AND.NBP.GE.1.AND.NBP.LE.4.AND.NBP.LT.ND) THEN
          TEMP = WORK(NBP+1:ND)
          WORK(NBP+2:ND+1) = TEMP
          WORK(NBP+1:NBP+1) = '.'
          ND = ND+1
          P = 0
C
C Otherwise insert a decimal point after the first digit, and adjust P.
C
      ELSE
          P = P + ND - 1
          IF (FORM.NE.2 .AND. P.EQ.-1) THEN
              TEMP = WORK
              WORK = '0'//TEMP
              ND = ND+1
              P = 0
          ELSE IF (FORM.NE.2 .AND. P.EQ.-2) THEN
              TEMP = WORK
              WORK = '00'//TEMP
              ND = ND+2
              P = 0
          END IF
          IF (ND.GT.1) THEN
              TEMP = WORK(2:ND)
              WORK(3:ND+1) = TEMP
              WORK(2:2) = '.'
              ND = ND + 1
          END IF
      END IF
C
C Add exponent if necessary.
C
      IF (P.NE.0) THEN
          WORK(ND+1:ND+6) = TIMES//'10'//UP
          ND = ND+6
          IF (P.LT.0) THEN
              P = -P
              ND = ND+1
              WORK(ND:ND) = '-'
          END IF
          J = 10
   40     IF (P.NE.0) THEN
              K = MOD(P,10)
              P = P/10
              WEXP(J:J) = CHAR(ICHAR('0')+K)
              J = J-1
           GOTO 40
          END IF
          WORK(ND+1:) = WEXP(J+1:10)
          ND = ND+10-J
          IF (WORK(1:3).EQ.'1'//TIMES) THEN
              TEMP = WORK(4:)
              WORK = TEMP
              ND = ND-3
          END IF
          WORK(ND+1:ND+2) = DOWN
          ND = ND+2
      END IF
C
C Add minus sign if necessary and move result to output.
C
      IF (MINUS) THEN
         TEMP = WORK(1:ND)
         STRING = '-'//TEMP
         NC = ND+1
      ELSE
         STRING = WORK(1:ND)
         NC = ND
      END IF
C
C Check result fits.
C
      IF (NC.GT.LEN(STRING)) THEN
          STRING = '*'
          NC = 1
      END IF
      END
