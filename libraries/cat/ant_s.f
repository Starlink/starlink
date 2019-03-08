
*+
*  Name:
*     ant_s

*  Purpose:
*     String handling routines

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     CGP: Clive Page
*     ACD: A C Davenhall (Edinburgh)

*  History:
*     1993-JUL-27 (CGP):
*        Original Version
*     1999-AUG-13 (ACD):
*        Modified the error reporting to use the ADAM libraries rather than
*        Fortran WRITE statements.
*-

*+ANT_STOMJD  Converts string date/time to double precision MJD
      SUBROUTINE ANT_STOMJD(STRING, DMJD, STATUS)
      CHARACTER STRING*(*)
      DOUBLE PRECISION DMJD
      INTEGER STATUS
*STRING input  Date and optional time, {may be enclosed in braces}
*              any reasonable format e.g. {12-JAN-1990 12:34:50}
*MJDATE output Modified Julian Date
*STATUS in/out Inherited status
*-Author  Clive Page  1993-JULY-13
      INTEGER K, IYEAR, MONTH, IDAY, IPOS, ISTAT, ITEMP
      CHARACTER WORD*4
      DOUBLE PRECISION HOURS
      CHARACTER DATEBF*12
      INTEGER LSTAT
*
*Get first symbol which should be an integer of up to 4 digits
      K = 1
      IF(STRING(1:1) .EQ. '{') K = 2
      CALL ANT_SCNSYM(2, STRING, K, WORD)
      READ(WORD, '(BN,I4)', IOSTAT=STATUS) IYEAR
      IF(STATUS .NE. 0) THEN
         CALL ANT_ERRC ('ANT_STOMJD: invalid date,', STRING)
C        WRITE(*,*)'!ANT_STOMJD: invalid date ', STRING
         STATUS = -9
         GO TO 999
      END IF
*Advance past terminator
      K = K + 1
*Get next symbol which could be month name or number
      CALL ANT_SCNSYM(3, STRING, K, WORD)
      IF(WORD(1:1) .GE. '0' .AND. WORD(1:1) .LE. '9') THEN
*Seems to be a numeric string, convert to number (error ignored
* as will be flagged later)
         READ(WORD, '(BN,I4)', IOSTAT=ISTAT) MONTH
      ELSE
         CALL ANT_SUPPER(WORD)
         IPOS = INDEX(
     $    'JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC JLY ', WORD)
         IF(MOD(IPOS,4) .EQ. 1) THEN
            MONTH = 1 + IPOS/4
*  allow 'JLY' as an alternative to 'JUL'
            IF(MONTH .EQ. 13) MONTH = 7
         ELSE
            MONTH = 0
         END IF
      END IF
*Skip another terminator
      K = K + 1
*Get third symbol which should be an integer of up to 4 digits
      CALL ANT_SCNSYM(2, STRING, K, WORD)
      READ(WORD, '(BN,I4)', IOSTAT=STATUS) IDAY
      IF(STATUS .NE. 0) THEN
         CALL ANT_ERRC ('ANT_STOMJD: invalid date,', STRING)
C        WRITE(*,*)'!ANT_STOMJD: invalid date,', STRING
         STATUS = -9
         GO TO 999
      END IF
*Swap first and third values if the third one is obviously the year
      IF(IYEAR .LE. 31 .AND. IDAY .GT. 31)THEN
         ITEMP = IYEAR
         IYEAR = IDAY
         IDAY  = ITEMP
      END IF
*If year value less than 100 assume it is just last 2 digits of year
      IF(IYEAR .LT. 100) IYEAR = IYEAR + 1900
*Intentionally allow IDAY = 0 for special case of zeroth of month
      IF( IYEAR .GT. 1900 .AND. IYEAR .LE. 2099 .AND.
     $    MONTH .GE.    1 .AND. MONTH .LE.   12 .AND.
     $    IDAY  .GE.    0 .AND. IDAY  .LE.   31) THEN
         DMJD = DBLE(367*(IYEAR-1900) + 275*MONTH/9 + IDAY + 18313 -
     $         7*(IYEAR+((MONTH+9)/12))/4)
      ELSE
         DATEBF = ' '
         WRITE(DATEBF, '(I5, I3, I3)', IOSTAT=LSTAT)
     $     IYEAR, MONTH, IDAY
         CALL ANT_ERRC ('ANT_STOMJD: invalid date,', DATEBF)
C        WRITE(*,*)'!ANT_STOMJD: invalid date', IYEAR, MONTH, IDAY
         STATUS = -9
         GO TO 999
      END IF
*A time string may follow: skip another terminator
      K = K + 1
      IF(K .LT. LEN(STRING) .AND. INDEX(STRING,':') .NE. 0) THEN
         CALL ANT_SEX2D(STRING(K:), HOURS, STATUS)
         DMJD = DMJD + HOURS/24.0D0
      END IF
999   CONTINUE
      END

*+ANT_SCAN   Scans string for match with any of a set of characters
      INTEGER FUNCTION ANT_SCAN(STRING, SET, BACK)
      CHARACTER STRING*(*), SET*(*)
      LOGICAL BACK
*STRING   input    Character string to scan
*SET      input    Set of characters to find
*BACK     input    If .TRUE. scans from right to left, else left to right.
*ANT_SCAN function returns position in STRING where first match is found, else
* returns zero if STRING contains none of the characters listed in SET.
*-Author  Clive Page 1993-July-21
*Can be replaced by FORTRAN-90 intrinsic function SCAN when available
      INTEGER K
*
      IF(BACK) THEN
         DO 111,K = LEN(STRING),1,-1
            IF(INDEX(SET,STRING(K:K)) .NE. 0) THEN
               ANT_SCAN = K
               GO TO 999
            END IF
111      CONTINUE
      ELSE
         DO 222,K = 1,LEN(STRING)
            IF(INDEX(SET,STRING(K:K)) .NE. 0) THEN
               ANT_SCAN = K
               GO TO 999
            END IF
222      CONTINUE
      END IF
      ANT_SCAN = 0
999   CONTINUE
      END

*+ANT_SCNDEC  Gets next unsigned decimal value from string, advances pointer.
      SUBROUTINE ANT_SCNDEC(TEXT, K, RVALUE)
      CHARACTER TEXT*(*)
      INTEGER K
      REAL RVALUE
*TEXT    input  String of text e.g. "10.015", signs and exponents not supported
*K       in/out Pointer to first digit, but skips leading spaces,
*               exits pointing to the terminating char.
*RVALUE  output Returns unsigned real value, or =0.0 if none found.
*-Author  Clive Page  1991-SEP-24
      LOGICAL DIGIT
      INTEGER I, ICZERO, LENT
      REAL FRAC
*Statement function DIGIT returns .true. of TEXT(K:K) is a digit
      DIGIT(I) = TEXT(I:I) .GE. '0' .AND. TEXT(I:I) .LE. '9'
*
      ICZERO = ICHAR('0')
      RVALUE = 0.0
      LENT = LEN(TEXT)
*Skip over any leading spaces
10    IF(K .LE. LENT .AND. TEXT(K:K) .EQ. ' ')THEN
         K = K + 1
         GO TO 10
      END IF
*Now compute value of digits preceding any decimal point
20    IF(K .LE. LENT .AND. DIGIT(K))THEN
         RVALUE = 10.0*RVALUE + ICHAR(TEXT(K:K)) - ICZERO
         K = K + 1
         GO TO 20
      ELSE IF(K .LE. LENT .AND. TEXT(K:K) .EQ. '.')THEN
*Skip the decimal point, add the value of each decimal fraction digit
         K = K + 1
         FRAC = 1.0
30       IF(K .LE. LENT .AND. DIGIT(K))THEN
            FRAC = 0.1*FRAC
            RVALUE = RVALUE + FRAC*(ICHAR(TEXT(K:K))-ICZERO)
            K = K + 1
            GO TO 30
         END IF
      END IF
      END

*+ANT_SCNINT  Get next unsigned integer from string, advances pointer
      SUBROUTINE ANT_SCNINT(TEXT, K, IVAL)
      CHARACTER TEXT*(*)
      INTEGER K, IVAL
*TEXT    input  String of text
*K       in/out Points to first digit, but skips leading spaces,
*               exits pointing to first non-digit after the number
*IVAL    output Returns +ve integer value, or =0 if none found.
*-Author  Clive Page  1991-SEP-24
      INTEGER ICZERO, LENT
*
      IVAL = 0
      LENT = LEN(TEXT)
      ICZERO = ICHAR('0')
*Skip any leading spaces
10    IF(K .LE. LENT .AND. TEXT(K:K) .EQ. ' ')THEN
         K = K + 1
         GO TO 10
      END IF
20    IF(K .LE. LENT .AND. TEXT(K:K) .GE. '0' .AND.
     $                     TEXT(K:K) .LE. '9')  THEN
         IVAL = 10*IVAL + ICHAR(TEXT(K:K)) - ICZERO
         K = K + 1
         GO TO 20
      END IF
      END

*+ANT_SCNSYM  Scans text for next word of specified type.
      SUBROUTINE ANT_SCNSYM(IMASK, TEXT, K, WORD)
      INTEGER IMASK, K
      CHARACTER*(*) TEXT, WORD
*IMASK  input  Sum of values to specify character types allowed in WORD:
*              1=letter, 2=digit, 4=underscore, 8=dot, 16=colon,
*              32=#$+*-/<=>\|
*TEXT   input  String to scan starting at position K, but note that
*              any leading spaces will be skipped.
*K      in/out On entry points to first character of TEXT to check,
*              returns pointing to terminator character (next invalid
*              character) or =1+LEN(TEXT) if remainder of TEXT is all valid.
*WORD   output Returns string if found, else blank.
*** restrictions: ASCII character set only,
***               uses IAND function for logical AND of integers.
*-Author  Clive Page  1992-AUG-13
      INTEGER MASK(0:255), KSTART
      SAVE MASK
*               ctrl       #$         +*       -   .  /   digits
      DATA MASK/32*0, 5*0, 2*32, 3*0, 2*32, 0, 32, 8, 32, 10*2,
     $ 16, 0, 3*32, 2*0, 26*1, 0, 32, 2*0, 4, 0, 26*1, 0, 32, 131*0/
*       :     <=>        LETTER   \        _     letter   |
*
*If K is not a valid pointer initially just return all blanks
*
      IF(K .LE. 0 .OR. K .GT. LEN(TEXT)) THEN
         WORD = ' '
         GO TO 999
      END IF
*
*Advance past any initial spaces
*
100   CONTINUE
      IF(TEXT(K:K) .EQ. ' ') THEN
         K = K + 1
         IF(K .LE. LEN(TEXT)) GO TO 100
         WORD = ' '
         GO TO 999
      END IF
*
*KSTART is the starting position of the next non-space item.
*AND each character with the specified mask, if result zero then no match
*
      KSTART = K
200   CONTINUE
      IF(IAND(IMASK,MASK(ICHAR(TEXT(K:K)))) .NE. 0) THEN
         K = K + 1
         IF(K .LE. LEN(TEXT)) GO TO 200
      END IF
*The required word is in TEXT(KSTART:K-1) if this range is non-zero
      IF(K .GT. KSTART) THEN
         WORD = TEXT(KSTART:K-1)
      ELSE
         WORD = ' '
      END IF
999   CONTINUE
      END

*+ANT_SCWORD  Scans string for next Space-delimited Word.
      SUBROUTINE ANT_SCWORD(TEXT, KPOS, WORD)
      CHARACTER TEXT*(*), WORD*(*)
      INTEGER KPOS
*TEXT  input   Line of text with words separated by spaces
*KPOS  in/out  Points to next char to check (but skips leading spaces)
*              returns pointing to space that terminates the word.
*WORD  output  Word, if line empty returns spaces.
*Note: any set of characters enclosed in single/double quotes also counts
* as a single word, and is returned with the enclosing quotes.
*-Author  Clive Page  1992-JUN-24
      INTEGER LENT, KSTART, KQ
*
*Check for invalid entry conditions
      LENT = LEN(TEXT)
      IF(KPOS .LT. 1 .OR. KPOS .GT. LENT)THEN
         WORD = ' '
         GO TO 999
      END IF
*Skip any leading blanks (return blank word if nothing else left)
100   IF(TEXT(KPOS:KPOS) .EQ. ' ')THEN
         KPOS = KPOS + 1
         IF(KPOS .LE. LENT)GO TO 100
         WORD = ' '
         GO TO 999
      END IF
*Save starting position in KSTART, must point to non-space by now
      KSTART = KPOS
*If first character is single or double quote then scan for matching one
      IF(INDEX('"''',TEXT(KPOS:KPOS)) .NE. 0 .AND.
     $    KPOS .LT. LENT) THEN
         KQ = INDEX(TEXT(KSTART+1:), TEXT(KSTART:KSTART))
         IF(KQ .EQ. 0)THEN
*No matching quote: probably an error but just return rest of line
            WORD = TEXT(KSTART+1:)
            KPOS = LENT + 1
         ELSE
*Return string with enclosing quotes, return KPOS at posn after closing quote
            KPOS = KPOS + KQ
            WORD = TEXT(KSTART:KPOS)
            KPOS = KPOS + 1
         END IF
      ELSE
*No quotes involved, search for next space
         KQ = INDEX(TEXT(KSTART:), ' ')
         IF(KQ .EQ. 0)THEN
*Word occupies the rest of the line
            WORD = TEXT(KSTART:)
            KPOS = LENT + 1
         ELSE
*Word is characters up to next space
            WORD = TEXT(KSTART:KSTART+KQ-1)
            KPOS = KSTART + KQ
         END IF
      END IF
999   CONTINUE
      END

*+ANT_SEXRAD Converts sexagesimal string (+dd:mm:ss.s) to radians.
      SUBROUTINE ANT_SEXRAD(STRING, DVALUE, STATUS)
      CHARACTER STRING*(*)
      DOUBLE PRECISION DVALUE
      INTEGER STATUS
*STRING input  Expects 2 or 3 items separated by spaces or colon
*              If contains + or - sign: taken as degs:arcmins:arcsecs
*              otherwise as hours:mins:secs.
*DVALUE output Double precision value converted to degrees/hours
*STATUS in/out Inherited status
*-
      DOUBLE PRECISION PI, DTOR
      PARAMETER (PI=3.141592653589D0, DTOR=PI/180.0D0)
*
      IF(STATUS .NE. 0) RETURN
      CALL ANT_SEX2D(STRING, DVALUE, STATUS)
      IF(  INDEX(STRING,'+') .NE. 0 .OR.
     $     INDEX(STRING,'-') .NE. 0) THEN
         DVALUE = DVALUE * DTOR
      ELSE
         DVALUE = DVALUE * 15.0 * DTOR
      END IF
      END

*+ANT_SEX2D  Converts sexagesimal string (dd:mm:ss.s) to degrees
      SUBROUTINE ANT_SEX2D(STRING, DVALUE, STATUS)
      CHARACTER STRING*(*)
      DOUBLE PRECISION DVALUE
      INTEGER STATUS
*STRING input  Expects 2 or 3 items separated by spaces or colon
*DVALUE output Double precision value converted to degrees/hours
*STATUS in/out Inherited status
*-Author  Clive Page  1993-JUL-21
*Should now cope with sign before any numerical component,
* will also convert hh:mm:ss to hours.
      LOGICAL NEGATE, SKIP
      INTEGER K, IDEG
      REAL RMIN, RSEC
*Statement function
      SKIP(K) = INDEX(' +-',STRING(K:K)) .NE. 0
*
      IF(STATUS .NE. 0)RETURN
*If minus sign found anywhere set NEGATE flag
      NEGATE = INDEX(STRING, '-') .NE. 0
      K = 1
*Skip any leading sign or spaces
10    IF(K .LT. LEN(STRING)) THEN
         IF(SKIP(K)) THEN
            K = K + 1
            GO TO 10
         END IF
      END IF
*Get the degrees value, ought always to be an integer
      CALL ANT_SCNINT(STRING, K, IDEG)
*Skip terminator of this integer, then any more spaces or signs
      K = K + 1
20    IF(K .LT. LEN(STRING)) THEN
         IF(SKIP(K)) THEN
            K = K + 1
            GO TO 20
         END IF
      END IF
*Minutes value could be integer or real, so could the seconds value
      CALL ANT_SCNDEC(STRING, K, RMIN)
      K = K + 1
30    IF(K .LT. LEN(STRING)) THEN
         IF(SKIP(K)) THEN
            K = K + 1
            GO TO 30
         END IF
      END IF
      CALL ANT_SCNDEC(STRING, K, RSEC)
*Now compute the final value, negate if necessary
      DVALUE = DBLE(IDEG) + RMIN/60.0D0 + RSEC/3600.0D0
      IF(NEGATE)DVALUE = -DVALUE
*Check mins, secs, harder to be sure of out-of-range value for degrees
      IF(RMIN .GT. 60.0 .OR. RSEC .GT. 60.0)THEN
         CALL ANT_ERRC ('ANT_SEX2D: bad mins/secs range,', STRING)
C        WRITE(*,*)'!ANT_SEX2D: bad mins/secs range ', STRING
         STATUS = -9
      END IF
      END

*+ANT_SLEN  Returns length of string when trailing spaces are trimmed off.
      INTEGER FUNCTION ANT_SLEN(STRING)
      CHARACTER STRING*(*)
*STRING    input    String of any length
*LEN_TRIM  function Returns no of chars ignoring trailing spaces, but >0.
***NOTE: for string of all spaces this function returns 1 not 0 so that
* a reference to STR(1:ANT_SLEN(STR)) is valid Fortran-77.
* Otherwise similar to Fortran-90 intrinsic LEN_TRIM.
*-Author Clive Page 1993-JUL-21
      INTEGER K
*
      DO 111,K = LEN(STRING), 2, -1
          IF(STRING(K:K) .NE. ' ') GO TO 200
111   CONTINUE
      K = 1
200   ANT_SLEN = K
      END

*+ANT_SQTOS Converts quoted string to simple string, returns length.
      SUBROUTINE ANT_SQTOS(INSTR, LENGTH, OUTSTR, STATUS)
      CHARACTER*(*) INSTR, OUTSTR
      INTEGER LENGTH, STATUS
*INSTR   input  String with enclosing double or single quotes, note that
*               the first and last chars of INSTR are ignored, as are
*               any chars same as this within the string.
*LENGTH  output Trimmed length of de-quoted string OUTSTR
*OUTSTR  output String with quotes removed.
*STATUS  in/out Inherited status.
*-Author  Clive Page 1993-JUL-28
      INTEGER J
      LOGICAL QUOTE, QPREV
*
      IF(STATUS .NE. 0) RETURN
      LENGTH = 0
      OUTSTR = ' '
      QPREV = .FALSE.
      DO J = 2,LEN(INSTR)
         QUOTE = INSTR(J:J) .EQ. INSTR(1:1)
         IF(QUOTE .EQV. QPREV) THEN
*non-quote follows non-quote (or quote follows quote): copy char to output
            QPREV = .FALSE.
            IF(LENGTH .LT. LEN(OUTSTR)) THEN
               LENGTH = LENGTH + 1
               OUTSTR(LENGTH:LENGTH) = INSTR(J:J)
            ELSE
               CALL ANT_ERRC ('ANT_SQTOS: string truncated,', INSTR)
C              WRITE(*,*)'!ANT_SQTOS: string truncated ', INSTR
               GO TO 999
            END IF
         ELSE IF(QUOTE) THEN
*quote follows non-quote
            QPREV = .TRUE.
         ELSE IF(QPREV) THEN
*non-quote follows quote - end of the quoted string
            GO TO 999
         END IF
      END DO
999   CONTINUE
      END

*+ANT_STOD  Decodes string to DOUBLE PRECISION
      SUBROUTINE ANT_STOD(STRING, VALUE, STATUS)
      CHARACTER STRING*(*)
      DOUBLE PRECISION VALUE
      INTEGER STATUS
*STRING  input  Character string
*VALUE   output Returned value
*STATUS  in/out Inherited status
*-Author  Clive Page  1993-JULY-21
*
      IF(STATUS .NE. 0)RETURN
      READ(STRING, '(BN,F40.0)', IOSTAT=STATUS) VALUE
      IF(STATUS .NE. 0) THEN
         CALL ANT_ERRC ('ANT_STOD: bad double prec. number,', STRING)
C        WRITE(*,*)'!ANT_STOD: bad double prec number ', STRING
      END IF
      END

*+ANT_STOI   Decodes string to integer value, (includes hex/octal/binary)
      SUBROUTINE ANT_STOI(STRING, VALUE, STATUS)
      CHARACTER*(*) STRING
      INTEGER VALUE, STATUS
*STRING  input  Decimal integer, or %Xhex, or %Ooctal or %Bbinary
*VALUE   output Integer value
*STATUS  in/out Status return
*-Author  Clive Page  1993-JUL-21
      INTEGER K, L, DIGIT
      CHARACTER TEMP*11
*
      IF(STATUS .NE. 0) RETURN
      K = 1
      L = LEN(STRING)
*Skip any leading spaces to find first non-space
100   IF(STRING(K:K) .EQ. ' ' .AND. K .LT. L) THEN
         K = K + 1
         GO TO 100
      ELSE IF(STRING(K:K) .EQ. '%' .AND. K+1 .LT. L) THEN
*Start of hexadecimal, octal, or binary value
         VALUE = 0
         K = K + 1
         IF(STRING(K:K) .EQ. 'X' .OR. STRING(K:K) .EQ. 'x') THEN
*Hexadecimal value
200         K = K + 1
            DIGIT = ICHAR(STRING(K:K))
*Allow 0-9 A-F and a-f
            IF(DIGIT .GE. 48 .AND. DIGIT .LE. 57) THEN
               VALUE = 16*VALUE + DIGIT - 48
            ELSE IF(DIGIT .GE. 65 .AND. DIGIT .LE. 70) THEN
               VALUE = 16*VALUE + DIGIT - 55
            ELSE IF(DIGIT .GE. 97 .AND. DIGIT .LE. 102) THEN
               VALUE = 16*VALUE + DIGIT - 87
            ELSE IF(DIGIT .EQ. 32) THEN
               GO TO 999
            ELSE
               CALL ANT_ERRC ('!ANT_STOI: bad hex digit,', STRING)
C              WRITE(*,*)'!ANT_STOI: bad hex digit', STRING
               STATUS = -9
               GO TO 999
            END IF
            IF(K .LT. L) GO TO 200
         ELSE IF(STRING(K:K) .EQ. 'O' .OR. STRING(K:K) .EQ. 'o') THEN
*Octal value
300         K = K + 1
            DIGIT = ICHAR(STRING(K:K))
            IF(DIGIT .GE. 48 .AND. DIGIT .LE. 55) THEN
               VALUE = 8*VALUE + DIGIT - 48
            ELSE IF(DIGIT .EQ. 32) THEN
               GO TO 999
            ELSE
               CALL ANT_ERRC ('ANT_STOI: bad octal digit,', STRING)
C              WRITE(*,*)'!ANT_STOI: bad octal digit ', STRING
               STATUS = -9
               GO TO 999
            END IF
            IF(K .LT. L) GO TO 300
         ELSE IF(STRING(K:K) .EQ. 'B' .OR. STRING(K:K) .EQ. 'b') THEN
*Binary value
400         K = K + 1
            DIGIT = ICHAR(STRING(K:K))
            IF(DIGIT .GE. 48 .AND. DIGIT .LE. 49) THEN
               VALUE = 2*VALUE + DIGIT - 48
               IF(K .LT. L) GO TO 400
            ELSE IF(DIGIT .EQ. 32) THEN
               GO TO 999
            ELSE
               CALL ANT_ERRC ('ANT_STOI: bad binary digit,', STRING)
C              WRITE(*,*)'!ANT_STOI: bad binary digit ', STRING
               STATUS = -9
               GO TO 999
            END IF
         ELSE
            CALL ANT_ERRC ('NT_STOI: unknown number base,', STRING)
C           WRITE(*,*)'!ANT_STOI: unknown number base ', STRING
            STATUS = -9
            GO TO 999
         END IF
      ELSE
*Assume this is a decimal number, let the system detect errors
         TEMP = STRING(K:)
         READ(TEMP, '(BN,I11)', IOSTAT=STATUS) VALUE
         IF(STATUS .NE. 0) THEN
            CALL ANT_ERRC ('ANT_STOI: bad integer number,', STRING)
C           WRITE(*,*)'!ANT_STOI: bad integer number ', STRING
         END IF
      END IF
999   CONTINUE
      END

*+ANT_STOL  Decodes string to logical
      SUBROUTINE ANT_STOL(STRING, VALUE, STATUS)
      CHARACTER STRING*(*)
      LOGICAL VALUE
      INTEGER STATUS
*STRING input   Character string, leading spaces or dots ignored,
*               next char should be T or Y for true, F or N for false
*VALUE  output  Returned value, .TRUE. or .FALSE.
*STATUS in/out  Inherited status
*-Author  Clive Page  1993-JUL-21
      INTEGER K, ANT_SVERIF
      EXTERNAL ANT_SVERIF
*
      IF(STATUS .NE. 0)RETURN
*Skip to first character that is not dot or space
      K = 1
100   CONTINUE
      IF(K .LT. LEN(STRING) .AND. (STRING(K:K) .EQ. ' ' .OR.
     $                             STRING(K:K) .EQ. '.')) THEN
         K = K + 1
         GO TO 100
      END IF
      IF(INDEX('YTyt',STRING(K:K)) .NE. 0)THEN
         VALUE = .TRUE.
      ELSE IF(INDEX('NFnf', STRING(K:K)) .NE. 0) THEN
         VALUE = .FALSE.
      ELSE
         CALL ANT_ERRC ('ANT_STOL: bad logical,', STRING)
C        WRITE(*,*)'!ANT_STOL: bad logical ', STRING
         STATUS = -9
      END IF
      END

*+ANT_SUPPER  Converts all letters in a string to upper-case
      SUBROUTINE ANT_SUPPER(STRING)
      CHARACTER STRING*(*)
*STRING  in/out  String, returned in UPPER-case
*-Author  Clive Page  1991-Jan-1
*RESTRICTION: assumes ASCII is the native character code.
      INTEGER K
*
      DO 111,K = 1, LEN(STRING)
         IF(STRING(K:K) .GE. 'a' .AND. STRING(K:K) .LE. 'z')
     $       STRING(K:K) = CHAR(ICHAR(STRING(K:K))-32)
111   CONTINUE
      END

*+ANT_SVERIF  Searches string for first character NOT in specified list
      INTEGER FUNCTION ANT_SVERIF(STRING, SET, BACK)
      CHARACTER STRING*(*), SET*(*)
      LOGICAL BACK
*STRING     input    Character string to scan
*SET        input    Set of characters to find
*BACK       input    If .TRUE. scans from right to left, else left to right.
*ANT_SVERIF returns position of character in STRING having a non-match if found,
* else returns zero if STRING contains only those listed in SET.
*-Author  Clive Page  1993-JULY-21
*Can be replaced by FORTRAN-90 intrinsic function VERIFY when available
      INTEGER K
*
      IF(BACK) THEN
         DO 111,K = LEN(STRING),1,-1
            IF(INDEX(SET,STRING(K:K)) .EQ. 0) THEN
               ANT_SVERIF = K
               GO TO 999
            END IF
111      CONTINUE
      ELSE
         DO 222,K = 1,LEN(STRING)
            IF(INDEX(SET,STRING(K:K)) .EQ. 0) THEN
               ANT_SVERIF = K
               GO TO 999
            END IF
222      CONTINUE
      END IF
      ANT_SVERIF = 0
999   CONTINUE
      END

*+ANT_STRIPS  Strips leading trailing and multiple embedded spaces from string
      SUBROUTINE ANT_STRIPS(INSTR, OUTSTR, NOUT)
      CHARACTER*(*) INSTR, OUTSTR
      INTEGER NOUT
*INSTR  input  Character string.
*OUTSTR output As input, but with leading and trailing spaces removed and
*              any multiple embedded spaces converted to single space.
*NOUT   output Number of significant characters copied to OUTSTR,
*              waning: returns 0 if INSTR is all blanks.
*-Author   Clive Page   1991-SEP-10
      LOGICAL FIRST
      INTEGER LENOUT, K
*
      LENOUT = LEN(OUTSTR)
      NOUT   = 0
      FIRST  = .FALSE.
      DO 100,K = 1,LEN(INSTR)
         IF(INSTR(K:K) .NE. ' ' .OR. FIRST) THEN
            IF(NOUT .LT. LENOUT) THEN
               NOUT = NOUT + 1
               OUTSTR(NOUT:NOUT) = INSTR(K:K)
            END IF
            FIRST = INSTR(K:K) .NE. ' '
         END IF
100   CONTINUE
*Remove last character if this is also a blank
      IF(NOUT .GT. 1 .AND. OUTSTR(NOUT:NOUT) .EQ. ' ') NOUT = NOUT - 1
      END
