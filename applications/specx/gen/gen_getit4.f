*  History:
*     31 July 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE GEN_GETIT4 (STRING, LEVEL, IST, IFIN, ICHAR, IERR)


      IMPLICIT  NONE

C   Routine to get location of the next field from the character string STRING

C   If there are insufficient fields the error return is set to 2,
C   otherwise it is returned with a value of 0.

*   Modified form of GEN_GETIT, but returns item locations in string rather
*   than the actual item.

C   A field may be delimited by any of the 3 characters  comma, /, (blank),
C   or ;, or by a string composed only of a number of blanks in combination with
C   a single valid delimiter at the selected level LEVEL,
C   and may include any of these delimiters if it is given as a character
C   field, i.e. enclosed in apostrophes ('). Within a character field
C   the double apostrophe '' is treated as a single character.

C   The parameter LEVEL sets the level at which delimiters are recognized.
C   At level 3 only the semicolon or colon/blank sequence is valid, at level 2
C   the slash and slash/blank sequence are also valid, and at level 1 the
C   comma, comma/blank sequence and pure blank sequence are all also valid.


*     Formal parameters

      CHARACTER STRING*(*)
      INTEGER*4 LEVEL
      INTEGER*4 IST, IFIN
      INTEGER*4 ICHAR
      INTEGER*4 IERR

*     Functions

      INTEGER*4 GEN_DELIM
      INTEGER*4 GEN_ILEN
      INTEGER*4 GEN_IENDCH

*     Local variables

      INTEGER*4 IEND
      INTEGER*4 I1
      INTEGER*4 IL
      INTEGER*4 ILDEL
      INTEGER*4 ILS
      INTEGER*4 J
      CHARACTER CHAR*1
      CHARACTER HD*1
      CHARACTER LBKT*1
      CHARACTER RBKT*1

      DATA HD    /''''/
      DATA LBKT  /'('/
      DATA RBKT  /')'/


      ILS   = GEN_ILEN (STRING)
      IERR  = 0

      IST   = 1
      IFIN  = 1

*     Print *,'Entry to GENLIB: input string below'
*     Print *, string

C  Suppress leading blanks.

      IF (ICHAR.le.0) ICHAR = 1

      DO WHILE (STRING(ICHAR:ICHAR).EQ.' ' .AND. ICHAR.LE.ILS)
        ICHAR = ICHAR+1
      END DO

C  If this now takes us past the end of the string then string is empty

      IF (ICHAR.GT.ILS) THEN
        IERR = 2
*       Print *, '--getit3-- empty string'
        RETURN
      END IF

      IST = ICHAR

C  At level 5 we just want the whole string

      IF (LEVEL.EQ.5) THEN
        IFIN  = ILS
        ICHAR = IFIN + 1
*       Print *, '--getit3-- level 5 request'
*       Print *, '  returned item: ', string(ist:ifin)
        RETURN
      END IF

C  Look for next string:

      CHAR = STRING(ICHAR:ICHAR)

C     Check after previous item for occurrence of one of...
C           Second delimiter ( => null string )
C           Other character
C     and find position in STRING of end of this item.

      IEND = GEN_DELIM (STRING, ICHAR, LEVEL, ILDEL)

C     Case 1; next character is new delimiter

      IF (IEND.GT.0)   THEN                     ! 2nd delimiter, null string
        IFIN  = ICHAR-1
        ICHAR = ICHAR+ILDEL

C     Case 2; other character, look for next delimiter to finish

      ELSE
        J = ICHAR

        DO WHILE (J.LE.ILS)
          CHAR = STRING(J:J)
          IEND = GEN_DELIM (STRING,J,LEVEL,IL)
          IF (IEND.GT.0)   GO TO 7             ! Next delimiter located

          IL = 0

          IF (CHAR.EQ.HD) THEN                 ! No, hollerith delimiter
C           Need to skip to end of hollerith string
            IL = GEN_IENDCH (STRING(J:))
            IF (IL.LE.2) THEN
              Print *,'Missing '' in GETIT3 - abandoning'
              IERR = 4
              RETURN
            END IF
            J = J+IL-1

          ELSE IF (CHAR.EQ.LBKT) THEN          ! No, expression or list
            CALL GET_SUBEXPR (STRING(J:), I1, IL, IERR)
            IF (IERR.NE.0) THEN
              Print *,'Missing parenthesis in GETIT3 - abandoning'
              IERR = 5
              RETURN
            END IF
            J  = J + IL   ! jump to closing bracket
            IL = 0        ! set delimiter length to zero, in case we don't
                          ! get back to another search
          END IF

          J = J+1
        END DO

*       Exit do by running out of input -- J must be set to character after string

*       If second delimeter found: set end of string to 1 less than start
*       of delimiter (or end of string if no delimeter)

    7   IFIN  = J-1

*       Start of next string set to 1 after end of terminating delimiter.

        ICHAR = J+IL

      END IF

CD    Print *, '--getit3--'
CD    Print *, '  returned item: ', string(ist:ifin)

      RETURN
      END
