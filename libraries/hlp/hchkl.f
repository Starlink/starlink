      SUBROUTINE hlp_HCHKL (RECORD, LEVEL, NAME)
*+
*  - - - - - -
*   H C H K L
*  - - - - - -
*
*  Check if the line held in RECORD is the start of a new help level
*  and if so get the level number and the keyword.
*
*  Given (in COMMON):
*     LEVOFF   i         logical level for the current help library
*
*  Given (argument):
*     RECORD   c*(*)     the string to be examined
*
*  Returned:
*     LEVEL    i         level number (-1 if not a keyword record)
*     NAME     c*(*)     level name (only if LEVEL.GE.0)
*
*  The line in RECORD is provisionally assumed to be the start of a new
*  help level if the first space-separated field begins at the start of
*  the line and consists wholly of decimal digits.
*
*  Called:  hlp_SPLIT, hlp_DEC
*
*  P.T.Wallace   Starlink   13 June 1995
*-

      IMPLICIT NONE

      CHARACTER*(*) RECORD
      INTEGER LEVEL
      CHARACTER*(*) NAME

      INCLUDE 'helpic'

      INTEGER IPS,IPF,L,I



*  Locate the first field in RECORD.
      CALL hlp_SPLIT(RECORD,1,IPS,IPF)

*  Does it start at the beginning of the line?
      IF (IPS.EQ.1) THEN

*     Yes: attempt to decode a decimal integer.
         CALL hlp_DEC(RECORD,IPS,L)

*     Was it entirely decimal digits?
         I=IPF+1
         IF (IPS.EQ.I) THEN

*        Yes: return the logical level number.
            LEVEL=L+LEVOFF

*        Obtain the level name if any.
            CALL hlp_SPLIT(RECORD,I,IPS,IPF)
            IF (IPS.GT.0) NAME=RECORD(IPS:IPF)
         END IF
      END IF

      END
