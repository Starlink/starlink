*  History:
*     19 Nov 1993 (hme):
*        Replace backslash in string constant with CHAR(92).
*     27 Oct 2006 (timj):
*        Use ICHAR rather than formatted read of character to integer
*        Use CHAR() rather than formatted write
*        Use INTEGER rather than BYTE for CHARINT because BYTE is
*        signed and can not ever get to 177 for De
C----------------------------------------------------------------------------

      SUBROUTINE CONFIRM (CHR,OUTCHAR)

C   Routine to generate a representation of the character typed in
C   response to cursor input or otherwise.

      CHARACTER CHR*1,OUTCHAR*2
      INTEGER  CHARINT

      OUTCHAR=' '
      CHARINT = ICHAR(CHR)
      IF(CHARINT.LE.26) THEN
       CHARINT=CHARINT+64
       OUTCHAR = '^'
       OUTCHAR(2:2) = CHAR(CHARINT)
      ELSE IF (CHARINT.EQ.27) THEN
       OUTCHAR='^['
      ELSE IF (CHARINT.EQ.28) THEN
       OUTCHAR='^'//CHAR(92)
      ELSE IF (CHARINT.EQ.29) THEN
       OUTCHAR='^]'
      ELSE IF (CHARINT.EQ.30) THEN
       OUTCHAR='`'
      ELSE IF (CHARINT.EQ.31) THEN
       OUTCHAR='/'
      ELSE IF (CHARINT.EQ.177) THEN
       OUTCHAR='De'
      ELSE
       OUTCHAR(2:2) = CHAR(CHARINT)
      END IF

      RETURN
      END


