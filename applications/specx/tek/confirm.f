*  History:
*     19 Nov 1993 (hme):
*        Replace backslash in string constant with CHAR(92).
C----------------------------------------------------------------------------

      SUBROUTINE CONFIRM (CHR,OUTCHAR)

C   Routine to generate a representation of the character typed in
C   response to cursor input or otherwise.

      CHARACTER CHR*1,OUTCHAR*2
      BYTE      ICHAR

      OUTCHAR=' '
      READ(CHR,'(A1)') ICHAR
      IF(ICHAR.LE.26) THEN
       ICHAR=ICHAR+64
       WRITE(OUTCHAR,1000) ICHAR
      ELSE IF (ICHAR.EQ.27) THEN
       OUTCHAR='^['
      ELSE IF (ICHAR.EQ.28) THEN
       OUTCHAR='^'//CHAR(92)
      ELSE IF (ICHAR.EQ.29) THEN
       OUTCHAR='^]'
      ELSE IF (ICHAR.EQ.30) THEN
       OUTCHAR='`'
      ELSE IF (ICHAR.EQ.31) THEN
       OUTCHAR='/'
      ELSE IF (ICHAR.EQ.177) THEN
       OUTCHAR='De'
      ELSE
       WRITE(OUTCHAR,1001) ICHAR
      END IF
      
 1000 FORMAT('^',A1)
 1001 FORMAT(1X,A1)

      RETURN
      END


