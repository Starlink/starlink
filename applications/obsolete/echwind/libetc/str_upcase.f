      SUBROUTINE STR_UPCASE(DEST,SOURCE)
C
C     S T R _ U P C A S E
C
C     Converts all lower case ASCII characters in SOURCE
C     to their upper case equivalents in DEST.
C
C     Parameters -   (">" input, "!" modified, "<" output)
C
C     (<) DEST     (Character) The converted string.
C     (>) SOURCE   (Character) The string to be converted.
C
C     History:
C
C     19th Feb 1988.  CKL / CIT. Original.
C      8th Oct 1990.  SNS / CIT. Hexidecimal constants converted to decimal
C                                because of bug in Sun Fortran 1.3.1.
C      5th Feb 1997.  Bly / RAL. Modified name from STR$UPCASE to STR_UPDATE
C                                for Linux port - g77 compiler whinges about
C                                $ in routine names.
C+
      CHARACTER*(*) DEST,SOURCE
C
      INTEGER I
      CHARACTER CHR
C
      DEST=SOURCE
      DO I=1,LEN(DEST)
	 CHR=DEST(I:I)
	 IF ((CHR.GE.'a').AND.(CHR.LE.'z')) THEN
	    DEST(I:I)=CHAR(ICHAR(CHR)-32)
	 END IF
      END DO
C
      END
