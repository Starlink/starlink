C*PGSCF -- set character font
C%void cpgscf(int font);
C+
      SUBROUTINE PGSCF (FONT)
      INTEGER  FONT
C
C Set the Character Font for subsequent text plotting. Four different
C fonts are available:
C   1: (default) a simple single-stroke font ("normal" font)
C   2: roman font
C   3: italic font
C   4: script font
C This call determines which font is in effect at the beginning of
C each text string. The font can be changed (temporarily) within a text
C string by using the escape sequences \fn, \fr, \fi, and \fs for fonts
C 1, 2, 3, and 4, respectively.
C
C Argument:
C  FONT   (input)  : the font number to be used for subsequent text
C                    plotting (in range 1-4).
C--
C 26-Sep-1985 - new routine [TJP].
C 25-OCT-1993 - changed name of argument [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSCF')) RETURN
      CALL GRSFNT(FONT)
      END
