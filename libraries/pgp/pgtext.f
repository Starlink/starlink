C*PGTEXT -- write text (horizontal, left-justified)
C%void cpgtext(float x, float y, const char *text);
C+
      SUBROUTINE PGTEXT (X, Y, TEXT)
      REAL X, Y
      CHARACTER*(*) TEXT
C
C Write text. The bottom left corner of the first character is placed
C at the specified position, and the text is written horizontally.
C This is a simplified interface to the primitive routine PGPTXT.
C For non-horizontal text, use PGPTXT.
C
C Arguments:
C  X      (input)  : world x-coordinate of start of string.
C  Y      (input)  : world y-coordinate of start of string.
C  TEXT   (input)  : the character string to be plotted.
C--
C (2-May-1983)
C-----------------------------------------------------------------------
      CALL PGPTXT(X, Y, 0.0, 0.0, TEXT)
      END
