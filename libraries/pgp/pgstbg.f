C*PGSTBG -- set text background color index
C%void cpgstbg(int tbci);
C+
      SUBROUTINE PGSTBG (TBCI)
      INTEGER  TBCI
C
C Set the Text Background Color Index for subsequent text. By default
C text does not obscure underlying graphics. If the text background
C color index is positive, however, text is opaque: the bounding box
C of the text is filled with the color specified by PGSTBG before
C drawing the text characters in the current color index set by PGSCI.
C Use color index 0 to erase underlying graphics before drawing text.
C
C Argument:
C  TBCI   (input)  : the color index to be used for the background
C                    for subsequent text plotting:
C                      TBCI < 0  => transparent (default)
C                      TBCI >= 0 => text will be drawn on an opaque
C                    background with color index TBCI.
C--
C 16-Oct-1993 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSTBG')) RETURN
      IF (TBCI.LT.0) THEN
          PGTBCI(PGID) = -1
      ELSE
          PGTBCI(PGID) = TBCI
      END IF
      END
