C*PGETXT -- erase text from graphics display
C%void cpgetxt(void);
C+
      SUBROUTINE PGETXT
C
C Some graphics terminals display text (the normal interactive dialog)
C on the same screen as graphics. This routine erases the text from the
C view surface without affecting the graphics. It does nothing on
C devices which do not display text on the graphics screen, and on
C devices which do not have this capability.
C
C Arguments:
C  None
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CALL GRETXT
      END
