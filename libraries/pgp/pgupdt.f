C*PGUPDT -- update display
C%void cpgupdt(void);
C+
      SUBROUTINE PGUPDT
C
C Update the graphics display: flush any pending commands to the
C output device. This routine empties the buffer created by PGBBUF,
C but it does not alter the PGBBUF/PGEBUF counter. The routine should
C be called when it is essential that the display be completely up to
C date (before interaction with the user, for example) but it is not
C known if output is being buffered.
C
C Arguments: none
C--
C 27-Nov-1986
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGUPDT')) RETURN
      CALL GRTERM
      END
