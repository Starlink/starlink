C*PGSAVE -- save PGPLOT attributes
C%void cpgsave(void);
C+
      SUBROUTINE PGSAVE
C
C This routine saves the current PGPLOT attributes in a private storage
C area. They can be restored by calling PGUNSA (unsave). Attributes
C saved are: character font, character height, color index, fill-area
C style, line style, line width, pen position, arrow-head style, 
C hatching style, and clipping state. Color representation is not saved.
C
C Calls to PGSAVE and PGUNSA should always be paired. Up to 20 copies
C of the attributes may be saved. PGUNSA always retrieves the last-saved
C values (last-in first-out stack).
C
C Note that when multiple devices are in use, PGUNSA retrieves the
C values saved by the last PGSAVE call, even if they were for a
C different device.
C
C Arguments: none
C--
C 20-Apr-1992 - new routine [TJP].
C 27-Nov-1992 - add arrowhead style [TJP].
C  6-Oct-1993 - add text opacity [TJP].
C 28-Feb-1994 - correct bug (variable not saved) [TJP].
C 26-Feb-1995 - add hatching attributes.
C 19-Jun-1996 - correction in header comments [TJP].
C 26-Feb-1997 - add clipping state [TJP].
C-----------------------------------------------------------------------
      INTEGER MAXS
      PARAMETER (MAXS=20)
C
      INTEGER LEV
      INTEGER CF(MAXS), CI(MAXS), FS(MAXS), LS(MAXS), LW(MAXS)
      INTEGER AHFS(MAXS), TBG(MAXS), CLP(MAXS)
      REAL    CH(MAXS), POS(2,MAXS)
      REAL    AHANG(MAXS), AHBARB(MAXS), HSA(MAXS), HSS(MAXS), HSP(MAXS)
      SAVE    LEV, CF, CI, FS, LS, LW, AHFS, TBG, CH, POS
      SAVE    AHANG, AHBARB, HSA, HSS, HSP, CLP
      DATA    LEV /0/
C
      IF (LEV.GE.MAXS) THEN
          CALL GRWARN('Too many unmatched calls to PGSAVE')
      ELSE
          LEV = LEV+1
          CALL PGQCF(CF(LEV))
          CALL PGQCH(CH(LEV))
          CALL PGQCI(CI(LEV))
          CALL PGQFS(FS(LEV))
          CALL PGQLS(LS(LEV))
          CALL PGQLW(LW(LEV))
C          CALL PGQVP(0, VP(1,LEV), VP(2,LEV), VP(3,LEV), VP(4,LEV))
C          CALL PGQWIN(WIN(1,LEV), WIN(2,LEV), WIN(3,LEV), WIN(4,LEV))
          CALL PGQPOS(POS(1,LEV), POS(2,LEV))
          CALL PGQAH(AHFS(LEV), AHANG(LEV), AHBARB(LEV))
          CALL PGQTBG(TBG(LEV))
          CALL PGQHS(HSA(LEV), HSS(LEV), HSP(LEV))
          CALL PGQCLP(CLP(LEV))
      END IF
      RETURN     
C
C*PGUNSA -- restore PGPLOT attributes
C%void cpgunsa(void);
C+
      ENTRY PGUNSA
C
C This routine restores the PGPLOT attributes saved in the last call to
C PGSAVE. Usage: CALL PGUNSA (no arguments). See PGSAVE.
C
C Arguments: none
C-----------------------------------------------------------------------
      IF (LEV.LE.0) THEN
          CALL GRWARN('PGUNSA: nothing has been saved')
      ELSE
          CALL PGSCF(CF(LEV))
          CALL PGSCH(CH(LEV))
          CALL PGSCI(CI(LEV))
          CALL PGSFS(FS(LEV))
          CALL PGSLS(LS(LEV))
          CALL PGSLW(LW(LEV))
C          CALL PGSVP(VP(1,LEV), VP(2,LEV), VP(3,LEV), VP(4,LEV))
C          CALL PGSWIN(WIN(1,LEV), WIN(2,LEV), WIN(3,LEV), WIN(4,LEV))
          CALL PGMOVE(POS(1,LEV), POS(2,LEV))
          CALL PGSAH(AHFS(LEV), AHANG(LEV), AHBARB(LEV))
          CALL PGSTBG(TBG(LEV))
          CALL PGSHS(HSA(LEV), HSS(LEV), HSP(LEV))
          CALL PGSCLP(CLP(LEV))
          LEV = LEV-1
      END IF
      RETURN     
      END
