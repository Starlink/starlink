C*PGMTXT -- write text at position relative to viewport
C%void cpgmtxt(const char *side, float disp, float coord, \
C% float fjust, const char *text);
C+
      SUBROUTINE PGMTXT (SIDE, DISP, COORD, FJUST, TEXT)
      CHARACTER*(*) SIDE, TEXT
      REAL DISP, COORD, FJUST
C
C Write text at a position specified relative to the viewport (outside
C or inside).  This routine is useful for annotating graphs. It is used
C by routine PGLAB.  The text is written using the current values of
C attributes color-index, line-width, character-height, and
C character-font.
C
C Arguments:
C  SIDE   (input)  : must include one of the characters 'B', 'L', 'T',
C                    or 'R' signifying the Bottom, Left, Top, or Right
C                    margin of the viewport. If it includes 'LV' or
C                    'RV', the string is written perpendicular to the
C                    frame rather than parallel to it.
C  DISP   (input)  : the displacement of the character string from the
C                    specified edge of the viewport, measured outwards
C                    from the viewport in units of the character
C                    height. Use a negative value to write inside the
C                    viewport, a positive value to write outside.
C  COORD  (input)  : the location of the character string along the
C                    specified edge of the viewport, as a fraction of
C                    the length of the edge.
C  FJUST  (input)  : controls justification of the string parallel to
C                    the specified edge of the viewport. If
C                    FJUST = 0.0, the left-hand end of the string will
C                    be placed at COORD; if JUST = 0.5, the center of
C                    the string will be placed at COORD; if JUST = 1.0,
C                    the right-hand end of the string will be placed at
C                    at COORD. Other values between 0 and 1 give inter-
C                    mediate placing, but they are not very useful.
C  TEXT   (input) :  the text string to be plotted. Trailing spaces are
C                    ignored when justifying the string, but leading
C                    spaces are significant.
C
C--
C 18-Apr-1983
C 15-Aug-1987 - fix BBUF/EBUF error.
C 27-Aug-1987 - fix justification error if XPERIN.ne.YPERIN.
C 05-Sep-1989 - change so that DISP has some effect for 'RV' and 
C               'LV' options [nebk]
C 16-Oct-1993 - erase background of opaque text.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL ANGLE, D, X, Y, RATIO, XBOX(4), YBOX(4)
      INTEGER CI, I, L, GRTRIM
      CHARACTER*20 TEST
C
      IF (PGNOTO('PGMTXT')) RETURN
C
      L = GRTRIM(TEXT)
      IF (L.LT.1) RETURN
      D = 0.0
      IF (FJUST.NE.0.0) CALL GRLEN(TEXT(1:L),D)
      D = D*FJUST
      RATIO = PGYPIN(PGID)/PGXPIN(PGID)
      CALL GRTOUP(TEST,SIDE)
      IF (INDEX(TEST,'B').NE.0) THEN
          ANGLE = 0.0
          X = PGXOFF(PGID) + COORD*PGXLEN(PGID) - D
          Y = PGYOFF(PGID) - PGYSP(PGID)*DISP
      ELSE IF (INDEX(TEST,'LV').NE.0) THEN
          ANGLE = 0.0
          X = PGXOFF(PGID) - PGYSP(PGID)*DISP - D
          Y = PGYOFF(PGID) + COORD*PGYLEN(PGID) - 0.3*PGYSP(PGID)
      ELSE IF (INDEX(TEST,'L').NE.0) THEN
          ANGLE = 90.0
          X = PGXOFF(PGID) - PGYSP(PGID)*DISP
          Y = PGYOFF(PGID) + COORD*PGYLEN(PGID) - D*RATIO
      ELSE IF (INDEX(TEST,'T').NE.0) THEN
          ANGLE = 0.0
          X = PGXOFF(PGID) + COORD*PGXLEN(PGID) - D
          Y = PGYOFF(PGID) + PGYLEN(PGID) + PGYSP(PGID)*DISP
      ELSE IF (INDEX(TEST,'RV').NE.0) THEN
          ANGLE = 0.0
          X = PGXOFF(PGID) + PGXLEN(PGID) + PGYSP(PGID)*DISP - D
          Y = PGYOFF(PGID) + COORD*PGYLEN(PGID) - 0.3*PGYSP(PGID)
      ELSE IF (INDEX(TEST,'R').NE.0) THEN
          ANGLE = 90.0
          X = PGXOFF(PGID) + PGXLEN(PGID) + PGYSP(PGID)*DISP
          Y = PGYOFF(PGID) + COORD*PGYLEN(PGID) - D*RATIO
      ELSE
          CALL GRWARN('Invalid "SIDE" argument in PGMTXT.')
          RETURN
      END IF
      CALL PGBBUF
      IF (PGTBCI(PGID).GE.0) THEN
          CALL GRQTXT (ANGLE, X, Y, TEXT(1:L), XBOX, YBOX)
          DO 25 I=1,4
              XBOX(I) = (XBOX(I)-PGXORG(PGID))/PGXSCL(PGID)
              YBOX(I) = (YBOX(I)-PGYORG(PGID))/PGYSCL(PGID)
   25     CONTINUE
          CALL PGQCI(CI)
          CALL PGSCI(PGTBCI(PGID))
          CALL GRFA(4, XBOX, YBOX)
          CALL PGSCI(CI)
      END IF
      CALL GRTEXT(.FALSE.,ANGLE,.TRUE., X, Y, TEXT(1:L))
      CALL PGEBUF
      END
