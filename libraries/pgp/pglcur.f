C*PGLCUR -- draw a line using the cursor
C%void cpglcur(int maxpt, int *npt, float *x, float *y);
C+
      SUBROUTINE PGLCUR (MAXPT, NPT, X, Y)
      INTEGER MAXPT, NPT
      REAL    X(*), Y(*)
C
C Interactive routine for user to enter a polyline by use of
C the cursor.  Routine allows user to Add and Delete vertices;
C vertices are joined by straight-line segments.
C
C Arguments:
C  MAXPT  (input)  : maximum number of points that may be accepted.
C  NPT    (in/out) : number of points entered; should be zero on
C                    first call.
C  X      (in/out) : array of x-coordinates (dimension at least MAXPT).
C  Y      (in/out) : array of y-coordinates (dimension at least MAXPT).
C
C Notes:
C
C (1) On return from the program, cursor points are returned in
C the order they were entered. Routine may be (re-)called with points
C already defined in X,Y (# in NPT), and they will be plotted
C first, before editing.
C
C (2) User commands: the user types single-character commands
C after positioning the cursor: the following are accepted:
C   A (Add)    - add point at current cursor location.
C   D (Delete) - delete last-entered point.
C   X (eXit)   - leave subroutine.
C--
C  5-Aug-1984 - new routine [TJP].
C 16-Jul-1988 - correct error in delete operation [TJP].
C 13-Dec-1990 - change warnings to messages [TJP].
C  3-Sep-1992 - fixed erase first point bug under Add option [JM/TJP].
C  7-Sep-1994 - use PGBAND [TJP].
C  2-Aug-1995 - remove dependence on common block [TJP].
C-----------------------------------------------------------------------
      LOGICAL  PGNOTO
      CHARACTER*1 LETTER
      INTEGER  PGBAND, I, SAVCOL, MODE
      REAL     XP, YP, XREF, YREF
      REAL     XBLC, XTRC, YBLC, YTRC
C
C Check that PGPLOT is in the correct state.
C
      IF (PGNOTO('PGLCUR')) RETURN
C
C Save current color.
C
      CALL GRQCI(SAVCOL)
C
C Put current line-segments on screen.
C
      IF (NPT.EQ.1) THEN
          CALL PGPT(1,X(1),Y(1),1)
      END IF
      IF (NPT.GT.0) THEN
          CALL GRMOVA(X(1),Y(1))
          DO 10 I=2,NPT
              CALL GRLINA(X(I),Y(I))
   10     CONTINUE
      END IF
C
C Start with the cursor in the middle of the box,
C unless lines have already been drawn.
C
      CALL PGQWIN(XBLC, XTRC, YBLC, YTRC)
      IF (NPT.GT.0) THEN
          XP = X(NPT)
          YP = Y(NPT)
      ELSE
          XP = 0.5*(XBLC+XTRC)
          YP = 0.5*(YBLC+YTRC)
      END IF
C
C Loop over cursor inputs.
C
      MODE = 0
  100 XREF = XP
      YREF = YP
      IF (PGBAND(MODE,1,XREF,YREF,XP,YP,LETTER).NE.1) RETURN
      CALL GRTOUP(LETTER,LETTER)
      MODE = 1
C
C A (ADD) command:
C
      IF (LETTER .EQ. 'A') THEN
          IF (NPT.GE.MAXPT) THEN
            CALL GRMSG('ADD ignored (too many points).')
              GOTO 100
          END IF
          NPT = NPT+1
          X(NPT) = XP
          Y(NPT) = YP
          IF (NPT.EQ.1) THEN
C           -- first point: draw a dot
            CALL GRMOVA(X(NPT),Y(NPT))
            CALL PGPT(1,X(NPT),Y(NPT),1)
          ELSE
C           -- nth point: draw from (n-1) to (n)
            CALL GRLINA(X(NPT),Y(NPT))
          END IF
          CALL GRTERM
C
C D (DELETE) command:
C
      ELSE IF (LETTER.EQ.'D') THEN
          IF (NPT.LE.0) THEN
            CALL GRMSG('DELETE ignored (there are no points left).')
            GOTO 100
          END IF
          IF (NPT.GT.1) THEN
C           -- delete nth point: erase from (n-1) to (n)
            CALL GRMOVA(X(NPT-1),Y(NPT-1))
            CALL GRSCI(0)
            CALL GRLINA(X(NPT),Y(NPT))
            CALL GRSCI(SAVCOL)
            CALL GRMOVA(X(NPT-1),Y(NPT-1))
            CALL GRTERM
          ELSE IF (NPT.EQ.1) THEN
C           -- delete first point: erase dot
            CALL GRSCI(0)
            CALL PGPT(1,X(NPT),Y(NPT),1)
            CALL GRSCI(SAVCOL)
          END IF
          NPT = NPT-1
          IF (NPT.EQ.0) THEN
            XP = 0.5*(XBLC+XTRC)
            YP = 0.5*(YBLC+YTRC)
          ELSE
            XP = X(NPT)
            YP = Y(NPT)
          END IF
          IF (NPT.EQ.1) THEN
C           -- delete 2nd point: redraw dot at first point
            CALL PGPT(1,X(1),Y(1),1)
          END IF
C
C X (EXIT) command:
C
      ELSE IF (LETTER.EQ.'X') THEN
          CALL GRETXT
          RETURN
C
C Illegal command:
C
      ELSE
          CALL GRMSG('Commands are A (add), D (delete), X (exit).')
      END IF
C
      GOTO 100
      END
