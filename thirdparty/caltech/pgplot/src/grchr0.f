C*GRCHR0 -- support routine for GRCHAR and GRMARK
C+
      SUBROUTINE GRCHR0 (WINDOW,CENTER,ORIENT,ABSXY,X0,Y0,STRING)
C
C GRPCKG (internal routine): Support routine for GRCHAR and GRMARK.
C Draw a string of characters.
C
C Arguments:
C
C WINDOW (input, logical): if .TRUE., the plot is windowed in the
C      current window.
C CENTER (input, logical): if .TRUE., the first character of the string
C      is centered at (X0,Y0); otherwise the bottom left corner of the
C      first character is placed at (X0,Y0).
C ORIENT (input, real): the angle in degrees that the string is to make
C      with the horizontal, increasing anticlockwise.
C ABSXY (input, logical): if .TRUE., (X0,Y0) are absolute device
C      coordinates; otherwise they are world coordinates (the scaling
C      transformation is applied).
C X0, Y0 (input, real): position of first character (see CENTER).
C STRING (input, character): the string of ASCII characters; control
C      characters 0-20 have special representations; all other
C      non-graphic characters are plotted as blank spaces.
C
C (1-Mar-1983)
C-----------------------------------------------------------------------
      INTEGER  DOT, MOVE, VECSIZ
      REAL     PI
      PARAMETER (DOT = 3)
      PARAMETER (MOVE = 2)
      PARAMETER (VECSIZ = 30)
      PARAMETER (PI = 3.14159265359)
      INCLUDE 'grpckg1.inc'
      CHARACTER*(*) STRING
      CHARACTER*1   NEXT
      REAL     XMIN, XMAX, YMIN, YMAX
      INTEGER  MODE,LSTYLE,LEVEL
      INTEGER  I, J, L, CH, POINTS
      LOGICAL  ABSXY, CENTER, MORE, WINDOW
      REAL     ORIENT, X0, Y0
      REAL     ANGLE, FACTOR, BASE, FAC
      REAL     COSA, SINA
      REAL     DX, DY, XORG, YORG
      REAL     XC(VECSIZ), YC(VECSIZ), XT, YT
C
      IF (LEN(STRING).LE.0) RETURN
C
C Compute scaling and orientation.
C
      CALL GRQLS(LSTYLE)
      CALL GRSLS(1)
      ANGLE = (AMOD(ORIENT, 360.0) / 180.0) * PI
      FACTOR = GRCFAC(GRCIDE)
      COSA = FACTOR * COS(ANGLE)
      SINA = FACTOR * SIN(ANGLE)
      DX = 10.0 * COSA
      DY = 10.0 * SINA
      CALL GRTXY0(ABSXY, X0, Y0, XORG, YORG)
      IF (.NOT.WINDOW) THEN
          XMIN = GRXMIN(GRCIDE)
          XMAX = GRXMAX(GRCIDE)
          YMIN = GRYMIN(GRCIDE)
          YMAX = GRYMAX(GRCIDE)
          CALL GRAREA(GRCIDE, 0.0, 0.0, 0.0, 0.0)
      END IF
C
C Plot the string of characters.
C
      MODE = MOVE
      BASE = 0.0
      FAC = 1.0
      I = 1
      LEVEL = 0
      L = LEN(STRING)
C     -- DO WHILE (I.LE.L)
   10 IF (I.LE.L) THEN
        IF (I.LT.L .AND. STRING(I:I).EQ.CHAR(92)) THEN
            CALL GRTOUP(NEXT,STRING(I+1:I+1))
            IF (NEXT.EQ.'U') THEN
                LEVEL = LEVEL+1
                BASE = BASE + 4.0*FAC
                FAC = 0.6**IABS(LEVEL)
                I = I+2
            ELSE IF (NEXT.EQ.'D') THEN
                LEVEL = LEVEL-1
                FAC = 0.6**IABS(LEVEL)
                BASE = BASE - 4.0*FAC
                I = I+2
            ELSE
                I = I+1
            END IF
        ELSE
          CH = ICHAR(STRING(I:I))
          IF (CH.GT.127 .OR. CH.LT.0) CH = ICHAR(' ')
          MORE = .TRUE.
C         -- DO WHILE (MORE)
   20     IF (MORE) THEN
            CALL GRGTC0(CH, CENTER, POINTS, XC, YC, MORE)
            DO 30 J=1,POINTS
                    XT = XC(J)*FAC
                    YT = YC(J)*FAC + BASE
                    XC(J) = XORG + COSA * XT - SINA * YT
                    YC(J) = YORG + SINA * XT + COSA * YT
   30       CONTINUE
            IF (POINTS.EQ.1) MODE = DOT
            IF (POINTS.GT.0) CALL GRVCT0(MODE,.TRUE.,POINTS,XC,YC)
            IF (POINTS.EQ.1) MODE = MOVE
          GOTO 20
          END IF
C         -- end DO WHILE
          XORG = XORG + DX*FAC
          YORG = YORG + DY*FAC
          I = I+1
        END IF
      GOTO 10
      END IF
C     -- end DO WHILE
C
C Clean up and return.
C
      IF (.NOT.WINDOW) THEN
          GRXMIN(GRCIDE) = XMIN
          GRXMAX(GRCIDE) = XMAX
          GRYMIN(GRCIDE) = YMIN
          GRYMAX(GRCIDE) = YMAX
      END IF
      CALL GRSLS(LSTYLE)
      RETURN
      END
