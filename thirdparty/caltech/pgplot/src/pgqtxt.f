C*PGQTXT -- find bounding box of text string
C%void cpgqtxt(float x, float y, float angle, float fjust, \
C% const char *text, float *xbox, float *ybox);
C+
      SUBROUTINE PGQTXT (X, Y, ANGLE, FJUST, TEXT, XBOX, YBOX)
      REAL X, Y, ANGLE, FJUST
      CHARACTER*(*) TEXT
      REAL XBOX(4), YBOX(4)
C
C This routine returns a bounding box for a text string. Instead
C of drawing the string as routine PGPTXT does, it returns in XBOX
C and YBOX the coordinates of the corners of a rectangle parallel
C to the string baseline that just encloses the string. The four
C corners are in the order: lower left, upper left, upper right,
C lower right (where left and right refer to the first and last
C characters in the string).
C
C If the string is blank or contains no drawable characters, all
C four elements of XBOX and YBOX are assigned the starting point
C of the string, (X,Y).
C
C Arguments:
C  X, Y, ANGLE, FJUST, TEXT (input) : these arguments are the same as
C                    the corrresponding arguments in PGPTXT.
C  XBOX, YBOX (output) : arrays of dimension 4; on output, they
C                    contain the world coordinates of the bounding
C                    box in (XBOX(1), YBOX(1)), ..., (XBOX(4), YBOX(4)).
C--
C 12-Sep-1993 - new routine [TJP].
C  8-Nov-1994 - return something for blank string [TJP].
C 14-Jan-1997 - additional explanation [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      INTEGER I, L, GRTRIM
      REAL D, XP, YP, XPBOX(4), YPBOX(4), XOFFS, YOFFS
C
      IF (PGNOTO('PGQTXT')) RETURN
C
      L = GRTRIM(TEXT)
      IF (L.LE.0) THEN
         DO 15 I=1,4
            XBOX(I) = X
            YBOX(I) = Y
 15      CONTINUE
      ELSE
         D = 0.0
         IF (FJUST.NE.0.0) CALL GRLEN(TEXT(1:L),D)
         XOFFS = PGXORG(PGID) - D*FJUST*COS(ANGLE/57.29578)
         YOFFS = PGYORG(PGID) - D*FJUST*SIN(ANGLE/57.29578)
         XP = X*PGXSCL(PGID) + XOFFS
         YP = Y*PGYSCL(PGID) + YOFFS
         CALL GRQTXT(ANGLE, XP, YP, TEXT(1:L), XPBOX, YPBOX)
         DO 25 I=1,4
            XBOX(I) = (XPBOX(I) - PGXORG(PGID))/PGXSCL(PGID)
            YBOX(I) = (YPBOX(I) - PGYORG(PGID))/PGYSCL(PGID)
 25      CONTINUE
      END IF
      END
