C*GRCURS -- read cursor position
C+
      INTEGER FUNCTION GRCURS (IDENT,IX,IY,IXREF,IYREF,MODE,POSN,CH)
      INTEGER IDENT, IX, IY, IXREF, IYREF, MODE, POSN
      CHARACTER*(*) CH
C
C GRPCKG: Read the cursor position and a character typed by the user.
C The position is returned in absolute device coordinates (pixels).
C GRCURS positions the cursor at the position specified, and
C allows the user to move the cursor using the joystick or
C arrow keys or whatever is available on the device. When he has
C positioned the cursor, the user types a single character on his
C keyboard; GRCURS then returns this character and the new cursor
C position.
C
C "Rubber band" feedback of cursor movement can be requested (although
C it may not be supported on some devices). If MODE=1, a line from
C the anchor point to the current cursor position is displayed as
C the cursor is moved. If MODE=2, a rectangle with vertical and
C horizontal sides and one vertex at the anchor point and the opposite
C vertex at the current cursor position is displayed as the cursor is
C moved.
C
C Returns:
C
C GRCURS (integer): 1 if the call was successful; 0 if the device
C      has no cursor or some other error occurs. 
C
C Arguments:
C
C IDENT (integer, input):  GRPCKG plot identifier (from GROPEN).
C IX    (integer, in/out): the device x-coordinate of the cursor.
C IY    (integer, in/out): the device y-coordinate of the cursor.
C IXREF (integer, input):  x-coordinate of anchor point.
C IYREF (integer, input):  y-coordinate of anchor point.
C MODE  (integer, input):  type of rubber-band feedback.
C CH    (char,    output): the character typed by the user; if the device
C      has no cursor or if some other error occurs, the value CHAR(0)
C      [ASCII NUL character] is returned.
C--
C  1-Aug-1984 - extensively revised [TJP].
C 29-Jan-1985 - add ARGS and HP2648 devices (?) [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 11-Jun-1987 - remove built-ins [TJP].
C 15-Feb-1988 - remove test for batch jobs; leave this to the device
C               handler [TJP].
C 13-Dec-1990 - remove code to abort after 10 cursor errors [TJP].
C  7-Sep-1994 - add support for rubber-band modes [TJP].
C 17-Jan-1995 - start picture if necessary [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL           RBUF(6)
      INTEGER        NBUF, LCHR, ICURS, ERRCNT
      CHARACTER*16   CHR
      CHARACTER      C
      SAVE           ERRCNT
      DATA           ERRCNT/0/
C
C Validate identifier, and select device.
C
      CALL GRSLCT(IDENT)
      CALL GRTERM
C
C Begin picture if necessary.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C Make sure cursor is on view surface. (It does not
C have to be in the viewport.)
C
      IX = MAX(0,MIN(GRXMXA(GRCIDE),IX))
      IY = MAX(0,MIN(GRYMXA(GRCIDE),IY))
C
C Does the device have a cursor?
C
      C = GRGCAP(GRCIDE)(2:2)
      ICURS = 0
      IF (C.EQ.'C' .OR. C.EQ.'X') ICURS=1
C
C Device does have a cursor.
C
      IF (ICURS.GT.0) THEN
C         -- initial position of cursor
          RBUF(1) = IX
          RBUF(2) = IY
C         -- reference point for rubber band
          RBUF(3) = IXREF
          RBUF(4) = IYREF
C         -- rubber band mode
          RBUF(5) = MODE
C         -- position cursor?
          RBUF(6) = POSN
          NBUF = 6
          LCHR = 0
          CALL GREXEC(GRGTYP,17,RBUF,NBUF,CHR,LCHR)
          IX = RBUF(1)
          IY = RBUF(2)
          CH = CHR(1:1)
          GRCURS = 1
C         -- error if driver returns NUL
          IF (ICHAR(CHR(1:1)).EQ.0) GRCURS = 0
C
C Other devices are illegal.
C
      ELSE
          CALL GREXEC(GRGTYP, 1,RBUF,NBUF,CHR,LCHR)
          LCHR = INDEX(CHR,' ')
          IF (ERRCNT.LE.10) CALL 
     1        GRWARN('output device has no cursor: '//CHR(:LCHR))
          CH = CHAR(0)
          GRCURS = 0
          ERRCNT = ERRCNT+1
      END IF
C
      END
