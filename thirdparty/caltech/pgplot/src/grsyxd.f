C*GRSYXD -- obtain the polyline representation of a given symbol
C+
      SUBROUTINE GRSYXD (SYMBOL, XYGRID, UNUSED)
      INTEGER SYMBOL
      INTEGER XYGRID(300)
      LOGICAL UNUSED
C
C Return the digitization coordinates of a character. Each character is
C defined on a grid with X and Y coordinates in the range (-49,49), 
C with the origin (0,0) at the center of the character.  The coordinate
C system is right-handed, with X positive to the right, and Y positive
C upward.  
C
C Arguments:
C  SYMBOL (input)  : symbol number in range (1..3000).
C  XYGRID (output) : height range, width range, and pairs of (x,y)
C                    coordinates returned.  Height range = (XYGRID(1),
C                    XYGRID(3)).  Width range = (XYGRID(4),XYGRID(5)).
C                    (X,Y) = (XYGRID(K),XYGRID(K+1)) (K=6,8,...).
C  UNUSED (output) : receives .TRUE. if SYMBOL is an unused symbol
C                    number. A character of normal height and zero width
C                    is returned. Receives .FALSE. if SYMBOL is a 
C                    valid symbol number.
C 
C The height range consists of 3 values: (minimum Y, baseline Y,
C maximum Y).  The first is reached by descenders on lower-case g, p,
C q, and y.  The second is the bottom of upper-case letters.  The third
C is the top of upper-case letters.  A coordinate pair (-64,0) requests
C a pen raise, and a pair (-64,-64) terminates the coordinate list. It
C is assumed that movement to the first coordinate position will be
C done with the pen raised - no raise command is explicitly included to
C do this. 
C--
C  7-Mar-1983.
C 15-Dec-1988 - standardize.
C-----------------------------------------------------------------------
      INTEGER*2    BUFFER(27000)
      INTEGER      INDEX(3000), IX, IY, K, L, LOCBUF
      INTEGER      NC1, NC2
      COMMON       /GRSYMB/ NC1, NC2, INDEX, BUFFER
C
C Extract digitization.
C
      IF (SYMBOL.LT.NC1 .OR. SYMBOL.GT.NC2) GOTO 3000
      L = SYMBOL - NC1 + 1
      LOCBUF = INDEX(L)
      IF (LOCBUF .EQ. 0) GOTO 3000
      XYGRID(1) = BUFFER(LOCBUF)
      LOCBUF = LOCBUF + 1
      K = 2
      IY = -1
C     -- DO WHILE (IY.NE.-64)
  100 IF (IY.NE.-64) THEN
          IX = BUFFER(LOCBUF)/128
          IY = BUFFER(LOCBUF) - 128*IX - 64
          XYGRID(K) = IX - 64
          XYGRID(K+1) = IY
          K = K + 2
          LOCBUF = LOCBUF + 1
      GOTO 100
      END IF
C     -- end DO WHILE
      UNUSED = .FALSE.
      RETURN
C
C Unimplemented character.
C
3000  XYGRID(1) = -16
      XYGRID(2) =  -9
      XYGRID(3) = +12
      XYGRID(4) =   0
      XYGRID(5) =   0
      XYGRID(6) = -64
      XYGRID(7) = -64
      UNUSED = .TRUE.
      RETURN
      END
