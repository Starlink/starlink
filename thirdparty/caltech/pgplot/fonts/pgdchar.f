      PROGRAM DCHAR
C----------------------------------------------------------------------
C Display construction of Hershey character.
C This program uses the PGPLOT internal routine GRSYXD and must
C therefore be linked with the non-shareable library.
C                              T. J. Pearson  1983 Feb 12
C----------------------------------------------------------------------
      INTEGER PGBEG, HEIGHT, DEPTH, WIDTH
      INTEGER          XYGRID(300),I,N,M
      REAL             XC,YC,X(5),BASE
      LOGICAL          UNUSED,MOVE
      CHARACTER*4      TEXT
C-----------------------------------------------------------------------
      IF (PGBEG(0,'?',1,1).NE.1) STOP
      CALL PGASK(.FALSE.)
   20 WRITE (*,'(A,$)') ' Symbol number: '
      M = N
      READ (*,*,END=30) N
      IF (N.EQ.0) N = M+1
      CALL GRSYXD(N,XYGRID,UNUSED)
      IF (UNUSED) THEN
          WRITE (*,'(A)') ' Symbol not defined'
          GOTO 20
      END IF
C
C Call PGENV to initialize the viewport and window; the
C AXIS argument is -2, so no frame or labels will be drawn.
C
      CALL PGBBUF
      CALL PGENV(-50.,50.,-50.,50.0,1,-2)
C
C Call PGBOX to draw a grid at low brightness.
C
      CALL PGSCI(15)
      CALL PGSLW(1)
      CALL PGBOX('G',10.0,0,'G',10.0,0)
      CALL PGSCI(5)
C
      DO 15 I=1,5
         X(I) = XYGRID(I)
   15 CONTINUE
C
C Shift coordinates so baseline is y=0; center is (0,-BASE)
C
      BASE = X(2)
      X(1) = X(1)-BASE
      X(3) = X(3)-BASE
      HEIGHT=X(3)
      DEPTH = X(1)
      WIDTH =X(5)-X(4)
      WRITE(*,*) N, HEIGHT, DEPTH, WIDTH
C
C Draw the `bounding box'.
C
      CALL PGMOVE(X(4),X(1))
      CALL PGDRAW(X(5),X(1))
      CALL PGDRAW(X(5),X(3))
      CALL PGDRAW(X(4),X(3))
      CALL PGDRAW(X(4),X(1))
C
C Draw the baseline.
C
      CALL PGMOVE(-50.0, 0.0)
      CALL PGDRAW(50.0, 0.0)
C
C Mark the `center' of the character.
C
      CALL PGPT(1, 0.0, -BASE, 9)
C
C Write the Hershey number in lower left corner.
C
      WRITE (TEXT,'(I4)') N
      CALL PGTEXT(-49.0, -49.0, TEXT)
C
      CALL PGSCI(3)
      CALL PGSLW(3)
      I = 6
      MOVE = .TRUE.
   26 XC = XYGRID(I)
      I = I+1
      IF (XYGRID(I).EQ.-64) THEN
          CALL PGEBUF
          GOTO 20
      END IF
      YC = XYGRID(I)-BASE
      I = I+1
      IF (XYGRID(I-2).EQ.-64) THEN
          MOVE = .TRUE.
          GOTO 26
      END IF
      IF (MOVE) THEN
          CALL PGMOVE(XC,YC)
          MOVE = .FALSE.
      ELSE
          CALL PGDRAW(XC,YC)
      END IF
      GOTO 26
C
C Don't forget to call PGEND!
C
   30 CALL PGEND
      END
