      PROGRAM PGDE11
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT: travelling sine wave.
C
C This program illustrates how animated displays can be generated with
C PGPLOT, although PGPLOT is not optimized for such use.
C
C To create an animated display:
C
C  (1) Do not call PGPAGE (or PGENV, which calls PGPAGE) between frames;
C  (2) Enclose all the calls required to generate each frame between
C      PGBBUF and PGEBUF calls;
C  (3) Either: erase the entire previous frame by calling PGERAS before
C      drawing the next frame; or: erase the parts of the frame that
C      have changed by overwriting with the background color (color
C      index 0).
C
C This program demonstrated both approaches. Using PGERAS is usually
C slower, because more has to be redrawn in each frame. Erasing selected
C parts of the display can be faster, but it may be difficult to avoid
C erasing parts that should remain visible.
C
C This program requires an interactive display that supports writing
C in color index 0.
C-----------------------------------------------------------------------
C Parameters:
C   NT is the number of frames in the animation.
      INTEGER N,NT
      REAL PI,A,B
      PARAMETER (N = 50)
      PARAMETER (NT = 100)
      PARAMETER (PI=3.14159265359)
      PARAMETER (A = 2.0*PI/N)
      PARAMETER (B = 2.0*PI/NT)
C Variables:
      REAL X(0:N), Y(0:N)
      INTEGER I, T, L
      CHARACTER*8 STR
      INTEGER PGBEG
C-----------------------------------------------------------------------

      WRITE (*,*) 'Demonstration of animation with PGPLOT'
      WRITE (*,*) 'This program requires an interactive display that'
      WRITE (*,*) 'supports writing in color index 0.'

      IF (PGBEG(0,'?',1,1) .NE. 1) STOP

      CALL PGQINF('HARDCOPY', STR, L)
      IF (STR(:L).NE.'NO') WRITE (*,*)
     :     'Warning: device is not interactive'

      WRITE (*,*) '1: erasing the entire screen between frames'

      CALL PGPAGE
      CALL PGVSTD
      CALL PGWNAD(-A, A*(N+1), -1.1, 1.1)
      
      DO 200 T=0,NT
        CALL PGBBUF
        CALL PGERAS
        CALL PGSCI(1)
        CALL PGBOX('bcnst', 0.0, 0, 'bcnst', 0.0, 0)
        DO 100 I=0,N
          X(I) = I*A 
          Y(I) = SIN(I*A-T*B)  
  100   CONTINUE
        CALL PGSCI(3)
        CALL PGLINE(N+1,X,Y)
        WRITE (STR,'(I8)') T
        CALL PGMTXT('T', 2.0, 0.0, 0.0, STR)
        CALL PGEBUF
  200 CONTINUE

      CALL PGPAGE
      WRITE (*,*) '2: erasing only the line between frames'

      CALL PGVSTD
      CALL PGWNAD(-A, A*(N+1), -1.1, 1.1)
      CALL PGBBUF
      CALL PGSCI(1)
      CALL PGBOX('bcnst', 0.0, 0, 'bcnst', 0.0, 0)
      DO 300 I=0,N
         X(I) = I*A 
         Y(I) = SIN(I*A)  
 300  CONTINUE
      CALL PGEBUF
   
      DO 500 T=0,NT
        CALL PGBBUF
        CALL PGSCI(0)
        CALL PGLINE(N+1,X,Y)
        CALL PGSCI(3)
        DO 400 I=0,N
          X(I) = I*A 
          Y(I) = SIN(I*A-T*B)  
  400   CONTINUE
        CALL PGLINE(N+1,X,Y)
        CALL PGEBUF
  500 CONTINUE

      CALL PGEND
      END
