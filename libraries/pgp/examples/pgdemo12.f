      PROGRAM PGDE12
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT: use of PGPAP to change the
C size of the view surface.
C
C
C Note that PGPAP must be called either immediately after PGOPEN or
C immediately before PGPAGE, and it affects all subsequent pages until
C the next call to PGPAP. (In the following code, PGENV calls PGPAGE.)
C-----------------------------------------------------------------------
      INTEGER PGOPEN

      WRITE (*,*) 'Demonstration of routine PGPAP to change the size of'
      WRITE (*,*) 'the view surface'
      WRITE (*,*)

      IF (PGOPEN('?') .LE. 0) STOP

      WRITE (*,*) 'First page: size is 7 x 3.5 inch'

      CALL PGPAP(7.0,0.5)
      CALL PGENV(0.0,1.0,0.0,2.0,0,0)
      CALL PGLAB('x','y','1')

      WRITE (*,*) 'Second page: size to 6 x 6 inch'

      CALL PGPAP(6.0,1.0)
      CALL PGENV(0.0,1.0,0.0,2.0,0,0)
      CALL PGLAB('x','y','2')

      WRITE (*,*) 'Third page: same size as second'

      CALL PGENV(0.0,1.0,0.0,2.0,0,0)
      CALL PGLAB('x','y','3')

      CALL PGCLOS
      END
