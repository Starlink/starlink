
      SUBROUTINE PDA_PRTVEC(VECTOR,NCOLS,NAME)
C  This subroutine prints the double precision vector named VECTOR.
C  Elements 1 thru NCOLS will be printed. NAME is a character variable
C  that describes VECTOR. Note that if NAME is given in the call to
C  PDA_PRTVEC, it must be enclosed in quotes. If there are more than 10
C  elements in VECTOR, 10 elements will be printed on each line.

      INTEGER NCOLS
      DOUBLE PRECISION VECTOR(NCOLS)
      CHARACTER *(*) NAME

      WRITE(*,1001) NAME

      IF (NCOLS .GT. 10) THEN
         LINES = INT(NCOLS/10.)

         DO 100, I = 1, LINES
            LL = 10*(I - 1)
            WRITE(*,1000) (VECTOR(J),J = 1+LL, 10+LL)
  100    CONTINUE

         WRITE(*,1000) (VECTOR(J),J = 11+LL, NCOLS)
      ELSE
         WRITE(*,1000) (VECTOR(J),J = 1, NCOLS)
      END IF

 1000 FORMAT( 10(G12.5,1X))
 1001 FORMAT(/,25X,A)

      RETURN
      END
