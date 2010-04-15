      SUBROUTINE CONTPLOT(CLEANARRAY,CLEANSIZE,SKY,SKYSIGMA)

*     ARGUMENTS

*     CLEANARRAY (input)                 The extracted image to be cleaned
*     REAL*4 (CLEANSIZE,CLEANSIZE)

*     CLEANSIZE (input)                  The side length of the extracted image
*     INTEGER

*     SKY,SKYSIGMA (input)               The median sky value and error as
*     REAL                               calculated by subroutine sky


C     Declare array

      REAL*4 CLEANARRAY(45,45),CONTARRAY(45,45),TR(6),C(3)

C     Declare other stuff

      INTEGER CLEANSIZE,NC,I,J
      REAL*4 A,SKY,SKYSIGMA,MAX
!      CHARACTER*30 FILE

      A=REAL(CLEANSIZE)


C     First subtract sky from array and find max pixel

      MAX=0.

      DO J=1,CLEANSIZE
         DO I=1,CLEANSIZE
            CONTARRAY(I,J)=CLEANARRAY(I,J)-SKY
            IF (CONTARRAY(I,J).GT.MAX) THEN
               MAX=CONTARRAY(I,J)
            ENDIF
         ENDDO
      ENDDO

      WRITE(6,'(A)')' SKY VALUE, SKY SIGMA AND MAX PIXEL ARE:'
      WRITE(6,*)SKY,SKYSIGMA,MAX

C     Provisionally try 3 contour levels

      NC=3
      C(2)=MAX/2.
      C(1)=C(2)/2.0
      C(3)=C(1)+C(2)
      TR(1)=0.
      TR(2)=1.
      TR(3)=0.
      TR(4)=0.
      TR(5)=0.
      TR(6)=1.

      CALL PGBEGIN(9,'?',1,1)
      CALL PGPAPER(6.0,1.0)
      CALL PGENV(0.0,A,0.0,A,0,0)
      CALL PGLABEL(' ',' ','CLEAN SUBIMAGE')
      CALL PGCONT(CONTARRAY,45,45,1,CLEANSIZE,1,
     >CLEANSIZE,C,NC,TR)

      WRITE(6,'(A)')' CONTOUR PLOT PLOTTED '

      RETURN
      END
