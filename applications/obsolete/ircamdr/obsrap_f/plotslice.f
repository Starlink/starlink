      SUBROUTINE PLOTSLICE(PIG,CLEANSIZE,MAXY,MINY,NOISEPEAK)

*     ARGUMENTS

*     PIG (input)                           Input 2d array
*     REAL (45,45)

*     CLEANSIZE (input)                     Size of array actually containing
*     INTEGER                               info

*     FILE (input)                          Name of output PGPLOT file
*     CHARACTER*30

*     MAXY,MINY (input)                     Max and min values for y axis in
*     REAL                                  pgplot

*     NOISEPEAK (input)                     2 sigma sky level
*     REAL

C     Declare arrays

      REAL*4 PIG(45,45),XCUTPIXEL(45),XCUTFLUX(45),
     >       D(2),E(2),F(2),G(2),YCUTPIXEL(45),YCUTFLUX(45)

C     Declare other stuff

      REAL*4 MAXY,MINY,NOISEPEAK,A,B,C
      INTEGER I,J,CLEANSIZE
!      CHARACTER*30 FILE,FILE1

C     Form the cut thru centre of image in x direction


      I=(CLEANSIZE+1)/2
      DO J=1,CLEANSIZE
         XCUTPIXEL(J)=REAL(J)
         XCUTFLUX(J)=PIG(I,J)
      ENDDO

      A=REAL(CLEANSIZE)
      B=1.2*MINY
      C=1.2*MAXY
      D(1)=0.0
      D(2)=A
      F(1)=0.0
      F(2)=A
      E(1)=NOISEPEAK
      E(2)=E(1)
      G(1)=-NOISEPEAK
      G(2)=G(1)

      CALL PGBEGIN(12,'?',1,1)
      CALL PGENV(0.0,A,B,C,0,0)
      CALL PGLABEL('PIXELS','COUNTS','XSLICE THRU IMAGE CENTRE')
      CALL PGLINE(CLEANSIZE,XCUTPIXEL,XCUTFLUX)
      CALL PGLINE(2,D,E)
      CALL PGLINE(2,F,G)
      CALL PGEND

C     Form the cut thru centre of image in Y direction


      J=(CLEANSIZE+1)/2
      DO I=1,CLEANSIZE
         YCUTPIXEL(I)=REAL(I)
         YCUTFLUX(I)=PIG(I,J)
      ENDDO


      CALL PGBEGIN(13,'?',1,1)
      CALL PGENV(0.0,A,B,C,0,0)
      CALL PGLABEL('PIXELS','COUNTS','YSLICE THRU IMAGE CENTRE')
      CALL PGLINE(CLEANSIZE,YCUTPIXEL,YCUTFLUX)
      CALL PGLINE(2,D,E)
      CALL PGLINE(2,F,G)
      CALL PGEND

      RETURN
      END
