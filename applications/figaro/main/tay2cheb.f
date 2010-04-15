C+
      SUBROUTINE TAY2CHEB(NORDER,XMIN,XMAX,C,A)
C
C     AJH - switched A and C above
C     TAY2CHEB
C
C     Converts polynomial coefficients held in Taylor series
C     form into Chebyshev polynomial coefficients.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NORDER    (Integer) The order of the polynomial. This
C                   must be less than or equal to 10.
C     (>) XMIN      (Double precision) The minimum value of the
C                   original X-range.
C     (>) XMAX      (Double precision) The maximum value of the
C                   original X-range.
C     (!) A         (Double precision array A(0:NORDER)) The
C                   Chebyshev series coefficients.
C                   The array is modified by this routine.
C     (<) C         (Double precision array C(0:NORDER)) The
C                   calculated power series coefficients.  The
C                   constant term comes first, in C(0).
C                   (The calling program can, of course, have
C                   dimensioned A and C as A(NORDER+1),C(NORDER+1))
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C     GEN_CHEB2NO
C     Original program by John Lucey, AAO.
C     Re-packaged by KS / AAO, 4th April 1985
C     Say in prologue that A() is messed about with. HME / UoE,
C        Starlink, 28th April 1995
C     TAY2CHEB - progam reverse engineered 3rd September 1997 AJH
C                AJH - switched A and C above
C+
      DOUBLE PRECISION XMAX,XMIN,A(0:10),C(0:10)
      DOUBLE PRECISION SUM,P(0:10),CHB(0:10,0:10)
      DOUBLE PRECISION COM(0:10,0:10),AA,BB,XRANGE

      DATA CHB/1.D0,0.D0,-1.D0,0.D0,1.D0,0.D0,-1.D0,0.D0,1.D0,0.D0,
     $        -1.D0,
     $         0.D0,1.D0,0.D0,-3.D0,0.D0,5.D0,0.D0,-7.D0,0.D0,9.D0,0.D0,
     $         2*0.D0,2.D0,0.D0,-8.D0,0.D0,18.D0,0.D0,-32.D0,0.D0,50.D0,
     $         3*0.D0,4.D0,0.D0,-20.D0,0.D0,56.D0,0.D0,-120.D0,0.D0,
     $         4*0.D0,8.D0,0.D0,-48.D0,0.D0,160.D0,0.D0,-400.D0,
     $         5*0.D0,16.D0,0.D0,-112.D0,0.D0,432.D0,0.D0,
     $         6*0.D0,32.D0,0.D0,-256.D0,0.D0,1120.D0,
     $         7*0.D0,64.D0,0.D0,-576.D0,0.D0,
     $         8*0.D0,128.D0,0.D0,-1280.D0,
     $         9*0.D0,256.D0,0.D0,
     $         10*0.D0,512.D0/

      DATA COM/11*1.D0,
     $   0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0,10.D0,
     $   2*0.D0,1.D0,3.D0,6.D0,10.D0,15.D0,21.D0,28.D0,36.D0,45.D0,
     $   3*0.D0,1.D0,4.D0,10.D0,20.D0,35.D0,56.D0,84.D0,120.D0,
     $   4*0.D0,1.D0,5.D0,15.D0,35.D0,70.D0,126.D0,210.D0,
     $   5*0.D0,1.D0,6.D0,21.D0,56.D0,126.D0,252.D0,
     $   6*0.D0,1.D0,7.D0,28.D0,84.D0,210.D0,
     $   7*0.D0,1.D0,8.D0,36.D0,120.D0,
     $   8*0.D0,1.D0,9.D0,45.D0,
     $   9*0.D0,1.D0,10.D0,
     $   10*0,1.D0/


      XRANGE=XMAX-XMIN
      AA=2.0D0/XRANGE
      BB=-(XMAX+XMIN)/XRANGE


*      print *,'tay2cheb'

      A(NORDER)=C(NORDER)/((AA**NORDER)*COM(NORDER,NORDER)*
     :     CHB(NORDER,NORDER))

      IF (NORDER.GT.1) THEN
         DO K=NORDER-1,0,-1
            SUM=0.D0
            DO I=K+2,NORDER
               SUM=SUM+CHB(I,K)*A(I)
            ENDDO
            P(K)=SUM
         ENDDO


         DO K=NORDER-1,0,-1
            SUM=0.D0
            DO I=K,NORDER
               SUM=SUM+COM(I,K)*(BB**(I-K))
            ENDDO
            A(K)=(C(K)/(SUM*(AA**K)))-P(K)
         ENDDO
      ENDIF

      A(0)=A(0)*2.D0

      END
