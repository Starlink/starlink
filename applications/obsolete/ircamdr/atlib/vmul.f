      SUBROUTINE VMUL(A,N,V,X)
*+
*   VMUL
*
*   Multiply a sub-area of a matrix by a vector
*
*   Given      (arguments)
*   A      DA   containing matrix (5,5)
*   N      I    size of sub-area
*   V      DA   containing input vector
*
*   Returned   (arguments)
*   X      DA   output vector
*
*   B.D.Kelly/ROE/5.4.1982
*-
      DOUBLE PRECISION A(5,5),V(5),X(5)
      INTEGER N

      DO I=1,N
         X(I)=0.0
         DO J=1,N
            X(I)=X(I)+A(I,J)*V(J)
         ENDDO
      ENDDO

      END
