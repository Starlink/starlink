
      SUBROUTINE INVERS(A,N)
C+
C   INVERS
C
C   Invert a submatrix within a (5,5) matrix.
C
C   Given      (arguments)
C   N           size of square submatrix
C   A           containing matrix
C
C   Returned   (arguments)
C   A           matrix containing inverted submatrix
C
C	B.D KELLY/ROE/1981
C-
      DOUBLE PRECISION A(5,5)

      DO I=1,N
         IF(ABS(A(I,I)).GT.1.0E-10) THEN
            A(I,I)=1./A(I,I)
         ELSE
            A(I,I)=1.0E10
         ENDIF

         DO J=1,N
            IF(J.NE.I) THEN
               A(J,I)=-A(J,I)*A(I,I)
               DO K=1,N
                  IF(K.NE.I) A(J,K)=A(J,K)+A(J,I)*A(I,K)
               ENDDO
            ENDIF
         ENDDO
         DO K=1,N
            IF(K.NE.I) A(I,K)=A(I,K)*A(I,I)
         ENDDO
      ENDDO

      END
