      SUBROUTINE TI( IA, NI, I )
      INTEGER NI, IA(NI), I
      INTEGER J

      I = 0
      DO J = 1, NI
         I = I + IA(J)
      END DO
      END

      SUBROUTINE TR( RA, NR, R )
      INTEGER NR
      REAL RA(NR), R
      INTEGER J

      R = 0
      DO J = 1, NR
         R = R + RA(J)
      END DO
      R = R / NR
      END

      SUBROUTINE TD( DA, ND, D )
      INTEGER ND
      DOUBLE PRECISION DA(ND), D
      INTEGER J

      D = 0
      DO J = 1, ND
         D = D + DA(J) * DA(J)
      END DO
      END

      SUBROUTINE TL( LA, NL, L )
      INTEGER NL
      LOGICAL LA(NL), L
      INTEGER J, K

      K  = 0
      DO J = 1, NL
         IF( LA(J) ) K = K + 1
      END DO
      L = K .EQ. 5
      END

      SUBROUTINE TB( BA, NB, B )
      INTEGER NB
      BYTE BA(NB), B
      INTEGER J

      B  = 0
      DO J = 1, NB
         B = B + BA(J)
      END DO
      END

      SUBROUTINE TW( WA, NW, W )
      INTEGER NW
      INTEGER*2 WA(NW), W
      INTEGER J

      W  = 0
      DO J = 1, NW
         W = W + WA(J)
      END DO
      END

      SUBROUTINE TUB( UBA, NUB, UB )
      INTEGER NUB
      BYTE UBA(NUB), UB
      INTEGER J

      UB  = 0
      DO J = 1, NUB
         UB = UB + UBA(J)
      END DO
      END

      SUBROUTINE TUW( UWA, NUW, UW )
      INTEGER NUW
      INTEGER*2 UWA(NUW), UW
      INTEGER J

      UW  = 0
      DO J = 1, NUW
         UW = UW + UWA(J)
      END DO
      END

      SUBROUTINE TC1( C1 )
      CHARACTER*(*) C1
      C1 = 'Test C1'
      END

      SUBROUTINE TC2( CA, N )
      CHARACTER*(*) CA(N)
      INTEGER J

      DO J = 1, N
         WRITE( CA(J), '(A,I2)' ) 'ELEMENT ',J
      END DO

      END
