
C==============================================================================

      SUBROUTINE PERIOD_SPREAD(Y, YY, N, X, M, IFAIL)

C==============================================================================
C Given an array YY of length N, extirpolate (spread) a value Y into M actual
C array elements that best approximate the "fictional" (ie. possibly
C non-integer) array element number X. The weights used are coefficients of
C the Lagrange interpolating polynomial.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 29-April-1992.
C
C Converted to Double Precision (KPD), August 2001
C==============================================================================

      IMPLICIT NONE

C------------------------------------------------------------------------------
C PERIOD_SPREAD declarations.
C------------------------------------------------------------------------------

      INTEGER M,IFAIL,J,N,IX,IHI,ILO
      DOUBLE PRECISION YY(N), NFAC(10)
      DOUBLE PRECISION FAC,NDEN,X,Y

      DATA NFAC /1.0D0, 1.0D0, 2.0D0, 6.0D0,
     :  24.0D0, 120.0D0, 720.0D0, 5040.0D0, 40320.0D0, 362880.0D0/

      IF ( M.GT.10 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *)
     :           '** ERROR: Factorial table too small in PERIOD_SPREAD.'
         IFAIL = 1
         GO TO 200
      END IF
      IFAIL = 0
      IX = IDINT(X)
      IF ( X.EQ.DFLOAT(IX) ) THEN
         YY(IX) = YY(IX) + Y
      ELSE
         ILO = MIN0(MAX0(IDINT(X-0.5D0*DFLOAT(M)+1.0D0),1), N-M+1)
         IHI = ILO + M - 1
         NDEN = NFAC(M)
         FAC = X - DFLOAT(ILO)
         DO 50 J = ILO + 1, IHI
            FAC = FAC*(X-DFLOAT(J))
 50      CONTINUE
         YY(IHI) = YY(IHI) + Y*FAC/(NDEN*(X-DFLOAT(IHI)))
         DO 100 J = IHI - 1, ILO, -1
            NDEN = (NDEN/DFLOAT(J+1-ILO))*DFLOAT(J-IHI)
            YY(J) = YY(J) + Y*FAC/(NDEN*(X-DFLOAT(J)))
 100     CONTINUE
      END IF

 200  CONTINUE
      RETURN
      END
