
      SUBROUTINE PERIOD_SCARGLE(X, Y, N, WK1, WK2, NDIM, NOUT, IFAIL,
     :                          FMIN, FMAX, FINT, INFO,
     :                          AVE, VAR, XMIN, XDIF,
     :                          OFAC, HIFAC, NFREQ)

C==============================================================================
C Subroutine for fast evaluation of the Lomb-Scargle statistic using the method
C described by Press, W. H. & Rybicki, G. B., 1989, Astrophysical Journal, 338,
C 277-280. Given N data points with abscissas X (which need not be equally
C spaced) and ordinates Y, this routine fills array WK1 with a sequence of
C NOUT increasing frequencies (not angular frequencies) from FMIN up to FMAX
C (with frequency interval FMIN) and fills array WK2 with the values of
C the Lomb-Scargle normalized periodogram at those frequencies. The arrays X and
C Y are not altered.
C If any errors occur, IFAIL C is set to 1, otherwise IFAIL = 0.
C If INFO=1 then loop information is output to the screen.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 29-April-1992.
C
C Converted to Double Precision (KPD), August 2001
C==============================================================================

      IMPLICIT NONE

C------------------------------------------------------------------------------
C Number of interpolation points per 1/4 cycle of highest frequency.
C------------------------------------------------------------------------------

      INTEGER MACC
      PARAMETER (MACC=4)

C------------------------------------------------------------------------------
C PERIOD_SCARGLE declarations.
C------------------------------------------------------------------------------

      INTEGER     NOUT, N, IFAIL, INFO, K, IEL, LOOP
      INTEGER     ISTEP, NDIM, J, NFREQ, NFREQT
      DOUBLE PRECISION FMIN, FMAX, FINT
      DOUBLE PRECISION AVE, VAR
      DOUBLE PRECISION X(N), Y(N), WK1(NDIM), WK2(NDIM)
      DOUBLE PRECISION XMIN, XDIF
      DOUBLE PRECISION FNDIM, OFAC, HIFAC, FAC, CK, CKK, DEN, DF
      DOUBLE PRECISION CTERM, STERM, HYPO, HC2WT, HS2WT, CWT, SWT
      DATA ISTEP/50/

      NOUT = IDINT(0.5D0*OFAC*HIFAC*DFLOAT(N))

C------------------------------------------------------------------------------
C Zero the workspaces.
C------------------------------------------------------------------------------

      DO 300 J = 1, NDIM
         WK1(J) = 0.0D0
         WK2(J) = 0.0D0
 300  CONTINUE
      FAC = DFLOAT(NDIM)/(XDIF*OFAC)
      FNDIM = DFLOAT(NDIM)

C------------------------------------------------------------------------------
C Extirpolate the data into the workspaces.
C------------------------------------------------------------------------------

      IFAIL = 0

      DO 400 J = 1, N
         CK = 1.0D0 + DMOD((X(J)-XMIN)*FAC, FNDIM)
         CKK = 1.0D0 + DMOD(2.0D0*(CK-1.0D0), FNDIM)

         CALL PERIOD_SPREAD(Y(J)-AVE, WK1, NDIM, CK, MACC, IFAIL)
         IF ( IFAIL.EQ.1 ) GO TO 600

         CALL PERIOD_SPREAD(1.0D0, WK2, NDIM, CKK, MACC, IFAIL)
         IF ( IFAIL.EQ.1 ) GO TO 600

 400  CONTINUE

C------------------------------------------------------------------------------
C Take the Fast Fourier Transforms.
C------------------------------------------------------------------------------

      CALL PERIOD_REALFT(WK1, NFREQ, 1)

      CALL PERIOD_REALFT(WK2, NFREQ, 1)

      DF = 1.0D0/(XDIF*OFAC)
      K = 3

C------------------------------------------------------------------------------
C Compute the Lomb-Scargle value for each frequency between FMIN, FMAX
C with step size FINT.
C------------------------------------------------------------------------------

      IEL = 0
      LOOP = 1
      DO 500 J = 1, NOUT
         IF ( (DFLOAT(J)*DF).LT.FMIN ) THEN
            K = K + 2
         ELSE
            HYPO = DSQRT(WK2(K)**2+WK2(K+1)**2)
            HC2WT = 0.5D0*WK2(K)/HYPO
            HS2WT = 0.5D0*WK2(K+1)/HYPO
            CWT = DSQRT(0.5D0+HC2WT)
            SWT = DSIGN(DSQRT(0.5D0-HC2WT), HS2WT)
            DEN = 0.5D0*DFLOAT(N) + HC2WT*WK2(K) + HS2WT*WK2(K+1)
            CTERM = (CWT*WK1(K)+SWT*WK1(K+1))**2/DEN
            STERM = (CWT*WK1(K+1)-SWT*WK1(K))**2/(DFLOAT(N)-DEN)
            IEL = IEL + 1
            WK1(IEL) = DFLOAT(J)*DF
            WK2(IEL) = (CTERM+STERM)/(2.0D0*VAR)
            IF ( INFO.EQ.1 ) THEN
               IF ( IEL.EQ.(LOOP*ISTEP) ) THEN
                  WRITE (*, 99001) WK1(IEL), WK2(IEL)
99001             FORMAT ('+Frequency =', D12.6,
     :                    ',  Lomb-Scargle Statistic =', D12.6)
                  LOOP = LOOP + 1
               END IF
            END IF
            K = K + 2
         END IF
 500  CONTINUE
      NOUT = IEL

 600  CONTINUE
      RETURN
      END
