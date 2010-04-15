      SUBROUTINE PERIGRAM (X, Y, NPT, PMASK, FRLO, FRHI,
     +                         DFR, FR, PER, NFR, OK)
C
C  Evaluate the Periodogram (PER) of (X,Y) data. For theory, see
C    J.D. Scargle, (1982), Astrophys. J., 263:835.
C
      IMPLICIT NONE
C Imports:
      INTEGER NPT                               ! no. datapairs
      DOUBLE PRECISION X(NPT), Y(NPT)           ! (x,y) datapairs
      DOUBLE PRECISION PMASK    ! proportion of data to be endmasked either end
      DOUBLE PRECISION FRLO                     ! lower end of frequency range
      DOUBLE PRECISION FRHI                     ! upper end of frequency range
      DOUBLE PRECISION DFR                      ! frequency spacing
C Exports:
      REAL FR(*)                                ! array of frequencies
      REAL PER(*)                               ! array of Periodogram values
      INTEGER NFR                               ! number of (fr,win) pairs
      LOGICAL OK                                ! success flag
C Local:
      DOUBLE PRECISION DX                       ! x-spacings
      DOUBLE PRECISION DXMIN                    ! smallest X-spacing
      DOUBLE PRECISION FRMAX                    ! pseudo-Nyquist frequency
      DOUBLE PRECISION FREQ                     ! holds doub. prec. frequencies
      DOUBLE PRECISION FRQ2                     ! holds twice angular freq.
      DOUBLE PRECISION SINSUM                   ! sum of sine terms
      DOUBLE PRECISION COSSUM                   ! sum of cosine terms
      DOUBLE PRECISION SSQSUM                   ! sum of sine-squared terms
      DOUBLE PRECISION CSQSUM                   ! sum of cosine-squared terms
      DOUBLE PRECISION EVSUM                    ! sum of "even terms"
      DOUBLE PRECISION ODSUM                    ! sum of "odd terms"
      DOUBLE PRECISION TAU                      ! delay term
      DOUBLE PRECISION TPI                      ! two-times-pi
      PARAMETER ( TPI = 6.2831853072D+00 )
      INTEGER NFRMAX                        ! maximum size of FR and WIN arrays
      PARAMETER ( NFRMAX = 20000 )
      INTEGER I, J                              ! do-loop variables
      OK = .TRUE.
C
C  Endmask the data
C
      CALL YENDMSK (X, Y, NPT, PMASK)
C
C  Check that x-values are in increasing order and find the smallest X-spacing
C
      DXMIN = X(2) - X(1)
      DO I = 2,NPT
        DX = X(I) - X(I-1)
        IF (DX .LT. 0.0D+00) THEN
          WRITE(*,*) '    PDGRAM:  X-values not in increasing order '
          OK = .FALSE.
          RETURN
        ENDIF
        IF (DX .LT. DXMIN) DXMIN = DX
      ENDDO
C
C  Evaluate the Pseudo-Nyquist frequency. This is set equal to half the
C reciprocal of the smallest X-spacing
C
      IF ( DXMIN .GT. 1.0D-10 )THEN
        FRMAX = 0.5D+00 / DXMIN
*       WRITE(*,*) 'pseudo-Nyquist frequency = ',FRMAX
      ENDIF
C
C  Evaluate Periodogram for up to NFRMAX frequencies.
C
      NFR = NINT( (FRHI-FRLO)/DFR ) + 1
      IF (NFR .GT. NFRMAX) THEN
        WRITE (*,
     :  '(''   PDGRAM:  too many frequencies specified'')')
        OK = .FALSE.
        RETURN
      ENDIF
      DO I = 1,NFR
        FREQ = FRLO + DFR * DBLE( I-1 )
        FR(I) = REAL( FREQ )
        FREQ = TPI * FREQ
        FRQ2 = 2.0D+00 * FREQ
C  Evaluate TAU.
        IF ( FREQ .NE. 0.0D+00 ) THEN
          SINSUM = 0.0D+00
          COSSUM = 0.0D+00
          DO J = 1,NPT
            SINSUM = SINSUM + DSIN( FRQ2*X(J) )
            COSSUM = COSSUM + DCOS( FRQ2*X(J) )
          ENDDO
          TAU = DATAN( SINSUM/COSSUM ) / FRQ2
        ELSE
          DO J = 1,NPT
            TAU = TAU + X(J)
          ENDDO
          TAU = TAU / DBLE(NPT)
        ENDIF
C  Accumulate sums.
        SSQSUM = 0.0D+00
        CSQSUM = 0.0D+00
        EVSUM = 0.0D+00
        ODSUM = 0.0D+00
        DO J = 1,NPT
          SSQSUM = SSQSUM + DSIN( FREQ * (X(J)-TAU) )**2
          CSQSUM = CSQSUM + DCOS( FREQ * (X(J)-TAU) )**2
          EVSUM = EVSUM + Y(J) * DCOS( FREQ * (X(J)-TAU) )
          ODSUM = ODSUM + Y(J) * DSIN( FREQ * (X(J)-TAU) )
        ENDDO
C  Calculate Periodogram for frequency FREQ
        IF (FREQ .NE. 0.0D+00 ) THEN
          PER(I) = 0.5 * REAL( EVSUM**2/CSQSUM + ODSUM**2/SSQSUM )
        ELSE
          PER(I) = 0.5 * REAL( EVSUM**2/CSQSUM )
        ENDIF
      ENDDO
      RETURN
      END
