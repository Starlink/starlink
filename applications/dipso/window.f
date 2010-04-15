      SUBROUTINE WINDOW (X, NPT, FRLO, FRHI, FRSP, FR, WIN, NFR, OK)
C
C  Evaluate the window function of data in array X. For theory, see
C    J.D. Scargle, (1982), Astrophys. J., 263:835.
C
      IMPLICIT NONE
C Imports:
      INTEGER NPT                               ! no. values in array X
      DOUBLE PRECISION X(NPT)                   ! x data
      DOUBLE PRECISION FRLO                     ! lower limit of frequency range
      DOUBLE PRECISION FRHI                     ! upper limit of frequency range
      DOUBLE PRECISION FRSP                     ! frequency spacing
C Exports:
      REAL FR(*)                                ! array of frequencies
      REAL WIN(*)                               ! array of window values
      INTEGER NFR                               ! number of (fr,win) pairs
      LOGICAL OK                                ! success flag
C Local:
      DOUBLE PRECISION DX                       ! x-spacings
      DOUBLE PRECISION DXMIN                    ! smallest X-spacing
      DOUBLE PRECISION FRMAX                    ! pseudo-Nyquist frequency
      DOUBLE PRECISION FREQ                     ! holds doub. prec. frequencies
      DOUBLE PRECISION SINSUM                   ! sum of sine terms
      DOUBLE PRECISION COSSUM                   ! sum of cosine terms
      DOUBLE PRECISION TPI                      ! two-times-pi
      PARAMETER ( TPI = 6.2831853072D+00 )
      INTEGER NFRMAX                        ! maximum size of FR and WIN arrays
      PARAMETER ( NFRMAX = 20000 )
      INTEGER I, J                              ! do-loop variables
      OK = .TRUE.
C
C  Check that x-values are in increasing order and find the smallest X-spacing
C
      IF (NPT .LE. 2) THEN
        WRITE (*,
     :  '(''   PDGWINDO:  not enough x values'')')
        OK = .FALSE.
        RETURN
      ENDIF
      DXMIN = X(2) - X(1)
      DO I = 2,NPT
        DX = X(I) - X(I-1)
        IF (DX .LT. 0.0D+00) THEN
          WRITE (*,
     :    '(''   PDGWINDO:  x values must be in increasing order'')')
          OK = .FALSE.
          RETURN
        ENDIF
        IF (DX .LT. DXMIN) DXMIN = DX
      ENDDO
C
C  Evaluate the Pseudo-Nyquist frequency. This is set equal to half the
C reciprocal of the smallest X-spacing
C
      IF ( DXMIN .GT. 1.0D-10 ) THEN
        FRMAX = 0.5D+00 / DXMIN
*       WRITE(*,*) 'pseudo-Nyquist frequency = ',FRMAX
      ENDIF
C
C  Evaluate window.
C
      NFR = NINT( (FRHI-FRLO)/FRSP ) + 1
      IF (NFR .GT. NFRMAX) THEN
*       WRITE(*,*) '    WINDOW: frequency range is too wide '
        WRITE (*,
     :  '(''   PDGWINDO:  too many frequencies specified'')')
        OK = .FALSE.
        RETURN
      ENDIF
      DO I = 1,NFR
        FREQ = FRLO + FRSP*DBLE( I-1 )
        FR(I) = REAL( FREQ )
        FREQ = FREQ * TPI
        SINSUM = 0.0D+00
        COSSUM = 0.0D+00
        DO J = 1,NPT
          SINSUM = SINSUM + DSIN( FREQ*X(J) )
          COSSUM = COSSUM + DCOS( FREQ*X(J) )
        ENDDO
        WIN(I) = REAL( (SINSUM**2 + COSSUM**2) / DBLE( NPT**2 ) )
      ENDDO
      RETURN
      END
