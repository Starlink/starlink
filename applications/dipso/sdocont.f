
       SUBROUTINE SDOCONT
     : (WAVE, FLUX, NPOINT, WORK)
!
!   Calculates smoothed continuum for sdO
!
       IMPLICIT NONE

       INTEGER I, J, NLOST, I1, I2, IERR, NPOINT, JTOT
       INTEGER NBIN, K
       INTEGER NB, NMAX
       PARAMETER (NB = 350)
       PARAMETER (NMAX=20000)

       REAL WAVE(1), FLUX(1), WORK(1)
       REAL FACTOR, W1, W2
       REAL FBIN(NB), WBIN(NB)

       DOUBLE PRECISION TEMP

       WRITE (*,
     : '(''   SDOCONT not implemented in this'',
     : '' version of DIPSO'',A)') 7

       END
