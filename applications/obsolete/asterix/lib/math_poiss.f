*+  MATH_POISS - Returns a random number according to the Poisson distribution
*                given the mean of the distribution
      INTEGER FUNCTION MATH_POISS(LAMB)
*
*    Description :
*
*    History :
*
*      9 Mar 90:  Original (BHVAD::SRD)
*     25 Oct 93 : Don't make assumption that LAMB>15 is Gaussian. Use new NAG
*                 routine in release 15. (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL LAMB
*
*    Functions :
*
      REAL MATH_RND
c     INTEGER G05DRF
*
*    Local variables :
*
      INTEGER IFAIL
      LOGICAL INITIALISE
*
*    Local Data :
*
      DATA INITIALISE/.TRUE./
*-

*    Initialise random number generator
      IF ( INITIALISE ) THEN
c       CALL G05CCF(NINT(MATH_RND()))
        INITIALISE=.FALSE.
      END IF

*    Poisson distribution
      IFAIL = 0
      MATH_POISS = INT( LAMB )
c     MATH_POISS = G05DRF( DBLE(LAMB), IFAIL )
      CALL MSG_PRNT( '*** WARNING: no PDA replacement for G05DRF' )

      END

