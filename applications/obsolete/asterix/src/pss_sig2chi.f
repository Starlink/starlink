*+  PSS_SIG2CHI - Convert significance into delta chi-squared
      REAL FUNCTION PSS_SIG2CHI( SIG, STATUS )
*
*    Description :
*
*     Returns that deviate of a chi-squared distribution with one degree
*     of freedom whose probability is equal to that of the deviate SIG
*     of a one tailed normal distribution.
*
*    Method :
*
*     IF significance < 5 THEN
*       Find probability from SENSIG
*       Convert to delta chisquared
*     ELSE
*       Can't use above as intermediate probability too small. Instead
*       use result of series expansion
*
*           -sig^2/2 - ln sig = ln 2 - DELCHI/2 - ln DELCHI^0.5
*
*       and iterate using Newton-Raphson iteration.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     13 Jan 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER                      STATUS
*
*    Import :
*
      REAL                         SIG			! Significance
*
*    Function declarations :
*
      DOUBLE PRECISION             S15ABF
*
*    Local constants :
*
      INTEGER                      NDOF                 ! Degrees of freedom
        PARAMETER                  ( NDOF = 1 )
      REAL                         TOLERANCE            ! Convergence tolerance
        PARAMETER                  ( TOLERANCE = 0.001 )
*
*    Local variables :
*
      DOUBLE PRECISION             PROB

      REAL                         DC, F, DF, CONSIG, OLDDEL

      INTEGER                      ISTAT
*-

*    Convert to a change in Cash statistic
      IF ( SIG .LE. 5.0 ) THEN
        ISTAT = 1
        PROB = 2.0D0 * S15ABF( DBLE(SIG), ISTAT) - 1.0D0
        CALL MATH_CHISQD( REAL(1.0D0-PROB), NDOF, DC, STATUS )

      ELSE
        OLDDEL = -1.0
        DC = SIG**2
        CONSIG = SIG**2/2.0 + LOG(SIG)
        DO WHILE ( ABS((OLDDEL-DC)/DC) .GT. TOLERANCE )
          F = LOG(2.0) - DC/2.0 - LOG(DC**0.5) + CONSIG
          DF = -0.5 - 0.5 / DC
          OLDDEL = DC
          DC = DC - F / DF
        END DO
      END IF

      PSS_SIG2CHI = DC

      END
