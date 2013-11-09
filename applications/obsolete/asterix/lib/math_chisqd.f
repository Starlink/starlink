*+  MATH_CHISQD - Return chi-sq deviate associated with given level of confidence
      SUBROUTINE MATH_CHISQD( CONF, NDOF, DEVIATE, STATUS )
*
*    Description :
*
*     Returns the deviate associated with the lower tail probability
*     found by subtracting the given confidence level from unity.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*     Richard Beard (RB)
*
*    History :
*
*     18 Jun 92 : Original (DJA)
*     13 Mar 97 : Correct NAG arguments (RB)
*     21 Jun 87 : Replace NAG with ASTPDA (RB)
*
*    Type definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      REAL                         CONF           ! Confidence
      INTEGER                      NDOF           ! # degrees of freedom
*
*    Export :
*
      REAL                         DEVIATE        ! Deviate
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      DOUBLE PRECISION             PDA_CHIDEV
*
*    Local variables :
*
      DOUBLE PRECISION             PROB           ! Probability
      INTEGER                      IFAIL          ! PDA status
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Convert confidence to probability
      PROB = 1.0D0 - DBLE(CONF)

*    Get deviate
      IFAIL = 0
      DEVIATE = PDA_CHIDEV( PROB, DBLE(NDOF), IFAIL )

      END
