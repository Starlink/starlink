*+  MATH_MEANPOSR - Finds mean of 2 celestial positions, with error
      SUBROUTINE MATH_MEANPOSR( RA1, DEC1, ER1, RA2, DEC2, ER2,
     :                                         ORA, ODEC, OER )
*
*    Description :
*
*     Finds mean celestial position of two input positions given errors. All
*     units are radians.
*
*     This subroutine could be made slightly more efficient by removing
*     references to 90-declinations. However, the geometrical origins of
*     the algorithm become far less clear.
*
*    History :
*
*      8 May 90 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL       RA1, DEC1, ER1
      REAL       RA2, DEC2, ER2
*
*    Export :
*
      REAL       ORA, ODEC, OER
*
*    Functions :
*
      DOUBLE PRECISION       SLA_DSEP
*
*    Local variables :
*
      REAL*8     SEP                        ! Separation of 2 positions

      REAL*8     A                          ! Angle between NCP, first position
                                            ! and new position
      REAL*8     A90                        ! Ninety degrees in radians
      REAL*8     ESQ                        ! Sum of squares of errors
      REAL*8     THETA                      ! Great circle offset from first
                                            ! pos to new pos
*-

      A90 = ( 2.0D0*DATAN(1.0D0))
      ESQ = ER1*ER1 + ER2*ER2

*    Find separation of sources
      SEP = SLA_DSEP( DBLE(RA1), DBLE(DEC1), DBLE(RA2), DBLE(DEC2) )
      THETA = DBLE( SEP * ER1 * ER1 / ESQ )
      A = DASIN( DSIN(A90-DBLE(DEC2)) * DSIN(DBLE(RA2-RA1)) /
     :                                           DSIN(SEP) )

*    Get output position
      ODEC = A90 - DACOS( DCOS(THETA)*DCOS(A90-DBLE(DEC1))
     :                    +DSIN(THETA)*DSIN(A90-DBLE(DEC1))*DCOS(A) )
      ORA  = RA1 + DASIN(DSIN(A)*DSIN(THETA)/
     :                   DSIN(A90-DBLE(ODEC)))

*    Output error
      OER = SQRT( (ER1*ER1*ER2*ER2)/ESQ )

      END
