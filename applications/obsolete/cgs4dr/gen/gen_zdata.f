      SUBROUTINE GEN_ZDATA( NELM, DATA, QUAL )
*
*    Routine to zero a data element if the quality is bad.
*    Written by P N Daly, JAC, 30th April 1993
*
      IMPLICIT NONE
      INTEGER  NELM               ! Number of elements in array
      REAL     DATA( NELM )       ! A real data array
      BYTE     QUAL( NELM )       ! A byte quality array
      INTEGER  I                  ! A counter
      INTEGER  BAD                ! A bad value ...
      PARAMETER ( BAD = 1 )       !  ... set to one

*    If quality is BAD (1) then zero the data value
      DO I = 1, NELM, 1
         IF ( QUAL( I ) .EQ. BAD ) DATA( I ) = 0.0
      END DO

      END
