*+  MATH_INTERP - Wrap up of MATH_CPOLY calls to produce interpolated values
      SUBROUTINE MATH_INTERP( NIN, XIN, YIN, NOUT, XREQ, PREFDEG,
     :                                             YOUT, STATUS )
*    Description :
*    History :
*
*     23 Nov 89 : Original ( BHVAD::DJA )
*
*    Type definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER                  NIN
      REAL                     XIN(NIN), YIN(NIN)
      INTEGER                  NOUT
      REAL                     XREQ(NOUT)              ! Request values
      INTEGER                  PREFDEG                 ! Preferred degree
*
*
*    Export :
*
      REAL                     YOUT(NOUT)
*
*    Local constants :
*
      INTEGER                  MAXDEG
         PARAMETER             (MAXDEG = 7)
*
*    Local variables :
*
      REAL                     AWORK(MAXDEG+2,MAXDEG+2)
      REAL                     COEFFS(MAXDEG+2)
      REAL                     CWORK((MAXDEG*2)+1)
      REAL                     XN(100),XR(100)
      REAL                     XMIN, XMAX, SUMSQ

      INTEGER                  DEGREE,I
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Choose degree of polynomial - (NIN - 2 ) but in range[3,MAXDEG]
      IF ( PREFDEG .EQ. 0 ) THEN
         DEGREE = NIN - 2
         DEGREE = MIN( MAXDEG, DEGREE )
         DEGREE = MAX( 3, DEGREE )
      ELSE
         DEGREE = PREFDEG
      END IF
      DEGREE = MIN( DEGREE, NIN-1 )

*    Fit polynomial
      CALL MATH_POLY( .TRUE., NIN, XIN, .FALSE., 0, DEGREE, AWORK,
     :                 CWORK, YIN, COEFFS, XN, SUMSQ, XMAX, XMIN )

*    Normalise the XREQ
      DO I = 1, NOUT
         XR(I) = (2*XREQ(I)-XMAX-XMIN)/(XMAX-XMIN)
      END DO

*    Evaluate at specified positions
      CALL MATH_EPOLY( NOUT, DEGREE, COEFFS, XR, YOUT, STATUS )

      END
