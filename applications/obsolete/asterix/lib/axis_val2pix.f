*+  AXIS_VAL2PIX - Return pixel values for range pair
      SUBROUTINE AXIS_VAL2PIX( DIM, AXVAL, INC_EXC, RLO, RHI, PLO,
     :                                               PHI, STATUS )
*
*    Description :
*
*     Translates axis values ( usually supplied by a ranges specification )
*     into pixel numbers. When INC_EXC is true, then the ASTERIX rules
*     regarding binning range specificaiotns are enforced, ie. first range
*     value is inclusive, the second exclusive, regardless of the direction
*     of increase of the axis. If the second range value corresponds to the
*     last axis value, then the pixel number is required. This exception is
*     required to permit selection of the entire axis range.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      4 Jul 90 : Original (DJA)
*     19 Nov 92 : Added INC_EXC parameter (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER DIM                          ! Size of dimension
      REAL    AXVAL(DIM)                   ! The axis values
      LOGICAL INC_EXC                      ! Apply binning bounds rules
      REAL    RLO, RHI                     ! Bounds in axis value
*
*    Export :
*
      INTEGER PLO, PHI                     ! Bounds in pixel numbers
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL    DIR,BIN
      INTEGER I
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*      which direction are values increasing
        IF ( AXVAL(DIM) .GT. AXVAL(1) ) THEN
          DIR = 1.0
        ELSE
          DIR = -1.0
        END IF

*      low bound
        I = 1
        DO WHILE ((RLO-AXVAL(I))*DIR.GT.0.0.AND.I.LE.DIM)
          I=I+1
        END DO
        PLO = I

*      upper bound
        DO WHILE ((RHI-AXVAL(I))*DIR.GT.0.0.AND.I.LE.DIM)
          I=I+1
        END DO
        IF ( INC_EXC ) THEN
          PHI = MAX(PLO,I-2)
        ELSE
          PHI = MAX(PLO,I-1)
        END IF

*      test for range = upper bound. Must account for rounding errors,
*      so allow range to be within 10^-3 of the upper bound
        IF ( DIM .GT. 1 ) THEN
          BIN = AXVAL(DIM) - AXVAL(DIM-1)
          IF ( ABS((RHI-AXVAL(DIM))/BIN) .LE. 1.0E-3 ) PHI = DIM
        END IF

      END IF

      END
