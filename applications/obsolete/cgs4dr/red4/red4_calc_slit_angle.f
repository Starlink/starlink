*+  RED4_CALC_SLIT_ANGLE - Calculate the CGS4 slit angle
      SUBROUTINE RED4_CALC_SLIT_ANGLE( STATUS )
*    Description :
*     This routine uses the information extracted from EMLT
*     to calculate the CGS4 slit angle.
*    Invocation :
*     CALL RED4_CALC_SLIT_ANGLE( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     19-Jan-1995: Original Unix version.                          (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'RED4_ENG.INC'
*    Status :
      INTEGER STATUS ! Global status
*    Local variables :
      DOUBLE PRECISION
     :  XDIFF,       ! X centroid difference
     :  YDIFF,       ! Y centroid difference
     :  DANGLE       ! Double precision slit angle
*-

*    Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that EMLT.LIS has been read an even number of times
      IF ( NMEAS .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_CALC_SLIT_ANGLE: READ_EMLT has not been used an even number of times', STATUS )
      ENDIF

*    Get the requested YCEN
      CALL PAR_GET0R( 'YCEN', YCEN, STATUS )
      CALL PAR_GET0R( 'YCEN_P', YCEN_P, STATUS )

*    Calculate the centroid differences, converting to double
*    precision, as the calculations below may involve very
*    small angles (being optimistic!).
      XDIFF = DBLE( XCEN ) - DBLE( XCEN_P )
      YDIFF = DBLE( YCEN ) - DBLE( YCEN_P )

*    Check the Y difference is not zeros
      IF ( ABS( YDIFF ) .GT. 1.0D-20 ) THEN

*       Determine the slit angle in degrees
         DANGLE = ATAN2D( XDIFF, YDIFF )

*         Ensure this is in the range -180.0 to +180.0
*         (This step ensures that the angle calculated
*         lies within a single rotation period).
            IF ( DANGLE .LT. -180.0D0 ) THEN
               DANGLE = DANGLE + 360.0D0
            ELSE IF ( DANGLE .GT. 180.0D0 ) THEN
               DANGLE = DANGLE - 360.0D0
            ENDIF

*         Now reset the slit angle to the range -90.0 to +90.0
*         (This step makes a slit angle of 180.0 equivalent to a
*         slit angle of 0.0 - the slit lies in the same direction,
*         but its top and bottom are reversed).
            IF ( DANGLE .LT. -90.0D0 ) THEN
               DANGLE = DANGLE + 180.0D0
            ELSE IF ( DANGLE .GT. 90.0D0 ) THEN
               DANGLE = DANGLE - 180.0D0
            ENDIF

*         Convert the slit angle to single precision.
            SLIT_ANGLE = SNGL( DANGLE )
      ELSE

*      The slit angle cannot be calculated. Issue a warning
*      and arbitrarily set it to 360.
         CALL MSG_OUT( ' ', 'WARNING : Slit angle could not '/
     :     /'be calculated. Y centroids are the same', STATUS )
         SLIT_ANGLE = 360.0
      ENDIF

*   If this has worked, there will now be a valid slit angle.
      IF ( STATUS .EQ. SAI__OK ) THEN
         SLIT_ANGLE_VALID = .TRUE.
      ELSE
         SLIT_ANGLE_VALID = .FALSE.
      END IF

      END
