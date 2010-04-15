      SUBROUTINE POIND1( NPSMP, NPROF, DATA, AXIS, BAND, PRFWID,
     :                   PRFSEG, STATUS )
*+
*  Name:
*     POIND1

*  Purpose:
*     Re-sample the specified profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POIND1( NPSMP, NPROF, DATA, AXIS, BAND, PRFWID, PRFSEG,
*                  STATUS )

*  Description:
*     This subroutine re-samples a segment of the point source profile
*     associated with the given waveband in the same rate as the IRAS
*     scans. The segment to be re-sampled is centred at the profile
*     centre, which has the zero X axis value, and has the length of
*     PRFWID.

*  Arguments:
*     NPSMP = INTEGER (Given)
*        Number of samples in the input ideal point source profile data
*        array.
*     NPROF = INTEGER (Given)
*        Number of profile in the input ideal point source profile data
*        array.
*     DATA( NPSMP, NPROF ) = REAL (Given)
*        The input ideal point source profile data array.
*     AXIS( NPSMP ) = REAL (Given)
*        The in-scan axis of the point source profile, in arc-min from
*        its centre.
*     BAND = INTEGER (Given)
*        Waveband number with which associated point source profile is
*        to be resampled.
*     PRFWID = INTEGER (Given)
*        Number of the samples in the resampled non-zero segment of
*        the profile.
*     PRFSEG( PRFWID ) = REAL (Returned)
*        The resampled non-zero segment of the selected profile.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1993 (WG):
*        Original version.
*     6-OCT-1994 (DCP):
*        Incorporated as is in new version of POINTCRDD
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 package constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      INTEGER NPSMP, NPROF
      REAL DATA( NPSMP, NPROF )
      REAL AXIS( NPSMP )
      INTEGER BAND
      INTEGER PRFWID

*  Arguments Returned:
      REAL PRFSEG( PRFWID )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL ARCDIS                ! Arcmin dist. from an resample
                                 ! position to the central position
      REAL DIS1, DIS2            ! Dist. from two ends when interpol.
      LOGICAL FOUND              ! Found sample flag
      INTEGER I, J, K            ! Do loop indices
      INTEGER LINNO              ! Line number of profile array used
      INTEGER LOSMP, UPSMP       ! Just lower and upper samples to a
                                 ! resampled position
      REAL SMPSIZ                ! new sample size in arcmin
      REAL VAL1, VAL2            ! Two ends values in linear interpol.
      REAL VAL                   ! Linear interpolated value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      LINNO = 0
      LOSMP = 0
      UPSMP = 0

*  If the input profile array contains only one profile, use it for all
*  waveband.
      IF ( NPROF .EQ. 1 ) THEN
         LINNO = 1

*  Otherwise, if the profile array contains 4 profiles, use the one
*  corresponding to the given waveband.
      ELSE IF ( NPROF .EQ. 4 ) THEN
         LINNO = BAND
      END IF

*  Calculate the new sample size in arcmin.
      SMPSIZ = I90__SPEED / I90__SRATE( BAND )

*  Find the samples along the profile's X axis which is just above 0
*  arcmin.
      FOUND = .FALSE.
      J = 0
      DO WHILE ( .NOT.FOUND .AND. J .LT. NPSMP  )
         J = J + 1
         IF ( AXIS( J ) .GE. 0.0 ) THEN
            UPSMP = J
            LOSMP = J - 1
            FOUND = .TRUE.
         END IF
      END DO

*  Find the value of the profile at the 0 arc-min position by linear
*  interpolation.
      DIS1 = - AXIS( LOSMP )
      VAL1 = DATA( LOSMP, LINNO )
      DIS2 = AXIS( UPSMP )
      VAL2 = DATA( UPSMP, LINNO )
      CALL IRM_LINR( DIS1, DIS2, VAL1, VAL2, VAL, STATUS )

*  Put this value at the middle of the output array.
      PRFSEG( PRFWID / 2 + 1 ) = VAL

*  Set the inital serach sample for the further re-sampling.
      J = LOSMP
      K = UPSMP

*  Find the re-sampled values for the lower half of the output segment.
      DO I = PRFWID / 2, 1, -1

*  Find the arc-min distance of the next sample to the centre.
         ARCDIS = SMPSIZ * ( I - ( PRFWID / 2 + 1 ) )

*  Search for the first sample in the input profile whose arc-min
*  distance from the centre is just less than ARCDIS.
         FOUND = .FALSE.
         DO WHILE ( .NOT.FOUND .AND. J .GT. 1 )
            J = J - 1
            IF ( AXIS( J ) .LT. ARCDIS ) THEN
               LOSMP = J
               UPSMP = J + 1
               FOUND = .TRUE.
            END IF
         END DO

*  If such sample is found, calculate the profile value at the
*  re-sampled position by linear interpolation.
         IF ( FOUND ) THEN
            DIS1 = ARCDIS - AXIS( LOSMP )
            VAL1 = DATA( LOSMP, LINNO )
            DIS2 = AXIS( UPSMP ) - ARCDIS
            VAL2 = DATA( UPSMP, LINNO )
            CALL IRM_LINR( DIS1, DIS2, VAL1, VAL2, VAL, STATUS )

*  If no such sample is found, it is because the re-sampled position has
*  gone outside the profile, set the value as zeor.
         ELSE
            VAL = 0.0
         END IF

*  Put the value to the output array at the resample position.
         PRFSEG( I ) = VAL
      END DO

*  Find the resampled values in the upper half of the output segment.
      DO I = PRFWID / 2 + 2, PRFWID

*  Find the arc-min distance of the next sample to the centre.
         ARCDIS = SMPSIZ * ( I - ( PRFWID / 2 + 1 ) )

*  Search for the first sample in the input profile whose arc-min
*  distance from the centre is just greater than ARCDIS.
         FOUND = .FALSE.
         DO WHILE ( .NOT.FOUND .AND. K .LT. NPSMP )
            K = K + 1
            IF ( AXIS( K ) .GT. ARCDIS ) THEN
               LOSMP = K - 1
               UPSMP = K
               FOUND = .TRUE.
            END IF
         END DO

*  If such sample is found, calculate the profile value at the
*  re-sampled position by linear interpolation.
         IF ( FOUND ) THEN
            DIS1 = ARCDIS - AXIS( LOSMP )
            VAL1 = DATA( LOSMP, LINNO )
            DIS2 = AXIS( UPSMP ) - ARCDIS
            VAL2 = DATA( UPSMP, LINNO )
            CALL IRM_LINR( DIS1, DIS2, VAL1, VAL2, VAL, STATUS )

*  If no such sample is found, it is because the re-sampled position has
*  gone outside the profile, set the value as zeor.
         ELSE
            VAL = 0.0
         END IF

*  Put the value to the output array at the resample position.
         PRFSEG( I ) = VAL
      END DO

      END
