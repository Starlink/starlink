      SUBROUTINE IRC1_DISTG( IDC, SAMP1, SAMP2, DETIND, DIST, STATUS )
*+
*  Name:
*     IRC1_DISTG

*  Purpose:
*     Find the arc-distance between two given samples without
*     argument verification using a CRDD-type independant method.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_DISTG( IDC, SAMP1, SAMP2, DETIND, DIST, STATUS )

*  Description:
*     This routine provides functionality for routine IRC_DIST, but
*     performs no check on the validity of the input arguments. The
*     method used makes no assumptions about CRDD type. The distance
*     between the two given samples is split into steps. These steps
*     are small enough (5 degrees) to ensure that the "straight line"
*     joining the two ends is less than 1 arc-second shorter than the
*     true in-scan distance. Thus the returned distance is the true
*     in-scan distance, rather than just the distance joining the two
*     end samples "as the crow flies".

*  Arguments:
*     IDC = INTEGER (Given)
*        An IRC identifier for the CRDD file.
*     SAMP1 = REAL (Given)
*        The first fractional sample number.
*     SAMP2 = REAL (Given)
*        The second fractional sample number.
*     DETIND = INTEGER (Given)
*        The detector index to which the sample numbers refer.
*     DIST = REAL (Given)
*        The arc-length of the detector track joining SAMP1 and SAMP2,
*        in radians. Positive if the displacement from SAMP1 to SAMP2
*        is in the same direction as the focal plane Y axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-FEB-1991 (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_BAND( IRC__MAX ) = INTEGER (Read)
*           IRAS band number (NOT wavelength).
*        CCM_NOMSP( IRC__MAX ) = REAL (Read)
*           Nominal scan speed.

*  Arguments Given:
      INTEGER IDC
      REAL SAMP1
      REAL SAMP2
      INTEGER DETIND

*  Arguments Returned:
      REAL  DIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A(20)     ! Sky longitude values at each step.
      DOUBLE PRECISION ANGLE(20) ! Scan angle at each step.
      DOUBLE PRECISION B(20)     ! Sky latitude values at each step.
      DOUBLE PRECISION DELTA     ! Distance between adjacent steps.
      INTEGER DET(20)            ! Detector index at each step.
      LOGICAL FIRST              ! True if currently at the first step.
      INTEGER GRPSIZ             ! No. of steps in current group.
      INTEGER I                  ! Loop counter.
      REAL    LSAMP              ! Sample number at previous step.
      DOUBLE PRECISION LA        ! sky longitude at previous step.
      DOUBLE PRECISION LB        ! sky latitude at previous step.
      INTEGER MEMBER             ! Index of current step within group.
      LOGICAL MORE               ! True if not completed.
      INTEGER NSTEP              ! No. of steps between SAMP1 and SAMP2.
      LOGICAL PROGRP             ! True if group is compete.
      REAL    SAMP(20)           ! Sampole number at each step.
      CHARACTER SCS*(IRA__SZSCS)! Sky Coordinate System in use.
      REAL    SPEED(20)          ! Scan speed at each step.
      INTEGER STEP               ! Current step.
      REAL    STEPSZ             ! No. of samples in a step.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the approximate number of samples in a 5 degree long scan. The
*  difference in length between the scan track and the great circle
*  joining two such samples is always less than an arc-second. The
*  total scan length is broken into 5 degree steps. The arc-lengths
*  of all such steps are added together to get the total length.
      STEPSZ = I90__SRATE( CCM_BAND( IDC ) )*0.087 / CCM_NOMSP( IDC )

*  If the second sample number is less than the first, then the step
*  size is negative, otherwise it is positive.
      IF( SAMP2 .LT. SAMP1 ) THEN
         STEPSZ = -ABS( STEPSZ )
      ELSE
         STEPSZ = ABS( STEPSZ )
      END IF

*  Calculate the number of steps joining the given samples.
      NSTEP = 1 + INT( ABS( ( SAMP2 - SAMP1 ) / REAL( STEPSZ ) ) )

*  Step one (the first member of the first group of steps) is at the
*  first given sample number.
      STEP = 1
      MEMBER = 1
      SAMP( 1 ) = SAMP1
      LSAMP = SAMP1
      DET( 1 ) = DETIND
      FIRST = .TRUE.

*  Steps are handled in groups of 20 (to make efficient use of
*  IRC1_DPOSI). Loop round each group..
      MORE = .TRUE.
      PROGRP = .FALSE.

      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

         MEMBER = MEMBER + 1
         IF( MEMBER .LE. 20 ) THEN

*  If more steps remain to be done, store the sample number and detector
*  index at which the step occurs.
            STEP = STEP + 1
            IF( STEP .LE. NSTEP ) THEN
               LSAMP = LSAMP + STEPSZ
               SAMP( MEMBER ) = LSAMP
               DET( MEMBER ) = DETIND

*  If all steps have been done, add the second given sample number to
*  the list (this is the end of the last step). Indicate that no more
*  steps remain to be done. Indicate that the group can now be
*  processed.
            ELSE
               SAMP( MEMBER ) = SAMP2
               DET( MEMBER ) = DETIND
               MORE = .FALSE.
               GRPSIZ = MEMBER
               PROGRP = .TRUE.
            END IF

*  If the group is full, indicate that the group can now be processed
*  and reset the group member counter for the start of the next group.
         ELSE
            PROGRP = .TRUE.
            GRPSIZ = 20
            MEMBER = 0

         END IF

*  If the group is complete, process it...
         IF( PROGRP .AND. STATUS .EQ. SAI__OK ) THEN

*  Find the sky coordinates at each member sample within the group.
            CALL IRC1_DPOSI(IDC, GRPSIZ, SAMP, DET, SCS, A, B, ANGLE,
     :                SPEED, STATUS )

*  Loop round each member.
            DO I = 1, GRPSIZ

*  If this is not the first step...
               IF( .NOT. FIRST ) THEN

*  Find the distance between this step and the previous step.
                  CALL IRA_DIST( A(I), B(I), LA, LB, DELTA, STATUS )

*  Add this distance onto the total distance.
                  DIST = DIST + REAL( DELTA )

*  If this is the first step, set the total distance to zero.
               ELSE
                  DIST = 0.0
                  FIRST = .FALSE.

               END IF

*  Save the current step position for use next time round.
               LA = A(I)
               LB = B(I)

            END DO

*  Indicate that the next group is not yet complete.
            PROGRP = .FALSE.

         END IF

      END DO

*  Give the final distance the correct sign.
*  Find the sign of the total distance.
      DIST = SIGN( DIST, CCM_NOMSP( IDC )*( SAMP2 - SAMP1 ) )

      END
