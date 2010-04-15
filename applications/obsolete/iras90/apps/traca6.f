      SUBROUTINE TRACA6( IDC, BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
     :                   NDISP, DTINDX, SCALE, XLMT, YMX, YMN, PYLMT,
     :                   YLMT, STATUS )
*+
*  Name:
*     TRACA6

*  Purpose:
*     Get vertical limits of the CRDD trace display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACA6( IDC, BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
*                  NDISP, DTINDX, SCALE, XLMT, YMX, YMN, PYLMT,
*                  YLMT, STATUS )


*  Description:
*     This rouinte get the upper and lower vertical limits in the
*     display of the CRDD traces. The returned limits are in the units
*     specified by the factors SCALE and ZERO. The suggested default
*     lower limit is the smallest value of the bottom trace, and the
*     upper limit is such that no traces ever overlap.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier.
*     BSMP = INTEGER (Given)
*        Begin index of the samples in input CRDD data.
*     ESMP = INTEGER (Given)
*        End index of the samples in input CRDD data.
*     BDET = INTEGER (Given)
*        Begin index of the detectors in input CRDD data.
*     EDET = INTEGER (Given)
*        End index of the detectors in input CRDD data.
*     INSCN( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The in-scan distance from the reference position in arcmins
*        of each detector data.
*     DETDAT( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The input CRDD data.
*     NDISP = INTEGER (Given)
*        The number of detectors to be displayed.
*     DTINDX( NDISP ) = INTEGER (Given)
*        Detector index whose data to be displayed.
*     SCALE( NDISP ) = REAL (Given)
*        The scaling factor for converting the data of each display
*        trace from original NDF file units to specified units.
*     XLMT( 2 ) = REAL (Given)
*        The lower and upper limits of the display in X axis (in-scan)
*        direction in arcmins north of the reference position.
*     YMX( BDET : EDET ) = REAL (Given)
*        The max. data value of each detector trace.
*     YMN( BDET : EDET ) = REAL (Given)
*        The min. data value of each detector trace.
*     PYLMT = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the values for lower and
*        upper vertical limits. If a blank is supplied, then no values
*        are obtained from the environment. Instead, the default values
*        are used.
*     YLMT( 2 ) = REAL (Returned)
*        The lower and upper vertical limits.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-APR-1991 (WG):
*        Original version.
*     8-DEC-1993 (DSB):
*        Guard against bad SCALE values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ package constants

*  Arguments Given:
      INTEGER IDC
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      REAL DETDAT( BSMP : ESMP, BDET : EDET )
      INTEGER NDISP
      INTEGER DTINDX( NDISP )
      REAL SCALE( NDISP )
      REAL XLMT( 2 )
      REAL YMX( BDET : EDET )
      REAL YMN( BDET : EDET )
      CHARACTER PYLMT*(*)

*  Arguments Returned:
      REAL YLMT( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      INTEGER INDEX1             ! Detector indices.
      INTEGER INDEX2             ! Detector indices.
      INTEGER J                  ! Do loop index
      INTEGER SMPHI              ! Upper sample index limit of display
      INTEGER SMPLO              ! Lower sample index limit of display

      LOGICAL FOUND              ! Flag for finding min at bottom trace

      REAL DATVAL                ! Data value.
      REAL DFYLMT( 2 )           ! Default lower and upper y limits
      REAL DIST1                 ! Distance of lower x limit to the
                                 ! first sample
      REAL DIST2                 ! Distance of upper x limit to the
                                 ! first sample
      REAL SMP1                  ! Fractional sample number
                                 ! corresponding to lower x display
                                 ! limit
      REAL SMP2                  ! Fractional sample number
                                 ! corresponding to upper x display
                                 ! limit
      REAL SPAN                  ! Max. trace span in the display
      REAL YMAX                  ! Max. value in the default display
      REAL YMIN                  ! Min. value of the bottom trace
      REAL YTEMP                 ! A temporary buffer.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      YMIN = 0.0

*  Find the sample numbers at the bottom trace corresponding to lower
*  and upper in-scan display limits.
      FOUND = .FALSE.
      J = 1

      DO WHILE ( .NOT.FOUND .AND. J .LE. NDISP .AND.
     :           STATUS .EQ. SAI__OK )

*  If this detector is dead, pass on.
         IF( SCALE( J ) .NE. VAL__BADR ) THEN

            DIST1 = INSCN( BSMP, DTINDX( J ) ) - XLMT( 1 )
            DIST2 = INSCN( BSMP, DTINDX( J ) ) - XLMT( 2 )

*  Convert them to radians.
            DIST1 = REAL( IRA__DTOR ) * DIST1 / 60.0
            DIST2 = REAL( IRA__DTOR ) * DIST2 / 60.0

            INDEX1 = DTINDX( J )
            INDEX2 = DTINDX( J )

            CALL IRC_OFFST( IDC, REAL( BSMP ), INDEX1, INDEX2,
     :                      DIST1, SMP1, STATUS )

            CALL IRC_OFFST( IDC, REAL( BSMP ), INDEX1, INDEX2,
     :                      DIST2, SMP2, STATUS )

            IF ( SMP2 .GT. SMP1 ) THEN
               SMPLO = NINT( SMP1 )
               SMPHI = NINT( SMP2 )
            ELSE
               SMPHI = NINT( SMP1 )
               SMPLO = NINT( SMP2 )
            END IF

*  Check that X axis is of sufficient extent to display.
            IF ( SMPHI - SMPLO .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'TRACA6_ERR1',
     :                       'TRACA6: Extent of X axis is too small.',
     :                       STATUS )
               GOTO 999
            END IF

*  Find the min value of the bottom trace within the display.
*  The search is confined within the scan segment of the trace.
            YMIN = VAL__MAXR
            DO I = MAX( SMPLO, BSMP ), MIN( SMPHI, ESMP )

*  Consider only valid data.
               DATVAL = DETDAT( I, DTINDX( J ) )
               IF (  DATVAL .NE. VAL__BADR  ) THEN
                  IF ( YMIN .GT. DATVAL ) THEN
                      YMIN = DATVAL
                      FOUND = .TRUE.
                  END IF
               END IF

            END DO

*  If the min. value exits, scale it.
            IF ( FOUND ) THEN
               YMIN = SCALE( J ) * YMIN

*  If no min. value found, this trace contains no valid data in the
*  displayed segment, and will not be displayed. The next trace will be
*  at the bottom of the display, so go back consider next trace.
            ELSE
               J = J + 1
            END IF

         END IF

      END DO

*  If no min. value found for all traces in the specified segment, the
*  x limit is improperly specified. Set Status, report error and exit.
      IF ( .NOT.FOUND .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACA6_ERR2',
     :                 'TRACA6: No valid data to display',
     :                  STATUS )
         GOTO 999
      END IF

*  Sum up the span of each trace (ignore dead detectors).
      SPAN = 0.0
      DO I = 1, NDISP
         IF( SCALE( I ) .NE. VAL__BADR ) THEN
            SPAN = SPAN + SCALE( I ) *
     :             ( YMX( DTINDX( I ) ) - YMN( DTINDX( I ) ) )
         END IF
      END DO

*  Set max. y display such that no trace ever overlap.
      YMAX = SPAN + YMIN

*  Set default lower and upper limits. Considering the round-off error
*  to make sure that they are within the given range.
      DFYLMT( 1 ) = YMIN
      DFYLMT( 2 ) = YMAX

*  If a parameter was supplied, get the vertical limits from the
*  environment.
      IF( PYLMT .NE. ' ' ) THEN
         CALL PAR_GDR1R( PYLMT, 2, DFYLMT, VAL__MINR, VAL__MAXR,
     :                   .FALSE., YLMT, STATUS )

*  Otherwise, just use the default values.
      ELSE
         YLMT( 1 ) = DFYLMT( 1 )
         YLMT( 2 ) = DFYLMT( 2 )

      END IF

*  If the given limits is in wrong order, swap them.
      IF ( YLMT( 2 ) .LT. YLMT( 1 ) ) THEN
         YTEMP = YLMT( 2 )
         YLMT( 2 ) = YLMT( 1 )
         YLMT( 1 ) = YTEMP
      END IF

*  Check that Y axis is of sufficient extent to display.
      IF ( YLMT( 2 ) - YLMT( 1 ) .LT. VAL__SMLR .AND.
     :     STATUS .EQ. SAI__OK) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACA6_ERR3',
     :                'TRACA6: Extent of Y axis is too small.',
     :                 STATUS )
      END IF

 999  CONTINUE

      END
