      SUBROUTINE TRACB3( BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
     :                   NDISP, OFFSET, DTINDX, SCALE, IDC,
     :                   X, Y, NERTRC, NERSMP, FOUND, STATUS )
*+
*  Name:
*     TRACB3

*  Purpose:
*     Get the trace and sample nearest to a given point in the display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACB3( BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
*                  NDISP, OFFSET, DTINDX, SCALE, IDC,
*                  X, Y, NERTRC, NERSMP, FOUND, STATUS )

*  Description:
*     The routine finds the trace and the sample of an NCAR (AUTOGRAPH)
*     display which is vertically nearest to a given point and retunes
*     the trace index and sample index. If such trace and sample are
*     not found, a logical flag will returned.
*
*     The curves in the display are scaled to the required units and
*     have an offset added to vertically displace them.
*
*     The calling routine should make sure that the given point be in
*     the NCAR (AUTOGRAPH) grid window.

*  Arguments:
*     BSMP = INTEGER (Given)
*        Begin sample index of the input CRDD array.
*     ESMP = INTEGER (Given)
*        End sample index of the input CRDD array.
*     BDET = INTEGER (Given)
*        Begin detector index of the input CRDD array.
*     EDET = INTEGER (Given)
*        End detector index of the input CRDD array.
*     INSCN( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The in-scan distance of each sample of each CRDD data trace.
*     DETDAT( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The unscaled data value of the CRDD data trace.
*     NDISP = INTEGER (Given)
*        Number of the traces in the display.
*     OFFSET( NDISP ) = REAL (Given)
*        The offset of each trace in the display
*     DTINDX( NDISP ) = INTEGER (Given)
*        The detector index of each displayed trace.
*     SCALE( NDISP ) = REAL (Given)
*        The scale factor to produce the scaled data in the display.
*     IDC = INTEGER (Given)
*        The IRC identifier of the CRDD file.
*     X = REAL (Given)
*        The x position of the given point in data coordinate.
*     Y = REAL (Given)
*        The y position of the given point in data coordinate.
*     NERTRC = INTEGER (Returned)
*        The index of the displayed trace which is nearest to the given
*        point in the display.
*     NERSMP = INTEGER (Returned)
*        The index of the sample which is nearest to the given point.
*     FOUND = LOGICAL (Returned)
*        If the nearest trace and sample are found, it is true.
*        Otherwise it is false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-MAR-1991 (WG):
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
      INCLUDE 'DAT_PAR'          ! DAT_ constants.
      INCLUDE 'PRM_PAR'          ! VAL_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      REAL DETDAT( BSMP : ESMP, BDET : EDET )
      INTEGER NDISP
      REAL OFFSET( NDISP )
      INTEGER DTINDX( NDISP )
      REAL SCALE( NDISP )
      INTEGER IDC
      REAL X
      REAL Y

*  Arguments Returned:
      INTEGER NERTRC
      INTEGER NERSMP
      LOGICAL FOUND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      INTEGER INDEX1             ! Detector index
      INTEGER INDEX2             ! Detector index
      INTEGER SMP                ! The nearest integer of SMP2


      REAL DATVAL                ! The data value of a trace at SMP
      REAL MDIST                 ! Minimum value of YDIST
      REAL RINSCN                ! Relative in-scan distance between
                                 ! the first point of the trace and the
                                 ! point
      REAL SMP2                  ! Fraction number of the sample which
                                 ! is RINSCN away from the first sample.
      REAL YDIST                 ! The y distance between a trace and
                                 ! the point.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the minimum distance from the given point to traces.
      MDIST = VAL__MAXR

*  No nearest trace and sample are found initially.
      FOUND = .FALSE.

*  Find a detector which has a sample nearest to the given positon.
      DO  I = 1, NDISP

*  Find the in-scan distance from the first sample of the ith trace to
*  the given point.
         RINSCN = INSCN( BSMP, DTINDX( I ) ) - X

*  Convert it to radians
         RINSCN = REAL( IRA__DTOR ) * RINSCN / 60.0

*  Find the sample index of the given point on the ith trace.
         INDEX1 = DTINDX( I )
         INDEX2 = DTINDX( I )
         CALL IRC_OFFST( IDC, REAL( BSMP ), INDEX1, INDEX2,
     :                   RINSCN, SMP2, STATUS )
         SMP = NINT( SMP2 )

*  If the sample found is inside the scan segment of the trace,
         IF ( SMP .LT. ESMP .AND. SMP .GT. BSMP ) THEN

*  And the sample is valid, set the found flag.
            IF ( DETDAT( SMP, DTINDX( I ) ) .NE. VAL__BADR .AND.
     :           SCALE( I ) .NE. VAL__BADR ) THEN
               FOUND = .TRUE.

*  Get the scaled data value at this sample and its distance in the
*  display from the given point.
               DATVAL = SCALE( I ) * DETDAT( SMP, DTINDX( I ) )
               YDIST = ABS( Y - OFFSET( I ) - DATVAL )

*  Note down the trace and sample indices which is nearest to the point.
               IF ( MDIST .GT. YDIST ) THEN
                  MDIST = YDIST
                  NERTRC = I
                  NERSMP = SMP
               END IF
            END IF
         END IF
      END DO

*  If no nearest trace and sample are found, report a message before
*  exit.
      IF ( .NOT.FOUND ) THEN
         CALL MSG_BLANKIF( MSG__QUIET, STATUS )
         CALL MSG_OUTIF( MSG__QUIET, 'TRACB3_MSG1',
     :                   'WARNING: Cannot find the nearest trace',
     :                   STATUS )
         CALL MSG_BLANKIF( MSG__QUIET, STATUS )
      END IF

      END
