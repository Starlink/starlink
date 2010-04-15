      SUBROUTINE DGCRA0( NPSMP, NPROF, PROF, PROFX, PRFWID, STATUS )
*+
*  Name:
*     DGCRA0

*  Purpose:
*     Find width of point source profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DGCRA0( NPSMP, NPROF, PROF, PROFX, PRFWID, STATUS )

*  Description:
*     This subroutine finds out the width of the point source profile.
*     The width of a point source profile is defined as the width of
*     profile at 0.2 of its max. value. The returned width is in
*     arcmin.

*  Arguments:
*     NPSMP = INTEGER (Given)
*        Number of samples in the point source profiles.
*     NPROF = INTEGER (Given)
*        Number of point source profiles.
*     PROF( NPSMP, NPROF ) = REAL (Given)
*        Point Source profiles.
*     PROFX( NPSMP ) = REAL (Given)
*        The in-scan axis of the point source profiles.
*     PRFWID( NPROF ) = REAL (Given)
*        The width of the point source profiles, in arcmin.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     24-MAY-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      INTEGER NPSMP, NPROF
      REAL PROF( NPSMP, NPROF )
      REAL PROFX( NPSMP )

*  Arguments Returned:
      REAL PRFWID( NPROF )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J               ! Do loop indices
      INTEGER FSTSMP, LSTSMP     ! 1st & last samples whose value is
                                 ! half of the max
      LOGICAL FIND               ! Find half value sample flag
      REAL MAXVAL                ! Max value of a profile

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Go for all profiles.
      DO I = 1, NPROF

*  Find the max. of this profile.
         MAXVAL = VAL__MINR
         DO J = 1, NPSMP
            IF ( PROF( J, I ) .NE. VAL__BADR )
     :         MAXVAL = MAX( MAXVAL, PROF( J, I ) )
         END DO

*  Search from the begin of the profile to find a sample which has the
*  value of half max.
         FSTSMP = NPSMP
         FIND = .FALSE.
         J = 2
         DO WHILE ( .NOT.FIND .AND. J .LE. NPSMP )
            IF ( PROF( J, I ) .NE. VAL__BADR .AND.
     :           PROF( J, I ) .GE. 0.2 * MAXVAL ) THEN
                FSTSMP = J - 1
                FIND = .TRUE.
            END IF
            J = J + 1
         END DO

*  Search from the end of the profile to find a sample which has the
*  value of half max.
         LSTSMP = 1
         FIND = .FALSE.
         J = NPSMP - 1
         DO WHILE ( .NOT.FIND .AND. J .GE. 1 )
            IF ( PROF( J, I ) .NE. VAL__BADR .AND.
     :           PROF( J, I ) .GE. 0.2 * MAXVAL ) THEN
               LSTSMP = J + 1
               FIND = .TRUE.
            END IF
            J = J - 1
         END DO

*  If no such samples are found, the point source profiles are
*  improperly supplied, set status report and exit.
         IF ( LSTSMP .LE. FSTSMP ) THEN
            STATUS = SAI__ERROR
            CALL MSG_BLANK( STATUS )
            CALL ERR_REP( 'DCRDA0_ERR1', 'DCRDA0:  Something wrong '/
     :                   /'with supplied point source profiles',
     :                     STATUS )
            GOTO 999
         END IF

*  Find the arcmin distance between this two samples.
         PRFWID( I ) = PROFX( LSTSMP ) - PROFX( FSTSMP )

      END DO

 999  CONTINUE

      END
