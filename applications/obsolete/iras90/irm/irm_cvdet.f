      SUBROUTINE IRM_CVDET( NDET, ADET, NDETS1, DETS1, DETS2, NDETS2,
     :                      STATUS )
*+
*  Name:
*     IRM_CVDET

*  Purpose:
*     Find the detectors not included in a supplied list of detectors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_CVDET( NDET, ADET, NDETS1, DETS1, DETS2, NDETS2, STATUS )

*  Description:
*     Argument ADET is supplied holding a list of available detector
*     numbers. The argument DETS1 is supplied holding a subset of the
*     available detector numbers.  DETS2 is returned holding all the
*     detectors in ADET which are not also in DETS1. NDETS2 returns the
*     number of such detectors. Any detector numbers supplied in DETS1
*     which are not also in ADET are ignored. Any numbers which are not
*     legal detector numbers are also ignored.

*  Arguments:
*     NDET = INTEGER (Given)
*        The size of the ADET and DETS2 arrays.
*     ADET( NDET ) = INTEGER (Given)
*        The list of all available detector numbers.
*     NDETS1 = INTEGER (Given)
*        The number of detectors listed in the DETS1 array.
*     DETS1( * ) = INTEGER (Given)
*        A list of detector numbers selected from those contained
*        in ADET.
*     DETS2( NDET ) = INTEGER (Returned)
*        A list of the detector numbers which are included in ADET, but
*        not in DETS1.
*     NDETS2 = INTEGER (Returned)
*        The number of detector numbers stored in DETS2. The detector
*        numbers are stored in elements 1 to NDETS2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-1991 (DSB):
*        Original version.
*     23-JAN-1992 (DSB):
*        Declaration of DETS1 changed from DETS1( NDETS1 ) to DETS1( * )
*        to avoid crash if NDETS1 = 0.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS satellite and mission data.

*  Arguments Given:
      INTEGER NDET
      INTEGER ADET( NDET )
      INTEGER NDETS1
      INTEGER DETS1( * )

*  Arguments Returned:
      INTEGER DETS2( NDET )
      INTEGER NDETS2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL AVAIL              ! True if a detector number is included
                                 ! in the list of available detectors.
      INTEGER DET                ! The next detector number.
      INTEGER I                  ! Loop count.
      LOGICAL IN                 ! True if the detector number is
                                 ! included in the list held in DETS1.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of returned detector numbers to zero.
      NDETS2 = 0

*  Loop round each IRAS detector number.
      DO DET = 1, I90__DETS

*  See if this detector is included in the list of available detectors.
         AVAIL = .FALSE.
         DO I = 1, NDET
            IF( ADET( I ) .EQ. DET ) THEN
               AVAIL = .TRUE.
               GO TO 10
            END IF
         END DO

  10     CONTINUE

*  If the detector is available, see if it is included in the list
*  held in DETS1.
         IF( AVAIL ) THEN

            IN = .FALSE.
            DO I = 1, NDETS1
               IF( DETS1( I ) .EQ. DET ) THEN
                  IN = .TRUE.
                  GO TO 20
               END IF
            END DO

  20        CONTINUE

*  If the detector is not included in DETS1, add it to DETS2.
            IF( .NOT. IN ) THEN
               NDETS2 = NDETS2 + 1
               DETS2( NDETS2 ) = DET
            END IF

         END IF

      END DO

      END
