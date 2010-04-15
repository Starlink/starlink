      SUBROUTINE DGCRA4( LOCS, QNAME, NSMP, NDET, INDAT, CLDAT,
     :                   NDIM, GLHWID, LIST, STATUS )
*+
*  Name:
*     DGCRA4

*  Purpose:
*     Assign samples in glitches given quality name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DGCRA4( LOCS, QNAME, NSMP, NDET, INDAT, CLDAT,
*                  NDIM, GLHWID, LIST, STATUS )

*  Description:
*     The cleaned version of input data array are compared with the
*     original input data array. Bad structures in the cleaned array
*     which have width less than GLHWID are regarded as glitches. The
*     samples in these detected glitches will be assign a given quality
*     name.

*  Arguments:
*     LOCS( 5 ) = CHARACTER (Given)
*        Locators to the quality information.
*     QNAME = CHARACTER (Given)
*        Name of the quality assigned to smaples in glitches.
*     NSMP, NDET = INTEGER (Given)
*        Number of samples and detectors in the input CRDD array.
*     INDAT( NSMP, NDET ) = REAL (Given)
*        The input data array.
*     CLDAT( NSMP, NDET ) = REAL (Given)
*        The cleaned version of input data array.
*     NDIM = INTEGER (Given)
*        The number of dimension to define samples in CRDD files.
*        It must be 2.
*     GLHWID = INTEGER (Given)
*        The width of glitches in sample numbers.
*     LIST( NDIM, GLHWID ) = INTEGER (Returned)
*        The working array used by the routine to note the coordinates
*        of the samples in the glitches.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     28-MAY-1993 (WG):
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
      CHARACTER*( * ) LOCS( 5 )
      CHARACTER*( * ) QNAME
      INTEGER NSMP, NDET
      REAL INDAT( NSMP, NDET ), CLDAT( NSMP, NDET )
      INTEGER NDIM, GLHWID
      INTEGER LIST( NDIM, GLHWID )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J, K, L         ! Do loop indices
      INTEGER SET                ! The number of quality named samples
      LOGICAL FOUND              ! Found end of structure flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note that L is used as an index before it is set!
      L = 0

*  Processing the data in the input array row by row.
      DO I = 1, NDET

*  Looking for samples which are valid but are set as bad in its cleaned
*  version. This is the beginning of a bad structure.
         J = 1
         DO WHILE ( J .LE. NSMP )
            IF ( CLDAT( J, I ) .EQ. VAL__BADR .AND.
     :           INDAT( J, I ) .NE. VAL__BADR ) THEN

*  See how long this bad structure will last: find the first valid
*  sample from the sample in the cleaned data array or the first invalid
*  sample in the uncleaned data array.
               FOUND = .FALSE.
               K = J
               DO WHILE( .NOT.FOUND .AND. K .LT. NSMP )
                  K = K + 1
                  IF ( CLDAT( K, I ) .NE. VAL__BADR .OR.
     :                 INDAT( K, I ) .EQ. VAL__BADR ) FOUND = .TRUE.
               END DO

*  If the length of the structure is less than the given glitch width,
*  this structure is a glitch, Assign the quality name to the samples in
*  this glitch.
               IF ( K - J .LE. GLHWID ) THEN

*  List the indices of the samples in the input data array.
                  IF ( K .EQ. J ) THEN
                     LIST( 1, 1 ) = J
                     LIST( 2, L ) = I
                     CALL IRQ_SETQL( LOCS, .TRUE., QNAME, 2, 1, LIST,
     :                               SET, STATUS )
                  ELSE
                     DO L = 1, K - J
                        LIST( 1, L ) = L + J - 1
                        LIST( 2, L ) = I
                     END DO
                     CALL IRQ_SETQL( LOCS, .TRUE., QNAME, 2, K - J,
     :                               LIST, SET, STATUS )
                  END IF
               END IF

*  Carry on the search from the end of this structure.
               IF ( K .EQ. J ) THEN
                  J = K + 1
               ELSE
                  J = K
               END IF

*  If this is not a beginning of a bad structure, see next sample.
            ELSE
               J = J + 1
            END IF
         END DO
      END DO

      END
