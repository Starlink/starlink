      SUBROUTINE DGCRA2( NSMP, NDET, INDAT, CLDAT, GLHWID, NBAD, OUTDAT,
     :                   STATUS )
*+
*  Name:
*     DGCRA2

*  Purpose:
*     Set samples in glitches as BAD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DGCRA2( NSMP, NDET, INDAT, CLDAT, GLHWID, NBAD, OUTDAT,
*                  STATUS )

*  Description:
*     The cleaned version of input data array are compared with the
*     original input data array. Bad structures in the cleaned array
*     which have width less than GLHWID are regarded as gliches. The
*     input array is copied to the output array with the samples in the
*     detected glitches set as BAD.

*  Arguments:
*     NSMP, NDET = INTEGER (Given)
*        The number of samples and detectors in the input data array.
*     INDAT( NSMP, NDET ) = REAL (Given)
*        The input data array.
*     CLDAT( NSMP, NDET ) = REAL (Given)
*        The cleaned version of input data array.
*     GLHWID = INTEGER (Given)
*        The width less than which a bad structure in the cleaned array
*        will be regarded as a glitch.
*     NBAD = INTEGER (Returned)
*        The number of samples being set as bad in the output array by
*        this subroutine.
*     OUTDAT( NSMP, NDET ) = REAL (Returned)
*        The output data array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     27-MAY-1993 (WG):
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
      INTEGER NSMP, NDET
      REAL INDAT( NSMP, NDET )
      REAL CLDAT( NSMP, NDET )
      INTEGER GLHWID

*  Arguments Returned:
      INTEGER NBAD
      REAL OUTDAT( NSMP, NDET )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J, K, L         ! Do loop index
      LOGICAL FOUND              ! Found next valid sample flag
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initially no sample in the output array has set as BAD.
      NBAD = 0

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
*  this structure is a glich, Set the output samples in the structure
*  as BAD.
               IF ( K - J .LE. GLHWID ) THEN
                  IF ( K .EQ. J ) THEN
                     OUTDAT( K, I ) = VAL__BADR
                     NBAD = NBAD + 1
                  ELSE
                     DO L = J, K - 1
                        OUTDAT( L, I ) = VAL__BADR
                        NBAD = NBAD + 1
                     END DO
                  END IF

*  If the length of the structure is greater than the given glitch
*  width, this structure is not a glich. Copy the input data to the
*  output.
               ELSE
                  DO L = J, K - 1
                     OUTDAT( L, I ) = INDAT( L, I )
                  END DO
               END IF

*  Carry on the search from the end of this structure.
               IF ( K .EQ. J ) THEN
                  J = K + 1
               ELSE
                  J = K
               END IF

*  For samples which are valid in the cleaned data array or are invalid
*  in both cleaned and uncleaned data array, just pass on to the output
*  data array.
            ELSE
               OUTDAT( J, I ) = INDAT( J, I )

*  See next sample.
               J = J + 1
            END IF
         END DO
      END DO

      END
