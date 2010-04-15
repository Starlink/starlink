      SUBROUTINE POIND0( NSMP, NLIN, DATA, LINNO, BG, ED, STATUS )
*+
*  Name:
*     POIND0

*  Purpose:
*     Find the begin and end of non-zero samples in a line of 2-D array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POIND0( NSMP, NLIN, DATA, LINNO, BG, ED, STATUS )

*  Description:
*     This routine find the begin and end index of the non-zero samples
*     in a line of a 2-D array. A sample in the data array with its
*     value less than VAL__SMLR (defined in PRM_PAR ) is regarded as
*     zero sample. If the line of the array contains only zero value,
*     the returned arguments BG and ED will both have value -1.

*  Arguments:
*     NSMP = INTEGER (Given)
*        The number of samples in a line of the data array.
*     NLIN = INTEGER (Given)
*        The number of lines in the data array.
*     DATA( NSMP, NLIN ) = REAL (Given)
*        Input data array.
*     LINNO = INTEGER (Given)
*        The line number whose begin and end non-zero positon to be
*        found.
*     BG = INTEGER (Returned)
*        The begin index of non-zero samples.
*     ED = INTEGER (Returned)
*        The end index of non-zero samples.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     24-MAR-1991 (WG):
*        Original version. (Base on INTERIM version of FSTLST by DSB )
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
      INCLUDE 'PRM_PAR'          ! Magic value definition

*  Arguments Given:
      INTEGER NSMP
      INTEGER NLIN
      REAL DATA( NSMP, NLIN )
      INTEGER LINNO

*  Arguments Returned:
      INTEGER BG
      INTEGER ED

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      LOGICAL FOUND              ! flag of finding the begin or end

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the begin and end positions of the non-zero samples.
      BG = -1
      ED = -1

*  Enter a do loop until the begin of non-zero sample is found.
      I = 1
      FOUND = .FALSE.
      DO WHILE ( .NOT.FOUND .AND. I .LE. NSMP )

*  Consider only those valid samples.
         IF ( DATA( I, LINNO ) .NE. VAL__BADR ) THEN

*  If absolute value of this sample is larger than smallest real value,
*  it is the first non-zero sample. Note down its index and set the
*  found flag.
            IF ( ABS( DATA( I, LINNO ) ) .GT. VAL__SMLR ) THEN
               BG = I
               FOUND = .TRUE.
            END IF
         END IF

*  Search for next sample, if begin has not found yet.
         I = I + 1
      END DO

*  Enter a do loop search for last non-zero sample.
      I = NSMP
      FOUND = .FALSE.
      DO WHILE ( .NOT.FOUND .AND. I .GE. 1 )

*  Consider only those valid samples.
         IF ( DATA( I, LINNO ) .NE. VAL__BADR ) THEN

*  If absolute value of this sample is larger than smallest real value,
*  it is the last non-zero sample. Note down its index and set the
*  found flag.
            IF ( ABS( DATA( I, LINNO ) ) .GT. VAL__SMLR ) THEN
               ED = I
               FOUND = .TRUE.
            END IF
         END IF

*  Search for next sample, if end has not found yet.
         I = I - 1
      END DO

*  If no non-zero sample are found, set status and report.
      IF ( BG .EQ. -1 .OR. ED .EQ. -1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POIND0_ERR1', 'POIND0: The given profile does '/
     :                /'not contain any non-zero sample.', STATUS )
      END IF

      END
