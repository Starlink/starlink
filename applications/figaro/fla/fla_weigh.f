      SUBROUTINE FLA_WEIGH( DIM1, DIM2, ARRAY, WEIGHT, WORK, NFIBRE,
     :                      STATUS )
*+
*  Name:
*     FLA_WEIGH

*  Purpose:
*     Forms the weight array for optimal extractions of FLAIR data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FLA_WEIGH( DIM1, DIM2, ARRAY, WEIGHT, WORK, NFIBRE, STATUS )

*  Description:
*     This routine compresses a 2-dimensional array along the y axis,
*     normalises the compressed array by its mean value, and then finds
*     the minima in the values and set these to the bad value.  Thus it
*     provides the weights for an optimal extraction.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the data array and length of the weight
*        array.
*     DIM2 = INTEGER (Given)
*        The second dimension of the data array.
*     ARRAY( DIM1, DIM2 ) = REAL (Given)
*        The data array.  This should be the co-added arc or sky
*        flat-field frames, so that the compressed array gives the
*        instrumental response of the detector system.
*     WEIGHT( DIM1 ) = REAL (Returned)
*        The weights to be used during an optimal extraction.
*     WORK( DIM1 ) = REAL (Returned)
*        A work array used to calculate the normalised weights.
*     NFIBRE = INTEGER (Returned)
*        The number of fibres found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 April 6 (MJC):
*        Original version.
*     1993 April 25 (MJC):
*        Added NFIBRE argument.
*     1998 October 15 (MJC):
*        Used softlinks for include files.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER DIM1
      INTEGER DIM2
      REAL ARRAY( DIM1, DIM2 )

*  Arguments Returned:
      REAL WEIGHT( DIM1 )
      REAL WORK( DIM1 )
      INTEGER NFIBRE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER START              ! Start of fibre column
      REAL SUM                   ! Summation counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the weights and counts of good values.
      DO I = 1, DIM1
         WEIGHT( I ) = 0.0
         WORK( I ) = 0.0
      END DO

*  Compress the input array.
      DO J = 1, DIM2
         DO I = 1, DIM1
            IF ( ARRAY( I, J ) .NE. VAL__BADR ) THEN
               WORK( I ) = WORK( I ) + 1.0
               WEIGHT( I ) = WEIGHT( I ) + ARRAY( I, J )
            END IF
         END DO
      END DO

*  Normalise the weights by the number of valid elements.
      DO I = 1, DIM1
         IF ( WORK( I ) .GT. 0.1 ) THEN
            WEIGHT( I ) = WEIGHT( I ) / WORK( I )
         ELSE
            WEIGHT( I ) = 0.0
         END IF
      END DO

*  Find the differences in the weights.
      DO I = 2, DIM1
         WORK( I ) = WEIGHT( I ) - WEIGHT( I - 1 )
      END DO

*  Find and count the minima in between the fibres.
      NFIBRE = 0
      START = 2
      DO I = 2, DIM1 - 1
         IF ( WORK( I ) .LT. 0.0 .AND. WORK( I + 1 ) .GT. 0.0 ) THEN
            WEIGHT( I ) = VAL__BADR
            NFIBRE = NFIBRE + 1

*  Normalise the weights to a sum of one for each fibre.  Allow for the
*  unlikely (impossible?) event of a zero sum.
            SUM = 0.0
            DO J = START, I - 1
               SUM = SUM + WEIGHT( J )
            END DO
            IF ( ABS( SUM ) .LT. VAL__SMLR ) THEN
               DO J = START, I - 1
                  WEIGHT( J ) = 0.0
               END DO
            ELSE
               DO J = START, I - 1
                  WEIGHT( J ) = WEIGHT( J ) / SUM
               END DO
            END IF

*  Shift to the start of the next fibre.
            START = I + 1
         END IF
      END DO

*  The above code usually does not normalise the last fibre, as the
*  upper bound of the fibre is not defined.  Therefore, normalise
*  the last fibre.
      IF ( START .LT. DIM1 - 1 .AND.
     :     WEIGHT( START ) .NE. VAL__BADR ) THEN

*  Count the final fibre.
         NFIBRE = NFIBRE + 1

*  Normalise the weights to a sum of one for each fibre.  Allow for the
*  unlikely (impossible?) event of a zero sum.
         SUM = 0.0
         DO J = START, DIM1 - 1
            SUM = SUM + WEIGHT( J )
         END DO
         IF ( ABS( SUM ) .LT. VAL__SMLR ) THEN
            DO J = START, DIM1 - 1
               WEIGHT( J ) = 0.0
            END DO
         ELSE
            DO J = START, DIM1 - 1
               WEIGHT( J ) = WEIGHT( J ) / SUM
            END DO
         END IF
      END IF

*  By definition the first and START weights are bad.
      WEIGHT( 1 ) = VAL__BADR
      WEIGHT( DIM1 ) = VAL__BADR

*  Normalise the weights.
      END
