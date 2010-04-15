      SUBROUTINE ERRCL( LIMIT, TYPE, EL, DIN, VIN, DOUT, VOUT, BAD,
     :                  NBAD, STATUS )
*+
*  Name:
*     ERRCL

*  Purpose:
*     Reject DATA and VARIANCE values with large errors

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERRCL( LIMIT, TYPE, EL, DIN, VIN, DOUT, VOUT, BAD, NBAD,
*                 STATUS )

*  Description:
*     Produces copies of the input DATA and VARIANCE arrays in which
*     pixels with errors greater than a specified limit are set
*     invalid. The error limit may be specified as the maximum
*     acceptable standard deviation (or variance), or the minimum
*     acceptable signal to noise ratio.

*  Arguments:
*     LIMIT = DOUBLE PRECISION (Given)
*        The error limit.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of limit supplied; SIGMA, VARIANCE or SNR.
*     EL = INTEGER (Given)
*        No. of elements in each array
*     DIN( EL ) = DOUBLE PRECISION (Given)
*        Input DATA array.
*     VIN( EL ) = DOUBLE PRECISION (Given)
*        Input VARIANCE array.
*     DOUT( EL ) = DOUBLE PRECISION (Returned)
*        Output DATA array.
*     VOUT( EL ) = DOUBLE PRECISION (Returned)
*        Output VARIANCE array.
*     BAD = LOGICAL (Returned)
*        True if there are any bad values in the returned arrays.
*     NBAD = INTEGER (Returned)
*        No. of pixels rejected by this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      DOUBLE PRECISION LIMIT
      CHARACTER * ( * ) TYPE
      INTEGER EL
      DOUBLE PRECISION DIN( EL )
      DOUBLE PRECISION VIN( EL )

*  Arguments Returned:
      DOUBLE PRECISION DOUT( EL )
      DOUBLE PRECISION VOUT( EL )
      LOGICAL BAD
      INTEGER NBAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :        DATVAL,            ! Data value
     :        VARVAL             ! Variance value

      INTEGER
     :        I                  ! Element count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned bad pixel flag.
      BAD = .FALSE.

*  Initialise the number of pixel set bad by this routine.
      NBAD = 0

*  First handle cases in which the limit is a standard deviation.
      IF( TYPE .EQ. 'SIGMA' ) THEN

*  Loop round each element of the arrays.
         DO I = 1, EL

*  Get the input values.
            VARVAL = VIN( I )
            DATVAL = DIN( I )

*  If neither are invalid, proceed.
            IF( VARVAL .NE. VAL__BADD .AND. DATVAL .NE. VAL__BADD ) THEN

*  Compare the variance with the limit, and store bad values in the
*  output arrays if it is to high.
               IF( SQRT( MAX( 0.0D0, VARVAL ) ) .GT. LIMIT ) THEN
                  DOUT( I ) = VAL__BADD
                  VOUT( I ) = VAL__BADD
                  BAD = .TRUE.
                  NBAD = NBAD + 1

*  Otherwise, store the input values in the output arrays.
               ELSE
                  DOUT( I ) = DATVAL
                  VOUT( I ) = VARVAL

               END IF

*  If one of the input values were bad, put both bad in the output
*  arrays and set the returned bad pixel flag.
            ELSE
               DOUT( I ) = VAL__BADD
               VOUT( I ) = VAL__BADD
               BAD = .TRUE.
            END IF

         END DO

*  Now handle VARIANCE limits...
      ELSE IF( TYPE .EQ. 'VARIANCE' ) THEN

*  Loop round each element of the arrays.
         DO I = 1, EL

*  Get the input values.
            VARVAL = VIN( I )
            DATVAL = DIN( I )

*  If neither are invalid, proceed.
            IF( VARVAL .NE. VAL__BADD .AND. DATVAL .NE. VAL__BADD ) THEN

*  Compare the variance with the limit, and store bad values in the
*  output arrays if it is to high.
               IF( VARVAL .GT. LIMIT ) THEN
                  DOUT( I ) = VAL__BADD
                  VOUT( I ) = VAL__BADD
                  BAD = .TRUE.
                  NBAD = NBAD + 1

*  Otherwise, store the input values in the output arrays.
               ELSE
                  DOUT( I ) = DATVAL
                  VOUT( I ) = VARVAL

               END IF

*  If one of the input values were bad, put both bad in the output
*  arrays and set the returned bad pixel flag.
            ELSE
               DOUT( I ) = VAL__BADD
               VOUT( I ) = VAL__BADD
               BAD = .TRUE.
            END IF

         END DO

*  Now handle SNR limits...
      ELSE IF( TYPE .EQ. 'SNR' ) THEN

*  Loop round each element of the arrays.
         DO I = 1, EL

*  Get the input values.
            VARVAL = VIN( I )
            DATVAL = DIN( I )

*  If neither are invalid, proceed.
            IF( VARVAL .NE. VAL__BADD .AND. DATVAL .NE. VAL__BADD ) THEN

*  Compare the SNR with the limit, and store bad values in the
*  output arrays if it is to low.
               IF( ABS( DATVAL ) .LT.
     :             ABS( SQRT( MAX( 0.0D0, VARVAL ) )*LIMIT ) ) THEN
                  DOUT( I ) = VAL__BADD
                  VOUT( I ) = VAL__BADD
                  BAD = .TRUE.
                  NBAD = NBAD + 1

*  Otherwise, store the input values in the output arrays.
               ELSE
                  DOUT( I ) = DATVAL
                  VOUT( I ) = VARVAL

               END IF

*  If one of the input values were bad, put both bad in the output
*  arrays and set the returned bad pixel flag.
            ELSE
               DOUT( I ) = VAL__BADD
               VOUT( I ) = VAL__BADD
               BAD = .TRUE.
            END IF

         END DO

*  Report an error if the limit type is unknown.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( 'ERRCL_ERR1', 'ERRCL: Unknown limit type '//
     :                 '''^TYPE'' (programming error).', STATUS )
      END IF

      END
