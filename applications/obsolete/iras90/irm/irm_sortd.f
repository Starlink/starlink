      SUBROUTINE IRM_SORTD( ASCEND, KEYROW, NVAL, NROWS, DATA, NGOOD,
     :                      STATUS )
*+
*  Name:
*     IRM_SORTD

*  Purpose:
*     Sort an array of values into ascending or descending order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_SORTD( ASCEND, KEYROW, NVAL, NROWS, DATA, NGOOD, STATUS )

*  Description:
*     The values in a specified row of the data array are sorted in to
*     ascending or descending order, using simple bubblesort method.
*     Any bad values present are stored at the end of the sorted array.
*     At the same time, corresponding elements in all the other rows are
*     moved to keep track with the elements in the sorted row.

*  Arguments:
*     ASCEND = LOGICAL (Given)
*        If true the values are sorted so that the last is greater than
*        the first. Otherwise the reverse is done.
*     KEYROW = INTEGER (Given)
*        The row to use as the basis for the rearrangement. The values
*        in this row are sorted into order, and the other rows are
*        rearranged so that corresponding elements stay together,
*     NVAL = INTEGER (Given)
*        The no. of elements in each row of the array.
*     NROWS = INTEGER (Given)
*        The no. of rows of the array.
*     DATA( NVAL, NROWS ) = DOUBLE PRECISION (Given and Returned)
*        The data.
*     NGOOD = INTEGER (Returned)
*        The number of good values in the sorted row. These will occupy
*        elements 1 to NGOOD in row KEYROW of the output array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Arguments Given:
      LOGICAL ASCEND
      INTEGER KEYROW
      INTEGER NVAL
      INTEGER NROWS

*  Arguments Given and Returned:
      DOUBLE PRECISION DATA( NVAL, NROWS )

*  Arguments Returned:
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   ADJEL            ! The element adjacent to the current
                                 ! element.
      LOGICAL   DONE             ! True when the first NGOOD elements
                                 ! of the array data are in the correct
                                 ! order.
      INTEGER   ELEMNT           ! The current element of the array
                                 ! being checked for correct order.
      INTEGER   I                ! Index of last element written to.
      INTEGER   J                ! Index of last element read.
      INTEGER   LASTEL           ! Points to the last element of the
                                 ! array which needs to be checked for
                                 ! being in order.
      INTEGER   ROW              ! Row index.
      DOUBLE PRECISION TEMP      ! Temporary storage.

*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Report an error if KEYROW is not within the bounds of the array.
      IF( KEYROW .LT. 1 .OR. KEYROW .GT. NROWS ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'K', KEYROW )
         CALL ERR_REP( 'IRM_SORTD_ERR1',
     :                 'IRM_SORTD: KEYROW ^K is out of range.', STATUS )
         GO TO 999
      END IF

*  First of all go through shunting data towards the start of the array
*  in order to remove any bad values.
      I = 0
      DO J = 1, NVAL
         IF( DATA( J, KEYROW ) .NE. VAL__BADD ) THEN
            I = I + 1

            DO ROW = 1, NROWS
               DATA( I, ROW ) = DATA( J, ROW )
            END DO

         END IF
      END DO

*  Save the number of good values in the sorted row of the array.
*  Return if none remain.
      NGOOD = I
      IF( NGOOD .EQ. 0 ) GO TO 999

*  Fill up the remainder of the array with bad values.
      DO I = NGOOD + 1, NVAL
         DO ROW = 1, NROWS
            DATA( I, ROW ) = VAL__BADD
         END DO
      END DO

*  Initialize things.
      LASTEL = NGOOD
      DONE = .FALSE.

*  Loop until data is in correct order
      DO WHILE( .NOT. DONE )

*  On each pass through the data the highest number gets 'washed' down
*  to the end of the array, therefore it is not neccessary to check the
*  previously last element because it is known to be in the right order
         LASTEL = LASTEL - 1
         DONE = .TRUE.

*  A different comparison operator is used for ascending and descending
*  modes.
         IF( ASCEND ) THEN

*  Go through all the data considering adjacent pairs.
            DO ELEMNT = 1, LASTEL
               ADJEL = ELEMNT + 1

*  If the pair is in the wrong order swap them round
               IF( DATA( ELEMNT, KEYROW ) .GT.
     :             DATA( ADJEL, KEYROW ) ) THEN

                  DO ROW = 1, NROWS
                     TEMP = DATA( ELEMNT, ROW )
                     DATA( ELEMNT, ROW ) = DATA( ADJEL, ROW )
                     DATA( ADJEL, ROW ) = TEMP
                  END DO

*  Flag that a pair was found in the wrong order.
                  DONE = .FALSE.

               ENDIF

            ENDDO

*  Now deal with descending mode.
         ELSE

*  Go through all the data considering adjacent pairs.
            DO ELEMNT = 1, LASTEL
               ADJEL = ELEMNT + 1

*  If the pair is in the wrong order swap them round
               IF( DATA( ELEMNT, KEYROW )
     :             .LT. DATA( ADJEL, KEYROW ) ) THEN

                  DO ROW = 1, NROWS
                     TEMP = DATA( ELEMNT, ROW )
                     DATA( ELEMNT, ROW ) = DATA( ADJEL, ROW )
                     DATA( ADJEL, ROW ) = TEMP
                  END DO

*  Flag that a pair was found in the wrong order.
                  DONE = .FALSE.

               ENDIF

            ENDDO

         END IF

*  Loop round for next pass through data.
      ENDDO

 999  CONTINUE

      END
