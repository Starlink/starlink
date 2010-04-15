      SUBROUTINE BADCHK( COMM, BADVAL, MXPIX, MXBRK, N, X, Y, NBREAK,
     :                   BREAK, WORK, STATUS )
*+
*  Name:
*     BADCHK

*  Purpose:
*     Remove bad values from the X and Y arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL BADCHK( COMM, BADVAL, MXPIX, MXBRK, N, X, Y, NBREAK, BREAK,
*                  WORK, STATUS )

*  Description:
*     The contents of the X and Y arrays are shuffled down towards
*     element 1 to remove any elements which have bad values in either
*     X or Y. A break point is added to the BREAK array for each
*     contiguous group of bad pixels. An error is reported if all the
*     data is bad. The resulting break points are sorted into ascending
*     order. The positions of any breaks supplied in BREAK on entry are
*     modified to take account of any pixel shifts caused by the removal
*     of bad pixels.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     BADVAL = REAL (Given)
*        The value used to flag bad pixels.
*     MXPIX = INTEGER (Given)
*        The size of the X and Y arrays.
*     MXBRK = INTEGER (Given)
*        The size of the BREAK array.
*     N = INTEGER (Given and Returned)
*        The number of elements in the X and Y arrays.
*     X( MXPIX ) = REAL (Given and Returned)
*        The X axis values.
*     Y( MXPIX ) = REAL (Given and Returned)
*        The Y axis values.
*     NBREAK = INTEGER (Given and Returned)
*        The number of breaks in the X and Y arrays.
*     BREAK( MXBRK ) = INTEGER (Given and Returned)
*        The pixel indices of the breaks in X and Y.
*     WORK( MXBRK ) = INTEGER (Returned)
*        Work space used to hold break points.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-AUG-1994 (DSB):
*        Original version.
*     23-JAN-1995 (DSB):
*        Algorithm corrected to handle break points properly.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER COMM*(*)
      REAL BADVAL
      INTEGER MXPIX
      INTEGER MXBRK

*  Arguments Given and Returned:
      INTEGER N
      REAL X( MXPIX )
      REAL Y( MXPIX )
      INTEGER NBREAK
      INTEGER BREAK( MXBRK )

*  Arguments Returned:
      INTEGER WORK( MXBRK )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        IBRK,              ! Index of next supplied break
     :        IR,                ! Index of next element to be read
     :        IW,                ! Index of next element to be written
     :        NBAD,              ! No. of bad pixels found
     :        NBRK,              ! No. of supplied breaks
     :        NXTBRK             ! Next supplied break

      LOGICAL
     :        BAD                ! Is the element bad?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If any break points have been supplied, ensure that they are in
*  ascending order. Store the number of supplied break points and the
*  index of the first. Report an error if the first break is at an index
*  of less than 1.
      IF( NBREAK .GT. 0 ) THEN
         CALL SORT1I( NBREAK, BREAK )
         IBRK = 1
         NXTBRK = BREAK( 1 )

         IF( NXTBRK .LT. 1 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'BRK', NXTBRK )
            CALL ERR_REP( 'BADCHK_ERR1', 'BADCHK: Break at illegal '//
     :                    'index (^BRK) supplied (programming error).',
     :                    STATUS )
            GO TO 999
         END IF

      ELSE
         NXTBRK = 0
      END IF

*  Indicate that there are currently no breaks in the returned array.
      NBRK = 0

*  Initialise the index within X and Y at which the next values are to
*  be written.
      IW = 1

*  Set a flag indicating if the first input element has bad X or Y values.
      BAD = ( X( 1 ) .EQ. BADVAL .OR. Y( 1 ) .EQ. BADVAL )

*  Loop through the X and Y arrays (excluding the last point which is
*  done as a special case later).
      DO IR = 1, MIN( N, MXPIX ) - 1

*  Only store good input values in the returned array.
         IF( .NOT. BAD ) THEN

*  Shuffle the current X and Y values towards the start of the array if
*  required, in order to over-write any previously encountered bad values.
            IF( IR .GT. IW ) THEN
               X( IW ) = X( IR )
               Y( IW ) = Y( IR )
            END IF

*  Now check the next input element to see if it is bad.
            BAD = X( IR + 1 ) .EQ. BADVAL .OR. Y( IR + 1 ) .EQ. BADVAL

*  The current element of the returned arrays will be a break point
*  if it was break point in the supplied arrays, OR if the
*  following input element is bad. First deal with the former
*  case.
            IF( IR .EQ. NXTBRK ) THEN
               NBRK = NBRK + 1
               IF( NBRK .LE. MXBRK ) WORK( NBRK ) = IW

               IBRK = IBRK + 1
               IF( IBRK .LE. NBREAK ) THEN
                  NXTBRK = BREAK( IBRK )
               ELSE
                  NXTBRK = 0
               END IF

*  Now store a break point if the next input element is bad.
            ELSE IF( BAD ) THEN
               NBRK = NBRK + 1
               IF( NBRK .LE. MXBRK ) WORK( NBRK ) = IW

            END IF

*  Increment the position at which the next good supplied value will be
*  stored in the returned array.
            IW = IW + 1

*  If the current input element i sbad, check the next input element to
*  see if it is also bad.
         ELSE
            BAD = X( IR + 1 ) .EQ. BADVAL .OR. Y( IR + 1 ) .EQ. BADVAL

         END IF

      END DO

*  Process the very last element if it is not bad...
      IF( .NOT. BAD ) THEN
         IR = MIN( N, MXPIX )

*  Store it at its new position in the returned array.
         IF( IR .GT. IW ) THEN
            X( IW ) = X( IR )
            Y( IW ) = Y( IR )
         END IF

*  The last element of the returned arrays will always be a break
*  point.
         NBRK = NBRK + 1
         WORK( NBRK ) = IW

*  Increment the position at which the next good supplied value will be
*  stored in the returned array.
         IW = IW + 1

      END IF

*  Report an error if too many break points were found.
      IF( NBRK .GT. MXBRK .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', NBRK )
         CALL MSG_SETI( 'M', MXBRK )
         CALL ERR_REP( 'BADCHK_ERR2', 'The NDF contains too many (^N)'//
     :                 'breaks. Maximum number allowed is ^M', STATUS )
         GO TO 999

*  Report an error if all the data is bad.
      ELSE IF( IW .EQ. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'BADCHK_ERR3', 'The NDF contains no valid data.',
     :                 STATUS )
         GO TO 999

*  Otherwise, tell the user how many pixels have been removed.
      ELSE
         NBAD = MIN( N, MXPIX ) - IW + 1

         IF( NBAD .EQ. 1 ) THEN
            CALL MSG_SETR( 'BADVAL', BADVAL )
            CALL MSGOUT( COMM, '1 bad data value equal to ^BADVAL '//
     :                   'found and removed.', .FALSE., STATUS)

         ELSE IF( NBAD .GT. 1 ) THEN
            CALL MSG_SETI( 'NBAD', NBAD )
            CALL MSG_SETR( 'BADVAL', BADVAL )
            CALL MSGOUT( COMM, '^NBAD bad data values equal to '//
     :                   '^BADVAL found and removed.', .FALSE., STATUS)
         END IF

      END IF

*  Return the size of the arrays after removal of the bad values. Never
*  return zero as this may cause subsequent subroutine calls to crash with
*  an access violation (if N should be zero, then an error will have
*  been reported any way).
      N = MAX( 1, IW - 1 )

*  Return the new array of break points.
      DO IBRK = 1, NBRK
         BREAK( IBRK ) = WORK( IBRK )
      END DO

      NBREAK = NBRK

*  Jump to here if an error occurs.
 999  CONTINUE

      END
