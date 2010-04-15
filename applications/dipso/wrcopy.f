       SUBROUTINE WRCOPY( COMM, NBREAK, BREAK, N, FLUX, WAVE, M, Y, X,
     :                    STATUS )
*+
*  Name:
*     WRCOPY

*  Purpose:
*     Copy X and Y values from internal to external arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRCOPY( COMM, NBREAK, BREAK, N, FLUX, WAVE, M, Y, X, STATUS )

*  Description:
*     The supplied WAVE and FLUX values are copied to the X and Y
*     arrays. If COMM indicates that the X and Y arrays are to be in
*     SPECTRUM format 0 (i.e. if the current command is SP0WR), then two
*     zeros are inserted into the Y array at each break point
*     (corresponding X values are also inserted). For this reason the
*     output arrays may be larger than the input arrays.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     NBREAK = INTEGER (Given)
*        The number of breaks in the FLUX and WAVE arrays.
*     BREAK( NBREAK ) = INTEGER (Given)
*        The pixel indices of the breaks in X and Y. These should be
*        sorted into ascending order.
*     N = INTEGER (Given)
*        The number of elements in the FLUX and WAVE arrays.
*     FLUX( N ) = REAL (Given)
*        The flux values.
*     WAVE( N ) = REAL (Given)
*        The wavelength values.
*     M = INTEGER (Given)
*        The size of the X and Y arrays. This should be the correct size
*        to include any zeros inserted to create a SPECTRUM format 0
*        output.
*     Y( M ) = REAL (Returned)
*        The Y axis values.
*     X( M ) = REAL (Returned)
*        The X axis values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-AUG-1994 (DSB):
*        Original version.
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
      INTEGER NBREAK
      INTEGER BREAK( NBREAK )
      INTEGER N
      REAL FLUX( N )
      REAL WAVE( N )
      INTEGER M

*  Arguments Given:
      REAL Y( M )
      REAL X( M )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        I,                 ! Index of next element to be read
     :        IBRK,              ! Index of next break
     :        J,                 ! Index of next element to be written
     :        NXTBRK             ! The next break point

      REAL
     :        DWAV,              ! Minimum step in X
     :        XSTEP              ! Step in X

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a SPECTRUM format 0 file is being produced...
      IF( COMM .EQ. 'SP0WR' ) THEN

*  Initialise the index within X and Y at which the next output values will
*  be stored.
         J = 1

*  Store the index within FLUX and WAVE at which the first break occurs.
         IBRK = 1
         NXTBRK = BREAK( IBRK )

*  Store the maximum step in X between points in a gap.
         DWAV = 0.0001

*  Loop round all the elements in the input arrays (FLUX and WAVE).
         DO I = 1, N

*  Copy the input values, and increment the pointer to the next element
*  of the output arrays.
            X( J ) = WAVE( I )
            Y( J ) = FLUX( I )
            J = J + 1

*  If there is a break in the spectrum following the current pixel
*  (ignoring the break after the last pixel)...
            IF( I .EQ. NXTBRK .AND. I .NE. N ) THEN

*  Calculate the step in X (this bit is taken from some IDH code and isn't
*  explained).
               XSTEP = MIN( DWAV, ( WAVE( I + 1 ) - WAVE( I ) )/3.0 )

*  Insert two zeros. The X values are set XSTEP away from the nearest
*  neighbour (above or below).
               Y( J ) = 0.0
               X( J ) = WAVE( I ) + XSTEP
               J = J + 1

               Y( J ) = 0.0
               X( J ) = WAVE( I + 1 ) - XSTEP
               J = J + 1

*  Store the next break point. Report an error if they are not monotonic.
               IBRK = IBRK + 1
               NXTBRK = BREAK( IBRK )

               IF( NXTBRK .LE. I ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'IBRK', IBRK )
                  CALL MSG_SETI( 'NXTBRK', NXTBRK )
                  CALL MSG_SETI( 'LSTBRK', I )
                  CALL ERR_REP( 'WRCOPY_ERR1', 'Break point ^IBRK '//
     :                          '(at element ^NXTBRK) occurs before '//
     :                          'or at the previous one (at element '//
     :                          '^LSTBRK) - (programming error).',
     :                          STATUS )
                  GO TO 999
               END IF

            END IF

         END DO

*  If a normal DIPSO array is being created, just copy the data.
      ELSE

         DO I = 1, N
            X( I ) = WAVE( I )
            Y( I ) = FLUX( I )
         END DO

      END IF

*  Jump to here if an error occurs.
 999  CONTINUE

      END
