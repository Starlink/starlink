      SUBROUTINE ARD1_COLAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                       NARG, I, KEYW, STATUS )
*+
*  Name:
*     ARD1_COLAR

*  Purpose:
*     Assemble argument list for a COLUMN keyword

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_COLAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND, NARG,
*                      I, KEYW, STATUS )

*  Description:
*     The first two values stored on the operand stack are the
*     components of a 2-D vector (in pixel co-ordinates) giving the
*     direction of the lines corresponding to the specified columns.
*     After that come pairs of (x,y) pixel co-ordinates (one for each
*     column) of the points (XX,0) where XX is one of the supplied
*     argument values.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The dimensionality of the ARD description (i.e. the number of
*        values required to specify a position).
*     C( * ) = REAL (Given)
*        The co-efficients of the current mapping from supplied
*        co-ordinates to pixel co-ordinates.
*     ELEM = CHARACTER * ( * ) (Given)
*        An element of an ARD description.
*     L = INTEGER (Given)
*        The index of the last character in ELEM to be checked.
*     IPOPND = INTEGER (Given)
*        The pointer to the array holding the operand stack.
*     IOPND = INTEGER (Given and Returned)
*        The index within the operand stack at which the next value
*        should be stored.
*     SZOPND = INTEGER (Given and Returned)
*        The size of the operand stack. This is increased if necessary.
*     NARG = INTEGER (Given and Returned)
*        The number of arguments so far obtained. This should be
*        supplied equal to -1 if no argument list has yet been found.
*     I = INTEGER (Given and Returned)
*        The index of the next character to be checked in ELEM.
*     KEYW = LOGICAL (Given and Returned)
*        Returned as .FALSE. if the argument list for the keyword has
*        been completed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Arguments Given:
      INTEGER NDIM
      REAL C( * )
      CHARACTER ELEM*(*)
      INTEGER L

*  Arguments Given and Returned:
      INTEGER IPOPND
      INTEGER IOPND
      INTEGER SZOPND
      INTEGER NARG
      INTEGER I
      LOGICAL KEYW

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL OK                 ! Was an argument value obtained?
      REAL VALUE                 ! The argument value
      REAL X                     ! X pixel co-ord at user x axis
      REAL Y                     ! Y pixel co-ord at user x axis

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error and abort if the dimensionality is not 2.
      IF( NDIM .NE. 2 ) THEN
         STATUS = ARD__NOT2D
         CALL ERR_REP( 'ARD1_COLAR_ERR1', 'Ard mask is not 2 '//
     :                 'dimensional.', STATUS )
         GO TO 999
      END IF

*  Attempt to read argument value from the current element until the
*  end of the element, or the end of the argument list is encountered.
      DO WHILE( I .LE. L .AND. KEYW .AND. STATUS .EQ. SAI__OK ) 

*  Read the next argument.
         CALL ARD1_GTARG( ELEM, L, I, OK, KEYW, VALUE, STATUS )

*  If an argument was obtained, increment the number of arguments
*  supplied by the user.
         IF( OK ) THEN
            NARG = NARG + 1

*  If an argument list has just been started, store the direction
*  vector of the lines in pixel co-ordinates on the operand stack.
            IF( NARG .EQ. 1 ) THEN
               CALL ARD1_STORR( C( 3 ), SZOPND, IOPND, IPOPND, STATUS )
               CALL ARD1_STORR( C( 6 ), SZOPND, IOPND, IPOPND, STATUS )
            END IF

*  Find the pixel co-ordinates corresponding to position (VALUE,0).
            X = C( 1 ) + C( 2 )*VALUE
            Y = C( 4 ) + C( 5 )*VALUE

*  Store them on the operand stack.
            CALL ARD1_STORR( X, SZOPND, IOPND, IPOPND, STATUS )
            CALL ARD1_STORR( Y, SZOPND, IOPND, IPOPND, STATUS )
  
         END IF

      END DO

*  Jump to here if an error occurs.
 999  CONTINUE

      END
