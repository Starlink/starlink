      SUBROUTINE ARD1_POLAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                       NARG, I, KEYW, STATUS )
*+
*  Name:
*     ARD1_POLAR

*  Purpose:
*     Assemble argument list for a POLYGON keyword

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_POLAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND, NARG,
*                      I, KEYW, STATUS )

*  Description:
*     Each supplied vertex is transformed from user co-ordinates to
*     pixel co-ordinates, and then stored on the operand stack.

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
*        The index of the final character in ELEM to be checked.
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
      INTEGER AXIS               ! Axis index
      LOGICAL OK                 ! Was an argument value obtained?
      REAL ARGS( ARD__MXDIM )    ! A set of co-ordinates
      REAL VALUE                 ! The argument value

*  Ensure that the contents of the ARGS array is saved between
*  invocations of this routine.
      SAVE ARGS
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error and abort if the dimensionality is not 2.
      IF( NDIM .NE. 2 ) THEN
         STATUS = ARD__NOT2D
         CALL ERR_REP( 'ARD1_POLAR_ERR1', 'POLYGON keyword found in '//
     :                 'non-2D ARD description.', STATUS )
         GO TO 999
      END IF

*  Attempt to read argument value from the current element until the
*  end of the element, or the end of the argument list is encountered.
      DO WHILE( I .LE. L .AND. KEYW .AND. STATUS .EQ. SAI__OK ) 

*  Read the next argument.
         CALL ARD1_GTARG( ELEM, L, I, OK, KEYW, VALUE, STATUS )

*  If an argument was obtained, work out which axis it refers to (they
*  cycle round from 1 to 2).
         IF( OK ) THEN
            NARG = NARG + 1
            AXIS = MOD( NARG - 1, 2 ) + 1

*  Store the argument value in a temporary array.
            ARGS( AXIS ) = VALUE

*  If a complete set of co-ordinates have now been obtained, transform
*  them using the supplied transformation and store in the operand
*  stack.
            IF( AXIS .EQ. 2 ) CALL ARD1_STORP( 2, C, ARGS, IPOPND,
     :                                         IOPND, SZOPND,
     :                                         STATUS )

*  If the end of the argument list has been reached, report an error if
*  the number of arguments obtained is incorrect (i.e. if it not a
*  multiple of 2).
         ELSE IF( .NOT. KEYW .AND. STATUS .EQ. SAI__OK ) THEN
 
            IF( AXIS .NE. 2 ) THEN
               STATUS = ARD__ARGS
               CALL ERR_REP( 'ARD1_POLAR_ERR2', 'Incorrect number of '//
     :                       'arguments found.', STATUS )

*  Report an error if less than 3 vertices have been supplied.
            ELSE IF( NARG .LT. 6 ) THEN
               STATUS = ARD__ARGS
               CALL ERR_REP( 'ARD1_POLAR_ERR3', 'Argument list '//
     :                       'specifies less than 3 vertices.',
     :                       STATUS )

            END IF

         END IF

      END DO

*  Jump to here if an error occurs.
 999  CONTINUE

      END
