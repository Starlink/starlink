      SUBROUTINE ARD1_FRAAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                       NARG, I, KEYW, STATUS )
*+
*  Name:
*     ARD1_FRAAR

*  Purpose:
*     Assemble argument list for a FRAME keyword

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_FRAAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND, NARG,
*                      I, KEYW, STATUS )

*  Description:
*     The supplied border width is transformed from user co-ordinates
*     to pixel co-ordinates and stored on the stack. The current
*     transformation must be 2-D and have equal scales in both
*     directions.

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
      INCLUDE 'PRM_PAR'          ! VAL_ constants
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
      REAL XSCA                  ! X scale factor
      REAL YSCA                  ! Y scale factor



*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error and abort if the dimensionality is not 2.
      IF( NDIM .NE. 2 ) THEN
         STATUS = ARD__NOT2D
         CALL ERR_REP( 'ARD1_FRAAR_ERR1', 'ARD mask is not 2 '//
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

*  Calculate the X and Y scale factors between user co-ordinates and
*  pixel co-ordinates.
            XSCA = SQRT( C( 2 )**2 + C( 5 )**2 ) 
            YSCA = SQRT( C( 3 )**2 + C( 6 )**2 ) 

*  Report an error and abort if the two scale factors are not equal.
            IF( ABS( XSCA - YSCA ) .GE. ABS( XSCA*VAL__EPSR ) .AND.
     :          STATUS .EQ. SAI__OK ) THEN
               STATUS = ARD__SCALE
               CALL ERR_REP( 'ARD1_FRAAR_ERR2', 'Current user '//
     :                       'co-ordinate system has different '//
     :                       'scales in the X and Y directions.',
     :                       STATUS )
               GO TO 999
            END IF

*  Store the transformed border width.
            CALL ARD1_STORR( XSCA*VALUE, SZOPND, IOPND, IPOPND, STATUS )

*  If the end of the argument list has been reached, report an error if
*  the number of arguments obtained is incorrect.
         ELSE IF( .NOT. KEYW ) THEN
 
            IF( NARG .NE. 1 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = ARD__ARGS
               CALL ERR_REP( 'ARD1_FRAAR_ERR4', 'Incorrect number of '//
     :                       'arguments found.', STATUS )
            END IF

         END IF

      END DO

*  Jump to here if an error occurs.
 999  CONTINUE

      END
