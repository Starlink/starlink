      SUBROUTINE ARD1_KEYW( TYPE, NEEDIM, NDIM, C, ELEM, L, IPOPND,
     :                      IOPND, PNARG, SZOPND, NARG, I, KEYW,
     :                      STATUS )
*+
*  Name:
*     ARD1_KEYW

*  Purpose:
*     Read keyword argument lists

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_KEYW( TYPE, NEEDIM, NDIM, C, ELEM, L, IPOPND, IOPND,
*                     PNARG, SZOPND, NARG, I, KEYW, STATUS )

*  Description:
*     This routine reads argument lists associated with keyword fields,
*     and stores corresponding information in the operand stack. The
*     routine will need to be called several times supplying new
*     elements each time if the argument list extends over more than
*     one element. A flag (KEYW) is returned to indicate when the
*     arguement list has been completed.

*  Arguments:
*     TYPE = INTEGER (Given)
*        An integer value identifying the current keyword.
*     NEEDIM = LOGICAL (Given)
*        Supplied .TRUE. if a DIMENSION statement is still needed to
*        establish the dimensionality of the ARD description. An error
*        is reported if this is supplied .TRUE.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the mask supplied to ARD_WORK.
*     C( * ) = REAL (Given)
*        The co-efficients of the current linear mapping from user
*        co-ordinates to pixel co-ordinates. The array should hold
*        NDIM*( NDIM + 1 ) values.
*     ELEM = CHARACTER * ( * ) (Given)
*        The text of the current element of the ARD description.
*     L = INTEGER (Given)
*        The index of the last non-blank character in ELEM.
*     IPOPND = INTEGER (Given and Returned)
*        A pointer to the one dimensional real work array holding the
*        operand stack. The array is extended if necessary.
*     IOPND = INTEGER (Given and Returned)
*        The index at which the next value is to be stored in the
*        operand stack.
*     PNARG = INTEGER (Given and Returned)
*        The number of values put onto the operand stack as a result of
*        the current keyword is itself stored on the operand stack at
*        the index supplied by PNARG. PNARG is incremented by one on
*        return.
*     SZOPND = INTEGER (Given and Returned)
*        The current size of the array pointed to by IPOPND. 
*     NARG = INTEGER (Given and Returned)
*        The number of arguments read from the keyword argument list.
*        Supplied equal to -1 if a new argument list is being started.
*     I = INTEGER (Given and Returned)
*        The index within ELEM of the next character to be checked.
*     KEYW = LOGICAL (Returned)
*        Returned .FALSE. if all the arguments for the current keyword
*        have now been read. Returned .TRUE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_KWSYM( ARD__NKEYW)*( ARD__SZKEY ) = CHARACTER (Read)
*           The symbols used to represent each keyword.
*        CMN_KWARG( ARD__NKEYW ) = INTEGER (Read)
*           The number of arguments required by each keyword.

*  Arguments Given:
      INTEGER TYPE
      LOGICAL NEEDIM
      INTEGER NDIM
      REAL C( * )
      CHARACTER ELEM*(*)
      INTEGER L
      INTEGER IPOPND

*  Arguments Given and Returned:
      INTEGER IOPND
      INTEGER PNARG
      INTEGER SZOPND
      INTEGER NARG
      INTEGER I

*  Arguments Returned:
      LOGICAL KEYW

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks

*  Local Variables:
      CHARACTER CC*1             ! Next character from ELEM
      INTEGER IOPND0             ! Top of operand stack on entry

*  Ensure that the local variable IOPND0 is saved between invocations 
*  of this routine.
      SAVE IOPND0
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is a new argument list, all we do this time through is
*  initialise things and find the opening parenthesis which marks the
*  start of the argument list. The routine then exists and is called
*  again to continue reading the argument list.
      IF( NARG .EQ. -1 ) THEN

*  Report an error and abort if a dimension statement is still needed.
*  No dimension statement is needed if NDIM is 2 (because 2 is the
*  default dimensionality).
         IF( NEEDIM ) THEN
            STATUS = ARD__BADDM
            CALL MSG_SETI( 'NDIM', NDIM )
            CALL ERR_REP( 'ARD1_KEYW_ERR1', 'ARD description is '//
     :                    'defaulting to 2-dimensions. It should be '//
     :                    '^NDIM dimensional.', STATUS )
            GO TO 999
         END IF

*  Save the current index of the top of the operand stack.
         IOPND0 = IOPND

*  Find the next non-blank character in ELEM. This should be the opening
*  parenthesis which marks the start of the argument list.
         CC = ELEM( I : I )
         DO WHILE( CC .EQ. ' ' .AND. I .LT. L ) 
            I = I + 1
            CC = ELEM( I : I )
         END DO

*  Increment the index of the next character to be checked so that it
*  refers to the first character after the opening parenthesis.
         I = I + 1

*  If the next non-blank character is an opening parenthesis, indicate
*  that the argument list has been started by setting the number of
*  arguments read so far to zero. Report an error if the current
*  keyword should not have an argument list.
         IF( CC .EQ. '(' ) THEN
            NARG = 0

            IF( CMN_KWARG( TYPE ) .EQ. 0 ) THEN
               STATUS = ARD__ARGS
               CALL ERR_REP( 'ARD1_KEYW_ERR2', 'Unnecessary argument '//
     :                       'list found.', STATUS )
            END IF
  
*  If some other non-blank character was found, an error is reported
*  unless the keyword should not have an argument list (in which case 
*  decrement the index of the next character to be checked so that the
*  current character can be included as part of the next field).

         ELSE IF( CC .NE. ' ' ) THEN
            IF( CMN_KWARG( TYPE ) .EQ. 0 ) THEN
               KEYW = .FALSE.
               I = I - 1
            ELSE
               STATUS = ARD__ARGS
               CALL ERR_REP( 'ARD1_KEYW_ERR3', 'No argument list '//
     :                       'found.', STATUS )
            END IF
         END IF

*  If an argument list has been started...
      ELSE

*  ... call a routine to read the keyword argument list from the ARD
*  description and store appropriate values in the returned operand
*  array. Note, keywords which do not have an argument list are not
*  included in this list.

*  POINT and PIXEL keywords...
         IF( TYPE .EQ. ARD__POI .OR. TYPE .EQ. ARD__PIX ) THEN
            CALL ARD1_POIAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                       NARG, I, KEYW, STATUS )

*  LINE keywords...
         ELSE IF( TYPE .EQ. ARD__LIN ) THEN
            CALL ARD1_LINAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                       NARG, I, KEYW, STATUS )

*  ROW keywords...
         ELSE IF( TYPE .EQ. ARD__ROW ) THEN
            CALL ARD1_ROWAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  COLUMN keywords...
         ELSE IF( TYPE .EQ. ARD__COL ) THEN
            CALL ARD1_COLAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  BOX keywords...
         ELSE IF( TYPE .EQ. ARD__BOX ) THEN
            CALL ARD1_BOXAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  RECT keywords...
         ELSE IF( TYPE .EQ. ARD__REC ) THEN
            CALL ARD1_RECAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  ROTBOX keywords...
         ELSE IF( TYPE .EQ. ARD__ROT ) THEN
            CALL ARD1_ROTAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  POLYGON keywords...
         ELSE IF( TYPE .EQ. ARD__POL ) THEN
            CALL ARD1_POLAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  CIRCLE keywords...
         ELSE IF( TYPE .EQ. ARD__CIR ) THEN
            CALL ARD1_CIRAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  ELLIPSE keywords...
         ELSE IF( TYPE .EQ. ARD__ELL ) THEN
            CALL ARD1_ELLAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  FRAME keywords...
         ELSE IF( TYPE .EQ. ARD__FRA ) THEN
            CALL ARD1_FRAAR( NDIM, C, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

         END IF

*  If the argument list is complete, store the number of values added
*  to the operand stack, at the index supplied in argument PNARG.
         IF( .NOT. KEYW ) CALL ARD1_STORR( REAL( IOPND - IOPND0 ),
     :                                   SZOPND, PNARG, IPOPND, STATUS )

      END IF

*  Jump to here if an error occurs.
 999  CONTINUE

*  Give a context message if an error has occurred.      
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'ELEM', ELEM )
         CALL MSG_SETC( 'KW', CMN_KWSYM( TYPE ) )
         CALL ERR_REP( 'ARD1_KEYW_ERR4', 'Unable to read argument '//
     :                 'list for a ^KW keyword in ARD description '//
     :                 '''^ELEM''.', STATUS )
      END IF

      END
