      SUBROUTINE ARD1_STAT( TYPE, ELEM, L, NDIM, AC, NEEDIM, NARG, II,
     :                      UC, TC, STAT, STATUS )
*+
*  Name:
*     ARD1_STAT

*  Purpose:
*     Get statement arguments from the ARD description and modify the
*     appropriate parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_STAT( TYPE, ELEM, L, NDIM, AC, NEEDIM, NARG, II, UC,
*                     TC, STAT, STATUS )

*  Description:
*     On succesive passes through this routine, arguments are read from
*     the supplied element until a complete set of arguments has been
*     constructed. The arguments are then used to implement the required
*     effect (modifying the user transformation, etc).

*  Arguments:
*     TYPE = INTEGER (Given)
*        An integer value identifing the statement.
*     ELEM = CHARACTER * ( * ) (Given)
*        The text of the current element of the ARD description.
*     L = INTEGER (Given)
*        The index of the last character to be checked in ELEM.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the mask supplied to ARD_WORK.
*     AC( * ) = REAL (Given)
*        The co-efficients of the linear mapping from application
*        co-ordinates to pixel co-ordinates. The array should hold
*        NDIM*( NDIM + 1 ) values.
*     NEEDIM = LOGICAL (Given and Returned)
*        .TRUE. if a DIMENSION statement is still needed to define the
*        dimensionality of the ARD description.
*     NARG = INTEGER (Given and Returned)
*        The number of arguments extracted from the ARD description so
*        far for the current statement. Supplied equal to -1 if a new
*        argument list is being started.
*     II = INTEGER (Given and Returned)
*        The index within ELEM of the next character to be checked.
*     UC( * ) = REAL (Given and Returned)
*        The co-efficients of the current linear mapping from user
*        co-ordinates to application co-ordinates. The array should
*        hold NDIM*( NDIM + 1 ) values.
*     TC( * ) = REAL (Given and Returned)
*        The co-efficients of the total current linear mapping from
*        user co-ordinates to pixel co-ordinates. The array should hold
*        NDIM*( NDIM + 1 ) values. This is the concatentation of UC and
*        AC.
*     STAT = LOGICAL (Returned)
*        Returned .TRUE. if more arguments are required for the current
*        statement. Returned .FALSE. if the current statement has now
*        been completed.
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
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_STARG( ARD__NSTAT ) = INTEGER (Read)
*           The number of arguments required by each statement.
*        CMN_STSYM( ARD__NSTAT ) = CHARACTER * ( ARD__SZSTA )
*           Symbols used to represent statement fields.

*  Arguments Given:
      INTEGER TYPE
      CHARACTER ELEM*(*)
      INTEGER L
      INTEGER NDIM
      REAL AC(*)

*  Arguments Given and Returned:
      LOGICAL NEEDIM
      INTEGER NARG
      INTEGER II
      REAL UC(*)
      REAL TC(*)

*  Arguments Returned:
      LOGICAL STAT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks.

*  Local Variables:
      INTEGER
     :  ARGREQ,                  ! No. of arguments required
     :  I,                       ! Dimension counter
     :  IOFF,                    ! Index of offset term in mapping
     :  J,                       ! Row count
     :  K                        ! Column count

      REAL
     :  COST,                    ! Cosine of twist angle
     :  CT( ARD__MXDIM*( ARD__MXDIM + 1 ) ),! Modified user mapping
     :  SINT,                    ! Sine of twist angle
     :  STARGS( ARD__MXSAR ),    ! Statement argument values
     :  T                        ! Twist angle in radians

*  Ensure that the number of arguments required, and the arguments
*  values obtained so far are preserved.
      SAVE ARGREQ, STARGS

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a new argument list is being started...
      IF( NARG .EQ. -1 ) THEN

*  ...Store the number of arguments required for the statement.
         ARGREQ = CMN_STARG( TYPE )

*  Some statements have a variable number of arguments (indicated by
*  CMN_STARG being negative). Calculate the number of arguments required
*  for such statements.
         IF( ARGREQ .LT. 0 ) THEN

*  Report an error and abort if the dimensionality of the ARD
*  description has not yet been determined (NEEDIM is supplied .FALSE.
*  if NDIM is 2).
            IF( NEEDIM .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = ARD__BADDM
               CALL MSG_SETI( 'NDIM', NDIM )
               CALL ERR_REP( 'ARD1_STAT_ERR1', 'ARD description is'//
     :                     ' defaulting to 2-dimensions. It should be'//
     :                     ' ^NDIM dimensional.', STATUS )
               GO TO 999
            END IF

*  OFFSET - The number of arguments required for the OFFSET statement
*  equals the dimensionality of the ARD description. 
            IF( TYPE .EQ. ARD__OFF ) THEN
               ARGREQ = NDIM

*  STRETCH - The number of arguments required for the STRETCH statement
*  equals the dimensionality of the ARD description. 
            ELSE IF( TYPE .EQ. ARD__STR ) THEN
               ARGREQ = NDIM

*  COEFFS - The number of arguments required for the COEFFS statement
*  depends on the dimensionality of the ARD description. 
            ELSE IF( TYPE .EQ. ARD__COE ) THEN
               ARGREQ = NDIM*( NDIM + 1 )

*  Report an error for any other statement.
            ELSE IF (STATUS .EQ. SAI__OK ) THEN
               STATUS = ARD__INTER
               CALL MSG_SETI( 'T', TYPE )
               CALL ERR_REP( 'ARD1_STAT_ERR2', 'ARD1_STAT: Statement '//
     :                    'type ^T should not have variable argument '//
     :                    'list (programming error).', STATUS )
               GO TO 999
            END IF

         END IF

      END IF

*  Copy the argument list from the ARD description into a local array.
      CALL ARD1_ARGS( ELEM, L, ARGREQ, ARD__MXSAR, STARGS, NARG, II,
     :                STAT, STATUS )

*  If the argument list is complete, update the appropriate ARD
*  parameters.
      IF( .NOT. STAT ) THEN 

*  If it is a DIMENSION statement, report an error and abort if the
*  specified dimensionality is not the same as that of the mask.
         IF( TYPE .EQ. ARD__DIM ) THEN

            IF( NINT( STARGS( 1 ) ) .NE. NDIM .AND.
     :          STATUS .EQ. SAI__OK ) THEN
               STATUS = ARD__BADDM
               CALL MSG_SETI( 'NDIM', NDIM )
               CALL ERR_REP( 'ARD1_STAT_ERR3', 'ARD description '//
     :                       'should be ^NDIM dimensional.',STATUS )
               GO TO 999
            END IF

*  Otherwise, indicate that a DIMENSION statement has been found.
            NEEDIM = .FALSE.

*  If it is a COEFFS statement, store the argument list as the new
*  user transformation (user -> application).
         ELSE IF( TYPE .EQ. ARD__COE ) THEN
            CALL ARD1_TRCOP( NDIM, STARGS, UC, STATUS )

*  If it is an OFFSET statement, modify the current user transformation.
         ELSE IF( TYPE .EQ. ARD__OFF ) THEN

            DO I = 1, NDIM
               IOFF = ( I - 1 )*( NDIM + 1 ) + 1 
               UC( IOFF ) = UC( IOFF ) + STARGS( I )
            END DO

*  If it is a SCALE statement, modify the current user transformation.
*  Report an error if the scale factor is zero.
         ELSE IF( TYPE .EQ. ARD__SCA ) THEN

            IF( ABS( STARGS( 1 ) ) .LE. VAL__SMLR .AND.
     :          STATUS .EQ. SAI__OK ) THEN
               STATUS = ARD__INDET
               CALL MSG_SETC( 'ST', CMN_STSYM( ARD__SCA ) )
               CALL ERR_REP( 'ARD1_STAT_ERR4', '^ST statement '//
     :                       'has invalid argument 0.0', STATUS )
               GO TO 999

            ELSE

               DO I = 1, NDIM*( NDIM + 1 )
                  UC( I ) = UC( I )*STARGS( 1 )
               END DO

            END IF

*  If it is a TWIST statement, modify the current user transformation.
         ELSE IF( TYPE .EQ. ARD__TWI ) THEN

*  Save commonly used values.
            T = STARGS( 1 ) * ARD__DTOR
            SINT = SIN( T )
            COST = COS( T )

*  Initialise the new mapping to be the same as the old mapping.
            CALL ARD1_TRCOP( NDIM, UC, CT, STATUS )

*  Rotate the new mapping within the X-Y plane. The rotation is about
*  the origin of the application co-ordinate system.
            CT( 1 ) = UC( 1 )*COST - UC( 4 )*SINT
            CT( 2 ) = UC( 2 )*COST - UC( 5 )*SINT
            CT( 3 ) = UC( 3 )*COST - UC( 6 )*SINT
            CT( NDIM + 2 ) = UC( NDIM + 2 )*COST + UC( 1 )*SINT
            CT( NDIM + 3 ) = UC( NDIM + 3 )*COST + UC( 2 )*SINT
            CT( NDIM + 4 ) = UC( NDIM + 4 )*COST + UC( 3 )*SINT

*  Copy the new mapping to the returned array.
            CALL ARD1_TRCOP( NDIM, CT, UC, STATUS )

*  If it is a STRETCH statement...
         ELSE IF( TYPE .EQ. ARD__STR ) THEN

*  Initialise the index of the current mapping co-efficient
            I = 1
      
*  Loop through the rows of the mapping matrix
            DO J = 1, NDIM

*  Increment the pointer to the next co-efficient in order to skip
*  over the constant term.
               I = I + 1

*  Loop through the other columns of the mapping matrix
               DO K = 1, NDIM

*  Find the index of the current co-efficient within the supplied vector
*  of co-efficients, and then modify its value.
                  I = I + 1                  
                  UC( I ) = UC( I )*STARGS( K )

               END DO

            END DO

*  Report an error and abort for any other statement.
         ELSE
            STATUS = ARD__INTER
            CALL MSG_SETI( 'TYPE', TYPE )
            CALL ERR_REP( 'ARD1_STAT_ERR5', 'Illegal statement '//
     :                    'identifier (^TYPE) encountered in routine '//
     :                    'ARD1_STAT (programming error).', STATUS )
            GO TO 999

         END IF

*  Update the total transformation (user->pixel) by concatentating the
*  user and application transformation.
         CALL ARD1_TRCON( NDIM, UC, AC, TC, STATUS )

      END IF

*  Jump to here if an error occurs.
 999  CONTINUE

*  Give a context message if an error has occurred.      
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'ELEM', ELEM )
         CALL MSG_SETC( 'ST', CMN_STSYM( TYPE ) )
         CALL ERR_REP( 'ARD1_STAT_ERR6', 'Error processing ^ST '//
     :                 'statement in ARD description ''^ELEM''.',
     :                 STATUS )
      END IF
      
      END
