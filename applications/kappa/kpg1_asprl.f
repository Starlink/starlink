      SUBROUTINE KPG1_ASPRL( MAP, LBND, UBND, PARAL, STATUS )
*+
*  Name:
*     KPG1_ASPRL

*  Purpose:
*     See if all current Frame axes are parallel to the corresponding pixel
*     axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASPRL( MAP, LBND, UBND, PARAL, STATUS )

*  Description:
*     This routine returns a .TRUE. value for PARAL if all the axes in
*     the current Frame of the NDF are parallel to the axes with the same 
*     indices in the PIXEL Frame. Thus, the GRID and AXIS Frame will be 
*     parallel to the PIXEL Frame, but a SKY Frame (in general) will not be.

*  Arguments:
*     MAP = INTEGER (Given)
*        The Mapping from PIXEL Frame to the NDFs current Frame.
*     LBND( * ) = INTEGER (Given)
*        The lower pixel bopunds.
*     UBND( * ) = INTEGER (Given)
*        The upper pixel bopunds.
*     PARAL = LOGICAL (Returned)
*        Returned .TRUE. only if all axes in the current Frame of the
*        NDF are parallel to the corresponding axes in the PIXEL Frame.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-JAN-2000 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER MAP
      INTEGER LBND( * )
      INTEGER UBND( * )

*  Arguments Returned:
      LOGICAL PARAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NTEST              ! No. of test points to use
      PARAMETER ( NTEST = 20 )   
      
*  Local Variables:
      DOUBLE PRECISION AXMAX     ! Largest axis value
      DOUBLE PRECISION AXMIN     ! Smallest axis value
      DOUBLE PRECISION AXVAL     ! Axis value
      DOUBLE PRECISION CTEST( NTEST, NDF__MXDIM ) ! Current Frame test points
      DOUBLE PRECISION DELT      ! PIXEL step between test points
      DOUBLE PRECISION OFFSET    ! PIXEL offset at tstart of 1st cell
      DOUBLE PRECISION PTEST( NTEST, NDF__MXDIM ) ! PIXEL Frame test points
      DOUBLE PRECISION RAN1      ! Largest range on any current Frame axis
      DOUBLE PRECISION RAN2      ! Second largest range on any cur. Frame axis
      DOUBLE PRECISION RANGE     ! Range of current Frame axis values
      INTEGER I                  ! PIXEL Frame axis index
      INTEGER J                  ! Axis index
      INTEGER J1                 ! Index of axis with largest range
      INTEGER J2                 ! Index of axis with second largest range
      INTEGER K                  ! Test point index
      INTEGER NAX                ! No. of axes in NDF
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Assume for the moment that all axes are parallel.
      PARAL = .TRUE.

*  Store the number of axes in the PIXEL Frame.
      NAX = AST_GETI( MAP, 'NIN', STATUS )

*  The Current Frame is not equivalent to the PIXEL Frame if they have
*  different number of axes.
      IF( AST_GETI( MAP, 'NOUT', STATUS ) .NE. NAX ) THEN
         PARAL = .FALSE.

*  Otherwise...
      ELSE

*  Loop round each axis.
       DO I = 1, NAX

*  Set up a set of evenly spaced points along the current PIXEL Frame axis
*  (other axes are set to a constant value of 0.5).
            DELT = DBLE( UBND( I ) - LBND( I ) + 1 )/DBLE( NTEST - 1 )
            OFFSET = DBLE( LBND( I ) ) - 0.5D0
            DO J = 1, NAX
               IF( J .NE. I ) THEN
                  DO K = 1, NTEST
                     PTEST( K, J ) = OFFSET
                  END DO
               ELSE
                  DO K = 1, NTEST
                     PTEST( K, J ) = ( K - 1 )*DELT + OFFSET
                  END DO
               END IF       
            END DO

*  Transform these points into the current Frame.
            CALL AST_TRANN( MAP, NTEST, NAX, NTEST, PTEST, .TRUE.,
     :                      NAX, NTEST, CTEST, STATUS ) 

*  Find the current Frame axes which have the largest and second largest
*  ranges of mapped axis value in these points.
            RAN1 = VAL__MIND
            RAN2 = VAL__MIND

            DO J = 1, NAX
               AXMAX = VAL__MIND
               AXMIN = VAL__MAXD

               DO K = 1, NTEST
                  AXVAL = CTEST( K, J ) 
                  IF( AXVAL .NE. AST__BAD ) THEN
                     AXMAX = MAX( AXMAX, AXVAL )
                     AXMIN = MIN( AXMIN, AXVAL )
                  END IF
               END DO

               IF( AXMAX .GT. AXMIN ) THEN
                  RANGE = AXMAX - AXMIN
                  IF( RANGE .GT. RAN1 ) THEN
                     RAN2 = RAN1
                     J2 = J1
                     RAN1 = RANGE
                     J1 = J
                  ELSE IF( RANGE .GT. RAN2 ) THEN
                     RAN2 = RANGE
                     J2 = J
                  END IF
               END IF

            END DO

*  The Frames are not parallel if the current Frame axis with the largest
*  range is different to the PIXEL Frame axis being used at the moment.
            IF( J1 .NE. I ) THEN
               PARAL = .FALSE.
               GO TO 999
            END IF

*  The Frames are not parallel if more than one current Frame axis has a
*  significant range of values (i.e. if the second largest range on any 
*  current Frame axis is significant compared with the largest range).
            IF( RAN2 .GT. RAN1*1.0D-6 ) THEN    
               PARAL = .FALSE.
               GO TO 999
            END IF

         END DO

      END IF

 999  CONTINUE

      END
