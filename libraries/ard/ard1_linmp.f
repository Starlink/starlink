      SUBROUTINE ARD1_LINMP( MAP, DLBND, DUBND, LINEAR, WCSDAT, STATUS )
*+
*  Name:
*     ARD1_LINMP

*  Purpose:
*     See if a mapping is linear and return a matrix describing it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_LINMP( MAP, DLBND, DUBND, LINEAR, WCSDAT, STATUS )

*  Description:
*     This routine checks to see if the supplied Mapping is linear,
*     except for an optional shift of origin.
*   
*     Unit vectors along each of the axes in the Mapping's input coordinate 
*     system are transformed using the Mapping. If the Mapping is linear, 
*     these transformed vectors form the columns of the required matrix. To 
*     test for linearity, a test point is transformed using the 
*     supplied Mapping, and is then also transformed using the candidate 
*     matrix formed by the above process. If the two resulting positions are 
*     more or less equal to each other, then the Mapping is considered to 
*     be linear. 
*
*     To account for a possible shift of origin, the origin of the Mappings
*     input Frame is transformed using the supplied Mapping, and this
*     position is subtracted from all other transformed positions.

*  Arguments:
*     MAP = INTEGER (Given)
*        A pointer to the Mapping to be checked.
*     DLBND( * ) = DOUBLE PRECISION (Given)
*        The lower bounds of the input domain.
*     DUBND( * ) = DOUBLE PRECISION (Given)
*        The upper bounds of the input domain.
*     LINEAR = LOGICAL (Returned)
*        Returned .TRUE. if the Mapping is linear and .FALSE. otherwise.
*     WCSDAT( * ) = DOUBLE PRECISION (Returned)
*        If the Mapping is linear, then WCSDAT is returned holding the 
*        coefficients of the linear mapping from pixel to user coords. 
*        Otherwise, wcsdat(1) holds a lower limit on the distance (within 
*        the user coords) per pixel, and the other elements in WCSDAT are 
*        not used. The supplied array should have at least NDIM*(NDIM+1) 
*        elements.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUL-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'ARD_ERR'          ! ARD error constants
      INCLUDE 'ARD_CONST'        ! ARD private constants

*  Arguments Given:
      INTEGER MAP
      INTEGER DLBND( * )
      INTEGER DUBND( * )

*  Arguments Returned:
      LOGICAL LINEAR
      DOUBLE PRECISION WCSDAT( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION EPS       ! Pixel increment from test position
      PARAMETER ( EPS = 0.05D0 ) 

*  Local Variables:
      DOUBLE PRECISION IN( ARD__MXDIM + 3, ARD__MXDIM ) ! Input positions
      DOUBLE PRECISION ORIG( ARD__MXDIM )               ! Transformed origin
      DOUBLE PRECISION OUT( ARD__MXDIM + 3, ARD__MXDIM )! Output positions
      DOUBLE PRECISION SUM       ! Mapped test value
      DOUBLE PRECISION SUMERR    ! Total squared error
      DOUBLE PRECISION SUMSQ     ! Max acceptable squared error
      DOUBLE PRECISION TEST( ARD__MXDIM )               ! Test position
      INTEGER I                  ! Loop count
      INTEGER J                  ! Loop count
      INTEGER K                  ! Loop count
      INTEGER NIN                ! No. of input axes
      INTEGER NOUT               ! No. of output axes
      INTEGER NTEST              ! No. of test positions tried so far
      LOGICAL MORE               ! Loop again?
*.

      LINEAR = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the number of input and output axes which the Mapping has.
      NIN = AST_GETI( MAP, 'NIN', STATUS )
      NOUT = AST_GETI( MAP, 'NOUT', STATUS )

*  Set the initial test point to the middle of the input domain.
      DO I = 1, NIN
         TEST( I ) = 0.5*( DLBND( I ) + DUBND( I ) ) 
      END DO

*  Loop until a good test point is found.
      MORE = .TRUE.
      NTEST = 0
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK ) 
         NTEST = NTEST + 1

*  Store unit vectors along each of the input axes, plus three extra points.
*  The first is the origin. The second is the test point. The third is like 
*  the second, but incremented by a small amount on each axis.
         DO I = 1, NIN
            DO J = 1, NIN + 1
               IN( J, I ) = 0.0
            END DO
            IN( I, I ) = 1.0
            IN( NIN + 2, I ) = TEST( I )
            IN( NIN + 3, I ) = TEST( I ) + EPS
         END DO

*  Transform these vectors using the supplied Mapping. If the Mapping is 
*  linear, the transformed data will define the required matrix, with three
*  extra columns holding the transformed extra points.
         CALL AST_TRANN( MAP, NIN + 3, NIN, ARD__MXDIM + 3, IN, .TRUE.,
     :                   NOUT, ARD__MXDIM + 3, OUT, STATUS )

*  Find the squared distance between the transformed second and third extra 
*  points. This is used as the upper limit on the squared distance
*  between two points if the two points are to be considered coincident.
         SUMSQ = 0.0
         MORE = .FALSE.
         DO I = 1, NOUT
            IF( OUT( NIN + 2, I ) .NE. AST__BAD .AND.
     :          OUT( NIN + 3, I ) .NE. AST__BAD ) THEN
               SUMSQ = SUMSQ + ( OUT( NIN + 2, I ) + 
     :                           OUT( NIN + 3, I ) )**2   
            ELSE    
               MORE = .TRUE.
               GO TO 10   
            END IF
         END DO

 10      CONTINUE

*  If the test point could not be transformed, choose another test point 
*  at random. Abort if more than 100 test points fail.
         IF( MORE ) THEN 
            IF( NTEST .GT. 100 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = ARD__NOTST
               CALL ERR_REP( 'ARD1_LINMP_ERR1', 'Cannot find any '//
     :                       'pixels with good user coordinates.', 
     :                       STATUS )
               GO TO 999
            ELSE
               DO I = 1, NIN
                  TEST( I )  = DLBND( I ) + DBLE( RAND( 0 ) )*
     :                                     ( DUBND( I ) - DLBND( I ) )
               END DO
            END IF
         END IF

      END DO

*  Subtract the transformed origin (the first extra position) from each of 
*  the other positions.
      DO I = 1, NOUT
         ORIG( I ) = OUT( NIN + 1, I )
         IF( ORIG( I ) .NE. AST__BAD ) THEN
            DO J = 1, NIN + 3
               IF( OUT( J, I ) .NE. AST__BAD ) THEN 
                  OUT( J, I ) = OUT( J, I ) - ORIG( I )
               ELSE
                  OUT( J, I ) = AST__BAD
               END IF
            END DO
         ELSE
            DO J = 1, NIN + 3
               OUT( J, I ) = AST__BAD
            END DO
         END IF
      END DO

*  We now test the Mapping for linearity. The test point is mapped using
*  the candidate matrix, and the squared distance between the resulting
*  point and the point found using the supplied Mapping is found. 
      SUMERR = 0.0
      DO I = 1, NOUT
         SUM = 0.0;
         DO J = 1, NIN
            IF( OUT( J, I ) .NE. AST__BAD ) THEN 
               SUM = SUM + OUT( J, I )*TEST( J )
            ELSE
               SUMERR = AST__BAD
               GO TO 20
            END IF
         END DO
         SUMERR = SUMERR + ( OUT( NIN + 2, I ) - SUM )**2
      END DO
 20   CONTINUE

*  If the squared distance is less than the maximum error obtained above, 
*  the Mapping is considered to be linear. Copy it to the returned array, 
*  row by row. 
      IF( SUMERR .NE. AST__BAD .AND. SUMERR .LT. SUMSQ ) THEN
         LINEAR = .TRUE.

         K = 1
         DO I = 1, NOUT
            WCSDAT( K ) = ORIG( I )
            K = K + 1
            DO J = 1, NIN
               WCSDAT( K ) = OUT( J, I )
               K = K + 1
            END DO
         END DO            

*  If the Mapping is not linear, return an estimate of the distance (within 
*  the user coords) per pixel.
      ELSE
         LINEAR = .FALSE.
         WCSDAT( 1 ) = SQRT( SUMSQ/DBLE( NIN ) )/EPS
      END IF

 999  CONTINUE

      END
