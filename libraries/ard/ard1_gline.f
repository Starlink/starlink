      INTEGER FUNCTION ARD1_GLINE( N, X, Y )
*+
*  Name:
*     ARD1_GLINE

*  Purpose:
*     Draw a 2D line into an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = ARD1_GLINE( N, X, Y )

*  Description:
*     A polyline is drawn into the 2-dimensional B array joining the 
*     supplied pixel positions.  Details of the B array are passed in 
*     common from ARD1_KDRAW.

*  Arguments:
*     N = INTEGER (Given)
*        The number of points in the polyline.
*     X( N ) = REAL (Given)
*        The first axis pixel coordinate values.
*     Y( N ) = REAL (Given)
*        The second axis pixel coordinate values.

*  Returned Value:
*     One for success. Zero for failure.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUN-2001 (DSB):
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

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_MSKSC = INTEGER (Write)
*           The MSKSIZ value passed to the ARD "drawing" routines.
*        CMN_RNDXC = INTEGER (Write)
*           The RINDEX value passed to the ARD "drawing" routines.
*        CMN_IPBC = INTEGER (Read)
*           Pointer to the B array passed to the ARD "drawing" routines.
*        CMN_LBNDC( 2 ) = INTEGER (Read)
*           The lower bounds of the B array passed to the ARD "drawing" 
*           routines.
*        CMN_UBNDC( 2 ) = INTEGER (Read)
*           The upper bounds of the B array passed to the ARD "drawing" 
*           routines.
*        CMN_LBIBC( 2 ) = INTEGER (Read)
*           The lower bounds of the interior bounding box passed to the
*           ARD "drawing" routines.
*        CMN_UBIBC( 2 ) = INTEGER (Read)
*           The upper bounds of the interior bounding box passed to the
*           ARD "drawing" routines.

*  Arguments Given:
      INTEGER N
      REAL X( N )
      REAL Y( N )

*  Local Variables:
      INTEGER I, STATUS
      DOUBLE PRECISION PC( 4 )      

*  Initialize the returned value to indicate success.
      ARD1_GLINE = 1
      STATUS = SAI__OK 

*  Loop round each line segment in the supplied polycurve, drawing each
*  one. 
      DO I = 2, N

         PC( 1 ) = X( I - 1 )
         PC( 2 ) = Y( I - 1 )
         PC( 3 ) = X( I )
         PC( 4 ) = Y( I )

         CALL ARD1_LINFL( CMN_RNDXC, 2, CMN_LBNDC, CMN_UBNDC, CMN_MSKSC, 
     :                    4, PC, %VAL( CMN_IPBC ), CMN_LBIBC, CMN_UBIBC, 
     :                    STATUS )

      END DO

      IF( STATUS .NE. SAI__OK ) ARD1_GLINE = 0

      END
