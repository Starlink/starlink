      SUBROUTINE POL1_AXSET( GOTZ, TR, NX, NY, NZ, CENX, CENY, CENZ, 
     :                       STATUS )
*+
*  Name:
*     POL1_AXSET

*  Purpose:
*     Store linear 2 or 3D axis centre values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_AXSET( GOTZ, TR, NX, NY, NZ, CENX, CENY, CENZ, STATUS )

*  Description:
*     This routine stores linearly increasing values values in 3 or 3
*     1D arrays.

*  Arguments:
*     GOTZ = LOGICAL (Given)
*        If .TRUE. then the data is 3D. Otherwise it is 2D.
*     TR( 6 ) = REAL (Given)
*        The coefficients of the transformation which converts cell indices
*        into (X,Y) values to be stored in the catalogue (if required). 
*           X = TR( 1 ) + TR( 2 )*REAL( IPIX )  ( IPIX = 1, NX )
*           Y = TR( 3 ) + TR( 4 )*REAL( IROW )  ( IROW = 1, NY )
*           Z = TR( 5 ) + TR( 6 )*REAL( IZ )    ( IZ = 1, NZ )
*     NX = INTEGER (Given)
*        The number of points in CENX.
*     NY = INTEGER (Given)
*        The number of points in CENY.
*     NZ = INTEGER (Given)
*        The number of points in CENZ.
*     CENX( NX ) = REAL (Returned)
*        The X axis centre values.
*     CENY( NY ) = REAL (Returned)
*        The Y axis centre values.
*     CENZ( NZ ) = REAL (Returned)
*        The Z axis centre values. Only accessed if GOTZ is .TRUE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-APR-1999 (DSB):
*        Original version.
*     7-FEB-2001 (DSB):
*        Modified to support 3D data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL GOTZ
      REAL TR( 6 )
      INTEGER NX
      INTEGER NY
      INTEGER NZ

*  Arguments Returned:
      REAL CENX( NX )
      REAL CENY( NY )
      REAL CENZ( NZ )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! loop index
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, NX
         CENX( I ) = TR( 1 ) + TR( 2 )*REAL( I ) 
      END DO

      DO I = 1, NY
         CENY( I ) = TR( 3 ) + TR( 4 )*REAL( I ) 
      END DO

      IF( GOTZ ) THEN 
         DO I = 1, NZ
            CENZ( I ) = TR( 5 ) + TR( 6 )*REAL( I ) 
         END DO
      END IF

      END
