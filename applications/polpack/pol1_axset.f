      SUBROUTINE POL1_AXSET( TR, NX, NY, CENX, CENY, STATUS )
*+
*  Name:
*     POL1_AXSET

*  Purpose:
*     Store linear 2D axis centre values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_AXSET( TR, NX, NY, CENX, CENY, STATUS )

*  Description:
*     This routine stores linearly increasing values values in a pair of
*     1D arrays.

*  Arguments:
*     TR( 4 ) = _REAL (Given)
*        The coefficients of the transformation which converts cell indices
*        into (X,Y) values to be stored in the catalogue (if required). 
*           X = TR( 1 ) + TR( 2 )*REAL( IPIX )  ( IPIX = 1, NX )
*           Y = TR( 3 ) + TR( 4 )*REAL( IROW )  ( IROW = 1, NY )
*     NX = INTEGER (Given)
*        The number of points in CENX.
*     NY = INTEGER (Given)
*        The number of points in CENY.
*     CENX( NX ) = REAL (Returned)
*        The X axis centre values.
*     CENY( NY ) = REAL (Returned)
*        The Y axis centre values.
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
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL TR( 4 )
      INTEGER NX
      INTEGER NY

*  Arguments Returned:
      REAL CENX( NX )
      REAL CENY( NY )

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

      END
