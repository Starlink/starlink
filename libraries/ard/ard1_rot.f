      SUBROUTINE ARD1_ROT( RINDEX, LBND1, UBND1, LBND2, UBND2, NPAR,
     :                     D, PAR, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :                     STATUS )
*+
*  Name:
*     ARD1_ROT

*  Purpose:
*     Initialise an array to hold a ROTBOX region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_ROT( RINDEX, LBND1, UBND1, LBND2, UBND2, NPAR, D, PAR, B, 
*                    LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The supplied parameters are modified so that they look the same
*     as those for a POLYGON region. ARD1_POL is then called to load the
*     region.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     LBND1 = INTEGER (Given)
*        The lower pixel index bounds of the B array on the first axis.
*     UBND1 = INTEGER (Given)
*        The upper pixel index bounds of the B array on the first axis.
*     LBND2 = INTEGER (Given)
*        The lower pixel index bounds of the B array on the second axis.
*     UBND2 = INTEGER (Given)
*        The upper pixel index bounds of the B array on the second axis.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     D( 6 ) = DOUBLE PRECISION (Given)
*        The coefficients of the user->pixel mapping. The mapping is:
*        P1 = D0 + D1*U1 + D2*U2 
*        P2 = D3 + D4*U1 + D5*U2 
*     PAR( NPAR ) = DOUBLE PRECISION (Given and Returned)
*        Region parameters. 
*     B( LBND1:UBND1, LBND2:UBND2 ) = INTEGER (Given and Returned)
*        The array.
*     LBEXTB( 2 ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        returned to indicate an "infinite" box. Other elements should
*        be ignored.
*     UBEXTB( 2 ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B. The returned values should be ignored
*        since the box is "infinite".
*     LBINTB( 2 ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( 2 ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1994 (DSB):
*        Original version.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
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
      INTEGER RINDEX
      INTEGER LBND1
      INTEGER UBND1
      INTEGER LBND2
      INTEGER UBND2
      INTEGER NPAR
      DOUBLE PRECISION D( 6 )
      DOUBLE PRECISION PAR( NPAR )

*  Arguments Given and Returned:
      INTEGER B( LBND1:UBND1, LBND2:UBND2 )
      INTEGER LBEXTB( 2 )
      INTEGER UBEXTB( 2 )
      INTEGER LBINTB( 2 )
      INTEGER UBINTB( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION 
     :    COST,                  ! COS of the rotation angle
     :    LPAR( 8 ),             ! Local parameters
     :    SINT                   ! SIN of the rotation angle
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the number of supplied parameters is wrong.
      IF( NPAR .NE. 5 ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL ERR_REP( 'ARD1_ROT_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for a ROTBOX region in '//
     :                 'ARD1_ROT (programming error).', STATUS )
         GO TO 999
      END IF

*  Save commonly used values.
      SINT = SIN( PAR( 5 ) * ARD__DTOR )
      COST = COS( PAR( 5 ) * ARD__DTOR )

*  Set up the user coordinates of the first corner of the box.
      LPAR( 1 ) = 0.5*( -PAR( 3 )*COST + PAR( 4 )*SINT ) + PAR( 1 )
      LPAR( 2 ) = 0.5*( -PAR( 4 )*COST - PAR( 3 )*SINT ) + PAR( 2 )

*  Now do the same with the second corner.
      LPAR( 3 ) = 0.5*( -PAR( 3 )*COST - PAR( 4 )*SINT ) + PAR( 1 )
      LPAR( 4 ) = 0.5*( PAR( 4 )*COST - PAR( 3 )*SINT ) + PAR( 2 )

*  And the third.
      LPAR( 5 ) = 0.5*( PAR( 3 )*COST - PAR( 4 )*SINT ) + PAR( 1 )
      LPAR( 6 ) = 0.5*( PAR( 4 )*COST + PAR( 3 )*SINT ) + PAR( 2 )

*  And the fourth.
      LPAR( 7 ) = 0.5*( PAR( 3 )*COST + PAR( 4 )*SINT ) + PAR( 1 )
      LPAR( 8 ) = 0.5*( -PAR( 4 )*COST + PAR( 3 )*SINT ) + PAR( 2 )

*  The parameters are now in the same format as those for a POLYGON region.
*  Call the subroutine used to load a POLYGON region.
      CALL ARD1_POL( RINDEX, LBND1, UBND1, LBND2, UBND2, 8,
     :               D, LPAR, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :               STATUS )

*  Jump here if an error occurs.
 999  CONTINUE

      END
