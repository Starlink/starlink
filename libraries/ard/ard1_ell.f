      SUBROUTINE ARD1_ELL( RINDEX, LBND1, UBND1, LBND2, UBND2, NPAR,
     :                     PAR, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :                     STATUS )
*+
*  Name:
*     ARD1_ELL

*  Purpose:
*     Initialise an array to hold a ELLIPSE region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_ELL( RINDEX, LBND1, UBND1, LBND2, UBND2, NPAR, PAR, B,
*                    LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values are then assigned to the points specified by the
*     supplied parameters. The supplied parameters are the co-efficients
*     of the 2-D linear transformation from user co-ordinates to pixel 
*     co-ordinates, followed by the user co-ordinates of the ellipse 
*     centre, the lengths of the two ellipse axes (in user co-
*     ordinates), and the angle from the user X axis to the first 
*     ellipse axis (in degrees, measured +ve from user +X to user +Y).

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
*     PAR( NPAR ) = REAL (Given)
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
*     11-APR-1994 (DSB):
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
      INTEGER RINDEX
      INTEGER LBND1
      INTEGER UBND1
      INTEGER LBND2
      INTEGER UBND2
      INTEGER NPAR
      REAL PAR( NPAR )

*  Arguments Given and Returned:
      INTEGER B( LBND1:UBND1, LBND2:UBND2 )
      INTEGER LBEXTB( 2 )
      INTEGER UBEXTB( 2 )
      INTEGER LBINTB( 2 )
      INTEGER UBINTB( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL
     :  AA1,                     ! First ellipse axis projected on Xp
     :  AA2,                     ! First ellipse axis projected on Yp
     :  ANG1,                    ! An angle 
     :  ANG2,                    ! An angle 
     :  BB1,                     ! Second ellipse axis projected on Xp
     :  BB2,                     ! Second ellipse axis projected on Yp
     :  SINA,                    ! Sine of supplied orientation
     :  COSA,                    ! Cosine of supplied orientation
     :  T,                       ! Temporary real storage.
     :  X0,                      ! X pixel co-ordinate at ellipse centre
     :  X1                       ! X pixel coord at lower intersection

      REAL
     :  X2,                      ! X pixel coord at upper intersection
     :  XRANGE,                  ! Half range of X pixel co-ordinates
     :  Y,                       ! Y pixel coordinate at row centre
     :  Y0,                      ! Y pixel co-ordinate at ellipse centre
     :  YHI,                     ! High limit of Y on ellipse
     :  YLO,                     ! Low limit of Y on ellipse
     :  YRANGE                   ! Half range of Y pixel co-ordinates

      INTEGER
     :  IX,                      ! X pixel index of current column
     :  IX1,                     ! Lower X pixel index limit
     :  IX2,                     ! Upper X pixel index limit
     :  IY,                      ! Y pixel index of current row
     :  LBND( 2 ),               ! Mask lower bounds
     :  MSKSIZ,                  ! No. of elements in mask
     :  UBND( 2 )                ! Mask upper bounds
  
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if less than 11 parameters have been supplied.
      IF( NPAR .LT. 11 ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL ERR_REP( 'ARD1_ELL_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for ARD1_ELL (programming '//
     :                 'error).', STATUS )
         GO TO 999
      END IF

*  Reset all pixels within the interior bounding box so that they
*  hold exterior values. The pixels outside the interior bounding box
*  already hold exterior values.
      LBND( 1 ) = LBND1
      UBND( 1 ) = UBND1
      LBND( 2 ) = LBND2
      UBND( 2 ) = UBND2
      MSKSIZ = ( UBND1 - LBND1 + 1 )*( UBND2 - LBND2 + 1 )

      CALL ARD1_BXSET( 2, LBND, UBND, MSKSIZ, 0, LBINTB,
     :                 UBINTB, B, STATUS )

*  Initialise the x-axis bounds of the interior bounding box.
      LBINTB( 1 ) = VAL__MAXI
      UBINTB( 1 ) = VAL__MINI

*  Find the pixel co-ordinates of the ellipse centre.
      X0 = PAR( 1 ) + PAR( 2 )*PAR( 7 ) + PAR( 3 )*PAR( 8 )
      Y0 = PAR( 4 ) + PAR( 5 )*PAR( 7 ) + PAR( 6 )*PAR( 8 )

*  Store the sine and cosine of the orientation of the ellipse in user 
*  co-ordinates.
      SINA = SIN( PAR( 11 )*ARD__DTOR )
      COSA = COS( PAR( 11 )*ARD__DTOR )

*  Find the half-ranges of pixel co-ordinates encompassed by the
*  ellipse.
      AA1 = PAR( 9 )*( PAR( 2 )*COSA + PAR( 3 )*SINA )
      BB1 = PAR( 10 )*( PAR( 3 )*COSA - PAR( 2 )*SINA )
      XRANGE = SQRT( MAX( 0.0, AA1**2 + BB1**2 ) )

      AA2 = PAR( 9 )*( PAR( 5 )*COSA + PAR( 6 )*SINA )
      BB2 = PAR( 10 )*( PAR( 6 )*COSA - PAR( 5 )*SINA )
      YRANGE = SQRT( MAX( 0.0, AA2**2 + BB2**2 ) )

*  Abort if the ellipse is actually a line.
      IF( XRANGE .LE. 0.0 .OR. YRANGE .LE. 0.0 ) THEN
         STATUS = ARD__INTER
         CALL ERR_REP( 'ARD1_ELL_ERR2', 'Null ELLIPSE region '//
     :                 'encountered in ARD description.', STATUS )
         GO TO 999
      END IF

*  Find an angle needed later.
      ANG2  = ATAN2( AA1, BB1 ) - ATAN2( AA2, BB2 )

*  Find the maximum and minimum Y pixel co-ordinates covered by the
*  ellipse.
      YHI = Y0 + YRANGE
      YLO = Y0 - YRANGE

*  Convert these values to pixel indices.
      LBINTB( 2 ) = INT( YLO ) - 1
      UBINTB( 2 ) = INT( YHI ) + 1

*  Limit them to the bounds of the array.
      LBINTB( 2 ) = MAX( LBND2, LBINTB( 2 ) )
      UBINTB( 2 ) = MIN( UBND2, UBINTB( 2 ) )

*  Loop round the range of rows covered by the ellipse.
      DO IY = LBINTB( 2 ), UBINTB( 2 )

*  Find the displacement from the ellipse centre to the centre of the 
*  current row.
         Y = REAL( IY ) - 0.5 - Y0

*  See if this row intersects the ellipse.
         IF( ABS( Y ) .LE. YRANGE ) THEN 

*  If so, find the X pixel co-ordinate values at which the row 
*  intersects the ellipse.
            ANG1 = ASIN( Y/YRANGE )
            X1 = XRANGE*SIN( ANG1 + ANG2 ) + X0
            X2 = XRANGE*SIN( ANG1 - ANG2 ) + X0

*  Ensure that X2 is not less than X1.
            IF( X2 .LT. X1 ) THEN
               T = X2
               X2 = X1
               X1 = T
            END IF

*  Find the corresponding pixel indices. IX1 is the lower pixel index
*  bound and IX2 is the upper pixel index bound. 
            T = X1 + 0.5
            IX1 = INT( T )
            IF( T .GT. 0.0 .AND. REAL( IX1 ) .NE. T ) IX1 = IX1 + 1

            T = X2 + 0.5
            IX2 = INT( T )
            IF( T .LT. 0.0 .AND. REAL( IX2 ) .NE. T ) IX2 = IX2 - 1

*  Limit them to the bounds of the mask.
            IX1 = MAX( IX1, LBND1 )
            IX2 = MIN( IX2, UBND1 )

*  Update the X bounds of the internal bounding box.
            LBINTB( 1 ) = MIN( LBINTB( 1 ), IX1 )
            UBINTB( 1 ) = MAX( UBINTB( 1 ), IX2 )

*  Assign the supplied value to all pixels on the current row between
*  the pixel index bounds just found. 
            DO IX = IX1, IX2
               B( IX, IY ) = RINDEX               
            END DO

         END IF

      END DO

*  If the interior bounding box is null, return the usual value
*  (VAL__MINI for LBINTB( 1 ) ).
      IF( LBINTB( 1 ) .GT. UBINTB( 1 ) .OR. 
     :    LBINTB( 2 ) .GT. UBINTB( 2 ) ) LBINTB( 1 ) = VAL__MINI

*  Ensure the the exterior bounding box is returned "infinite".
      LBEXTB( 1 ) = VAL__MAXI

*  Jump to here if an error occurs.
 999  CONTINUE

      END
