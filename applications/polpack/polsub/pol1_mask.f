      SUBROUTINE POL1_MASK( LX, UX, LY, UY, MASK, NROW, X, Y, BADVAL,
     :                      LIST, NUMSEL, STATUS )
*+
*  Name:
*     POL1_GET2D

*  Purpose:
*     Get a list of vectors corresponding to good mask pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_MASK( LX, UX, LY, UY, MASK, NROW, X, Y, BADVAL,
*                     LIST, NUMSEL, STATUS )

*  Description:
*     This routine returns a list of the indices of vectors that correspond
*     to good pixels in the supplied mask array.

*  Arguments:
*     LX =  INTEGER (Given)
*        Lower pixel bound of MASK on X axis.
*     UX =  INTEGER (Given)
*        Upper pixel bound of MASK on X axis.
*     LY =  INTEGER (Given)
*        Lower pixel bound of MASK on Y axis.
*     UY =  INTEGER (Given)
*        Upper pixel bound of MASK on Y axis.
*     MASK( LX:UX, LY:UY ) =  REAL (Given)
*        The mask array. Selected pixels have a value greater than 0.
*     NROW =  INTEGER (Given)
*        Number of rows in X and Y columns.
*     X( NROW ) = REAL (Given)
*        The X column values.
*     Y( NROW ) = REAL (Given)
*        The Y column values.
*     LIST( NROW ) = INTEGER (Returned)
*        The returned list. The indices of selected rows are stored at
*        the start of this array.
*     BADVAL = INTEGER (Given)
*        The mask value used to flag unselected pixels.
*     NUMSEL = INTEGER (Returned)
*        The number of selected rows. This is the length of the used part
*        of the LIST array on exit.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory.

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     22-MAY-2018 (DSB):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants
      INCLUDE 'PRM_PAR'     ! VAL__ constants

*  Arguments Given:
      INTEGER LX
      INTEGER UX
      INTEGER LY
      INTEGER UY
      INTEGER MASK( LX:UX, LY:UY )
      INTEGER NROW
      REAL X( NROW )
      REAL Y( NROW )
      INTEGER BADVAL

*  Arguments Returned:
      INTEGER LIST( NROW )
      INTEGER NUMSEL

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IROW
      INTEGER IX
      INTEGER IY

*.

*  Initialise the returned value.
      NUMSEL = 0

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Loop over all rows.
      DO IROW = 1, NROW

*  Get pixel indices for the vector in this row.
         IX = NINT( X( IROW ) + 0.5 )
         IY = NINT( Y( IROW ) + 0.5 )

*  If the mask pixel is good, add the row top the returned list.
         IF( MASK( IX, IY ) .NE. BADVAL ) THEN
            NUMSEL = NUMSEL + 1
            LIST( NUMSEL ) = IROW
         END IF

      END DO

      END
