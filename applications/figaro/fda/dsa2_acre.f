      SUBROUTINE DSA2_ACRE( NDF, STATUS )
*+
*  Name:
*     DSA2_ACRE

*  Purpose:
*     Sets up default axis centres for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA2_ACRE( NDF, STATUS )

*  Description:
*     This routine assigns default axis-centre arrays for an NDF.
*     The default centres are either the Starlink standard (SSN/22),
*     where the co-ordinates go from the lower bound - 0.5 to the upper
*     bound - 0.5, or the pixel indices between the lower and upper
*     bounds.  The selection is made via a call to DSA2_PIXIN.  The data
*     type of the axis-centre arrays is _REAL.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF, whose axis-centre arrays are
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 July 9 (MJC):
*        Original version.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL                 ! Number of elements in the array
      LOGICAL EXIST              ! Axis centres already exist?
      INTEGER IAXIS              ! Axis number
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NDIM               ! Number of dimensions in the NDF
      LOGICAL PIXIND             ! Fill centres with pixel indices?
      INTEGER PNTR( 1 )          ! Pointer to an axis-centre array
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Decide how to fill the axis centres.
      CALL DSA2_PIXIN( PIXIND, STATUS )

*  See if the axis structure exists.
      CALL NDF_STATE( NDF, 'Axis', EXIST, STATUS )

*  Where there is no axis structure and pixel indices are required,
*  create axis centres with pixel-index co-ordinates.
      IF ( PIXIND .AND. .NOT. EXIST ) THEN

*  Obtain the axis bounds.
         CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Loop for each axis.
         DO 10 IAXIS = 1, NDIM

*  Map the axis-centre array for writing.
            CALL NDF_AMAP( NDF, 'Centre', IAXIS, '_REAL', 'WRITE',
     :                     PNTR, EL, STATUS )

*  Fill the axis-centre array with pixel indices.
            CALL DSA2_IFILLF( EL, LBND, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                        STATUS )

*  Unmap the axis-centre array.
            CALL NDF_AUNMP( NDF, 'Centre', IAXIS, STATUS )
   10    CONTINUE

*  Use the Starlink standard axis centres.
      ELSE
         CALL NDF_ACRE( NDF, STATUS )
      END IF

      END
