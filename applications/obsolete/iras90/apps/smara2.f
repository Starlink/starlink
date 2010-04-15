      SUBROUTINE SMARA2( NPNT, X, Y, MXMNSZ, NDF, MKSIZ, STATUS )
*+
*  Name:
*     SMARA2

*  Purpose:
*     Calculate the sizes of the marks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SMARA2( NPNT, X, Y, MXMNSZ, NDF, MKSIZE, STATUS )

*  Description:
*     This subroutine calculate the size of the marks so that is varies
*     according to the image data value at the mark positions within the
*     given upper and lower limit.

*  Arguments:
*     NPNT = INTEGER (Given)
*        Number of positions to be marked.
*     X( NPNT ), Y( NPNT ) = DOUBLE PRECISION (Given)
*        The positions to be marked.
*     MXMNSZ( 2 ) = REAL (Given)
*        The max and min mark size.
*     NDF = INTEGER (Given)
*        The ID of the NDF underlying the current image.
*     MKSIZ( NPNT ) = REAL (Returned)
*        The size of each mark.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     25-JAN-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPNT
      DOUBLE PRECISION X( NPNT ), Y( NPNT )
      REAL MXMNSZ( 2 )
      INTEGER NDF

*  Arguments Returned:
      REAL MKSIZ( NPNT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PNTR( 1 )          ! Pointer to the mapped NDF data array
      INTEGER EL                 ! Number of elements in the data array
      INTEGER LBND( 2 ), UBND( 2 ) ! Bounds of the NDF data array
      INTEGER NDIM               ! Number of the data array dimension


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Map the underlying NDF.
      CALL NDF_MAP( NDF, 'Data', '_REAL', 'READ', PNTR, EL, STATUS )

*  Get the bounds of the NDF.
      CALL NDF_BOUND( NDF, 2, LBND, UBND, NDIM, STATUS )

*  Find the size of the marker for each point to be marked.
      CALL SMARB0( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ),
     :            %VAL( PNTR( 1 ) ), NPNT, X, Y, MXMNSZ, MKSIZ, STATUS )

*  Unmap the NDF.
      CALL NDF_UNMAP( NDF, 'Data', STATUS )

      END
