      SUBROUTINE POL1_PRWCS( INDF1, INDF2, STATUS )
*+
*  Name:
*     POL1_PRWCS

*  Purpose:
*     Calculates polarisation parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL POL1_PRWCS( INDF1, INDF2, STATUS )

*  Description:
*     This routine copies WCS information from the input 3-D NDF to the
*     output 2-D NDF.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        The 3-D input NDF.
*     INDF2 = INTEGER (Given)
*        The 2-D output NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of second NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of second NDF
      INTEGER NDIM               ! No. of axes in second NDF
      INTEGER INDF1A             ! Identifier for matching section
      INTEGER IWCS               ! AST identifier for WCS Frameset
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds of the second NDF.
      CALL NDF_BOUND( INDF2, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Get a section from the first NDF which has bounds matching the
*  the second NDF.
      CALL NDF_SECT( INDF1, NDIM, LBND, UBND, INDF1A, STATUS )

*  Copy the WCS information from this section to the second NDF.
      CALL KPG1_GTWCS( INDF1A, IWCS, STATUS )
      CALL NDF_PTWCS( IWCS, INDF2, STATUS )

      END
