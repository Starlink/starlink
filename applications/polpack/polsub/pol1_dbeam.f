      SUBROUTINE POL1_DBEAM( IGRP1, DBEAM, STATUS )
*+
*  Name:
*     POL1_DBEAM

*  Purpose:
*     See if all input NDFs contain dual-beam data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_DBEAM( IGRP1, DBEAM, STATUS )

*  Description:
*     This routine returns DBEAM = .TRUE. if all the NDFs supplied in
*     the given group contain dual-beam data (indicated by a leagl RAY item
*     in their POLPACK extensions). If any do not, then .FALSE. is
*     returned.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the group containing the input NDF names.
*     DBEAM = LOGICAL (Returned)
*        Do all NDFs contain dual-beam data?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-FEB-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants

*  Arguments Given:
      INTEGER IGRP1

*  Arguments Returned:
      LOGICAL DBEAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER XLOC*(DAT__SZLOC)! POLPACK extension locator
      INTEGER I                  ! Index of current input NDF
      INTEGER INDF               ! NDF identifier for the current input NDF
      INTEGER NNDF               ! No. of input NDFs
      LOGICAL THERE              ! Does item exist?
*.

*  Initialise the returned value.
      DBEAM = .TRUE.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the number of input NDFs.
      CALL GRP_GRPSZ( IGRP1, NNDF, STATUS )

*  Loop round each NDF.
      DO I = 1, NNDF

*  Get the current NDF identifier.
         CALL NDG_NDFAS( IGRP1, I, 'READ', INDF, STATUS )

*  See if the NDF has a POLPACK extension. If not, return .FALSE.
         CALL NDF_XSTAT( INDF, 'POLPACK', THERE, STATUS )
         IF( .NOT. THERE ) THEN
            DBEAM = .FALSE.
            GO TO 999
         END IF

*  Get a locator to the POLPACK extension.
         CALL NDF_XLOC( INDF, 'POLPACK', 'READ', XLOC, STATUS )

*  See if this NDF contains dual-beam data, indicated by a RAY item
*  being present.
         CALL DAT_THERE( XLOC, 'RAY', THERE, STATUS )
         IF( .NOT. THERE ) THEN
            DBEAM = .FALSE.
            GO TO 999
         END IF

*  Annul the locator to the POLPACK extension.
         CALL DAT_ANNUL( XLOC, STATUS )

*  Annul the current NDF identifier.
         CALL NDF_ANNUL( INDF, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Tidy up.
*  ========

 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
