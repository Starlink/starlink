      SUBROUTINE NDG_NDFAS( IGRP, INDEX, MODE, INDF, STATUS )
*+
*  Name:
*     NDG_NDFAS

*  Purpose:
*     Obtain an NDF identifier for an existing NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_NDFAS( IGRP, INDEX, MODE, INDF, STATUS )

*  Description:
*     The routine returns an NDF identifier for an existing NDF. The 
*     name of the NDF is held at a given index within a given group.
*     It is equivalent to NDF_ASSOC.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding the names of NDFs. This
*        will often be creted using NDG_ASSOC, but groups created "by 
*        hand" using GRP directly (i.e. without the supplemental groups
*        created by NDG_ASSOC) can also be used.
*     INDEX = INTEGER (Given)
*        The index within the group at which the name of the NDF to be
*        accessed is stored.
*     MODE = CHARACTER * ( * ) (Given)
*        Type of NDF access required: 'READ', 'UPDATE' or 'WRITE'.
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOID
*     constant is defined in the include file NDF_PAR.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1992 (DSB):
*        Original version.
*     29-AUG-1997 (DSB):
*        Updated to work with automatic NDF data conversion.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDG_CONST'        ! NDG constants.

*  Arguments Given:
      INTEGER IGRP
      INTEGER INDEX
      CHARACTER MODE*(*)

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM)! NDF file specification
      INTEGER PLACE              ! Dummy NDF place holder
*.

*  Set an initial value for the INDF argument.
      INDF = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( NDG__UCASE ) CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  Get the required name.
      CALL GRP_GET( IGRP, INDEX, 1, NAME, STATUS )

*  If the name could not be obtained, set the name blank and abort.     
      IF ( STATUS .NE. SAI__OK ) THEN
         NAME = ' '
         GO TO 999
      END IF

*  Open the NDF.
      CALL NDF_OPEN( DAT__ROOT, NAME, MODE, 'OLD', INDF, PLACE, STATUS )

*  If an error occured, add context information.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN

         IF( NAME .NE. ' ' ) THEN
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'NDG_NDFAS_ERR1', 'Unable to get an NDF '//
     :                    'identifier for ''^NAME''', STATUS )
         ELSE
            CALL ERR_REP( 'NDG_NDFAS_ERR2', 'Unable to get an NDF '//
     :                    'identifier for an existing data set.', 
     :                    STATUS )
         END IF

      END IF

      END
