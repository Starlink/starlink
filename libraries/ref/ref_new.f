       SUBROUTINE REF_NEW( ELOC, CNAME, STATUS )
*+
*  Name:
*     REF_NEW

*  Purpose:
*     Create a new reference object.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL REF_NEW( ELOC, CNAME, STATUS )

*  Description:
*     This routine creates a reference object as a component of a
*     specified structure.  If the component already exists, an error
*     is reported.

*  Arguments:
*     ELOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        A locator associated with the structure which is to contain
*        the reference object.
*     CNAME = CHARACTER * ( DAT__SZNAM ) (Given)
*        The name of the component to be created in the structure
*        located by ELOC.
*     STATUS = INTEGER (Given and Returned)
*        Inherited global status.

*  Authors:
*     SLW: Sid Wright (UCL)
*     AJC: A.J. Chipperfield (STARLINK, RAL)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     24-APR-1982 (SLW):
*        Original version.
*     16-FEB-1987 (AJC):
*        Expanded REFERENCE_OBJ structure.
*     13-JAN-1988 (AJC):
*        Output error message.
*     21-JAN-1988 (AJC):
*        Improve prologue.
*     20-FEB-1992 (RFWS):
*        Standardise prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'		 ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'REF_PAR'		 ! REF_ public constants

*  Arguments Given:
      CHARACTER * ( * ) ELOC
      CHARACTER * ( * ) CNAME

*  Status:
      INTEGER STATUS		 ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) TLOC ! Locator to reference component
      CHARACTER * ( DAT__SZTYP ) TYPE ! Type of reference container

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the new component.
      CALL DAT_NEW( ELOC, CNAME, 'REFERENCE_OBJ', 0, 0, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Component created. Obtain a locator for it.
         CALL DAT_FIND( ELOC, CNAME, TLOC, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Create the FILE component.
            CALL DAT_CCTYP( REF__SZREF, TYPE )
            CALL DAT_NEW( TLOC, 'FILE', TYPE, 0, 0, STATUS )

*  Create the PATH component.
            CALL DAT_NEW( TLOC, 'PATH', TYPE, 0, 0, STATUS )

*  Annul the temporary locator.
            CALL DAT_ANNUL( TLOC, STATUS )
         ENDIF
      ENDIF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_REP( 'REF_NEW_ERR',
     :   'REF_NEW: Error creating a new reference object.', STATUS )
      END IF

      END
