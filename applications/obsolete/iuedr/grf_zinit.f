      SUBROUTINE GRF_ZINIT( WKSTN, DEV, ZBASE, STATUS )
*+
*  Name:
*     SUBROUTINE GRF_ZINIT

*  Purpose:
*     Initialise a GKS workstation and return a base zone identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRF_ZINIT( WKSTN, DEV, ZBASE, STATUS )

*  Arguments:
*     WKSTN = CHARACTER*( * ) (Given)
*        SGS workstation name.
*     ZBASE = INTEGER (Returned)
*        SGS identifier for base zone.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     08-SEP-87 (PCTR):
*       IUEDR Vn. 2.0
*     09-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     19-SEP-94 (MJC):
*       IUEDR Vn. 3.1-4
*       SAEised
*     06-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER WKSTN*( * ) ! SGS workstation name.
      INTEGER DEV           ! Current SGS workstation ID.

*  Arguments Returned:
      INTEGER ZBASE         ! SGS workstation base zone.

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      INTEGER ZONID         ! Zone ID.
      INTEGER DEVNEW        ! New SGS workstation ID.
      INTEGER DTYPE         ! SGS connection identifier.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Determine new workstation ID.
      CALL SGS_WIDEN( WKSTN, DEVNEW, DTYPE, STATUS )

*   If new workstation exists - then proceed.
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Close old workstation if one exists.
         IF ( DEV .NE. 0 ) THEN
            CALL SGS_CLSWK( ZBASE, STATUS )
         END IF

*      Open new workstation.
         CALL SGS_OPNWK( WKSTN, ZONID, STATUS )

*      Trap SGS errors.
         IF ( STATUS .EQ. SAI__OK ) THEN

*         No error - then set base zone ID: ZBASE.
            ZBASE = ZONID

            DEV = DEVNEW
         END IF
      END IF

      END
