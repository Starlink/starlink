      SUBROUTINE USR_LBLS( STATUS )
*+
*  Name:
*     SUBROUTINE USR_LBLS

*  Description:
*     An LBLS data array is created, associated with a particular ORDER
*     or APERTURE, as appropriate.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_LBLS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Parameters:
*     DATASET:  dataset name

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS    ! Global status.

*  External References:
      LOGICAL STR_SIMLR ! Caseless string equality.

*  Global Variables:
      INCLUDE 'CMHEAD'
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration and Image.
      CALL DASSOC( 'I\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*   Branch on RESOLUTION.
      IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
         CALL PANHI( STATUS )

      ELSE IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
         CALL PANLO( STATUS )

      ELSE
         CALL ERROUT( 'Error: unknown IUE Resolution\\', STATUS )
      END IF

 999  CONTINUE

      END
