      SUBROUTINE USR_TRAK( STATUS )
*+
*  Name:
*     SUBROUTINE USR_TRAK

*  Description:
*     The Data Array is scanned in a way that will provide information
*     on the location of spectra on it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_TRAK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
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

*  Global Variables:
      INCLUDE 'CMHEAD'

*  Status:
      INTEGER STATUS        ! Global status.

*  External References:
      LOGICAL STR_SIMLR     ! Caseless string equality.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration and Image and Spectrum.
      CALL DASSOC( 'IS\\', 'TF\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*  Branch on RESOLUTION.
      IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
         CALL TRAKHI( STATUS )

      ELSE IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
         CALL TRAKLO( STATUS )

      ELSE
         CALL ERROUT( 'Error: unknown IUE Resolution\\', STATUS )
      END IF

 999  CONTINUE

      END
