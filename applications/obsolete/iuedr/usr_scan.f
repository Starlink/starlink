      SUBROUTINE USR_SCAN( STATUS )
*+
*  Name:
*     SUBROUTINE USR_SCAN

*  Function:
*     The Data Array is scanned in a way that will provide information
*     on the location of spectra on it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_SCAN( STATUS )

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
*     16-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     08-JUL-94 (MJC):
*       IUEDR Vn. 3.1-1
*     18-JAN-95 (MJC):
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

*  Local Variables:
      INTEGER NAXIS1        ! Size of axis 1.
      INTEGER NAXIS2        ! Size of axis 2.
      INTEGER DATA_VM       ! VM address for DATA array.
      INTEGER QUAL_VM       ! VM address for QUAL array.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration and Image.
      CALL DASSOC( 'I\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*  Map data.
      CALL MRDATA( NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: unable to access data\\', STATUS )
         GO TO 999
      END IF

*  Branch on RESOLUTION.
      IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
         CALL SCANHI( NAXIS1, NAXIS2, %VAL( DATA_VM ), %VAL( QUAL_VM ),
     :                STATUS )

      ELSE IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
         CALL SCANLO( NAXIS1, NAXIS2, %VAL( DATA_VM ), %VAL( QUAL_VM ),
     :                STATUS )

      ELSE
         CALL ERROUT( 'Error: unknown IUE Resolution\\', STATUS )
      END IF

 999  CONTINUE

      END
