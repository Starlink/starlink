      SUBROUTINE USR_MODIMAGE( STATUS )
*+
*  Name:
*     SUBROUTINE USR_MODIMAGE

*  Description:
*     The image is modified using the image display cursor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_MODIMAGE( STATUS )

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
*     15-DEC-88 (PCTR):
*       IUEDR Vn. 2.0
*     24-OCT-94 (MJC):
*       IUEDR Vn. 3.2
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
      INTEGER STATUS   ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'

*  Local Variables:
      INTEGER DATA_VM  ! VM address for DATA array.
      INTEGER NAXIS1   ! Size of axis 1.
      INTEGER NAXIS2   ! Size of axis 2.
      INTEGER QUAL_VM  ! VM address for QUAL array.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration and Image.
      CALL DASSOC( 'I\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )

      ELSE

*      Map image.
         CALL MRDATA( NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: accessing image data\\', STATUS )

         ELSE
            CALL MODIM( NAXIS1, NAXIS2, %VAL( DATA_VM ),
     :                  %VAL( QUAL_VM ), STATUS )
         END IF
      END IF

      END
