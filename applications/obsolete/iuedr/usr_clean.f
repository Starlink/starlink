      SUBROUTINE USR_CLEAN( STATUS )
*+
*  Name:
*     SUBROUTINE USR_CLEAN

*  Description:
*     Marks all pixels in an image which fall below the
*     supplied threshold value BAD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_CLEAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
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

*  Local Variables:
      REAL*8 THRESH     ! Cleaning threshold pixel value.

      INTEGER ACTVAL    ! Parameter value count.
      INTEGER DATA_VM   ! VM address for DATA array.
      INTEGER NAXIS1    ! Size of axis 1.
      INTEGER NAXIS2    ! Size of axis 2.
      INTEGER QUAL_VM   ! VM address for QUAL array.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration and Image.
      CALL DASSOC( 'I\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\',
     :                STATUS )
         GO TO 999
      END IF

*  Map image.
      CALL MRDATA( NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: unable to access data\\', STATUS )
         GO TO 999
      END IF

*  Get THRESHold value.
      CALL RDPARF( 'THRESH\\', .FALSE., 1, THRESH, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'THRESH\\', STATUS )
         GO TO 999
      END IF

*  Apply threshold rule to image.
      CALL CLEAN( NAXIS1, NAXIS2, %VAL( DATA_VM ), %VAL( QUAL_VM ),
     :            THRESH )

 999  CONTINUE

      END
