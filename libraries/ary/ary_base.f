      SUBROUTINE ARY_BASE( IARY1, IARY2, STATUS )
*+
*  Name:
*     ARY_BASE

*  Purpose:
*     Obtain an identifier for a base array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_BASE( IARY1, IARY2, STATUS )

*  Description:
*     The routine returns an identifier for the base array with which
*     an array section is associated.

*  Arguments:
*     IARY1 = INTEGER (Given)
*        Identifier for an existing array section (the routine will also
*        work if this is already a base array).
*     IARY2 = INTEGER (Returned)
*        Identifier for the base array with which the section is
*        associated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of
*     ARY__NOID will be returned for the IARY2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The ARY__NOID
*     constant is defined in the include file ARY_PAR.

*  Algorithm:
*     -  Set an initial value for the IARY2 argument before checking the
*     inherited status.
*     -  Import the input array identifier.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Create a new base array entry in the ACB to describe it.
*     -  Transfer the access control flags from the old ACB entry to the
*     new one.
*     -  Export an identifier for the new base array.
*     -  If an error occurred, then annul the new ACB entry.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-OCT-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_ACC( ARY__MXACC, ARY__MXACB ) = LOGICAL (Read and Write)
*           Access control flags.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IARY1

*  Arguments Returned:
      INTEGER IARY2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB1              ! Index to input array entry in ACB
      INTEGER IACB2              ! Index to output array entry in ACB
      INTEGER IACC               ! Loop counter for access control flags
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Set an initial value for the IARY2 argument.
      IARY2 = ARY__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the input array identifier.
      CALL ARY1_IMPID( IARY1, IACB1, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB1 )

*  Create a new base array entry in the ACB to describe it.
         CALL ARY1_CRNBA( IDCB, IACB2, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Transfer the access control flags from the old ACB entry to the new
*  one.
            DO 1 IACC = 1, ARY__MXACC
               ACB_ACC( IACC, IACB2 ) = ACB_ACC( IACC, IACB1 )
1           CONTINUE

*  Export an identifier for the new base array.
            CALL ARY1_EXPID( IACB2, IARY2, STATUS )

*  If an error occurred, then annul the new ACB entry.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ARY1_ANL( IACB2, STATUS )
            END IF
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_BASE_ERR',
     :   'ARY_BASE: Error obtaining identifier for a base array.',
     :   STATUS )
         CALL ARY1_TRACE( 'ARY_BASE', STATUS )
      END IF

      END
