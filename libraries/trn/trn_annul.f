      SUBROUTINE TRN_ANNUL( IDT, STATUS )
*+
*  Name:
*     TRN_ANNUL

*  Purpose:
*     Annul compiled transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_ANNUL( IDT, STATUS )

*  Description:
*     The routine annuls the compiled transform associated with the
*     identifier supplied.  Resources associated with the compiled
*     transformation are released and the identifier is reset to
*     TRN__NOID.  This routine attempts to execute even if STATUS is
*     set on entry, although no error report will be made if it fails
*     under these circumstances.

*  Arguments:
*     IDT = INTEGER (Given & Returned)
*        Compiled transformation ID to be annulled.
*     STATUS = INTEGER (Given & Returned)
*        Inherited error status.

*  Algorithm:
*     - Mark the error stack.
*     - Ensure the TRANSFORM facility is active.
*     - Find the compiled transformation table (CTT) slot number
*       associated with the IDT value supplied.
*     - Release the CTT slot.
*     - Reset the IDT value to TRN__NOID.
*     - If an error occurred, decide whether an error report is
*       required.  If not, then annul it.
*     - Release the error stack.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     17-AUG-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Added handling of character string length when passing mapped
*        values (for Unix compatibility).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants


*  Arguments Given and Returned:
      INTEGER IDT               ! Compiled transformation ID


*  Status:
      INTEGER STATUS            ! Error status


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Local Variables:
      INTEGER LSTAT             ! Local error status

      INTEGER SLOT              ! CTT slot number for transformation


*.



*   Initialise the local status variable and mark the error stack.
      LSTAT = SAI__OK
      CALL ERR_MARK


*   Ensure the TRANSFORM facility is active.
      CALL TRN1_SETUP( .TRUE., LSTAT )


*   Get the compiled transformation table (CTT) slot number from the ID
*   supplied.
      CALL TRN1_IMPID( IDT, %VAL( CNF_PVAL( TRN_PCTTI ) ), SLOT, LSTAT )

*   If there is no error, release the CCT slot concerned.
      IF( LSTAT .EQ. SAI__OK )
     :  CALL TRN1_RELSL( SLOT, SLOT, %VAL( CNF_PVAL( TRN_PCTTI ) ),
     :                   %VAL( CNF_PVAL( TRN_PCTTL ) ), LSTAT,
     :                   %VAL( DAT__SZLOC ) )

*   If there is no error, reset the ID value.
      IF( LSTAT .EQ. SAI__OK ) THEN
        IDT = TRN__NOID


*   If an error was detected but STATUS was set on entry, then ignore
*   it and annul the error report.
      ELSE
        IF( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( LSTAT )


*   Otherwise, return the local status value and allow the error report
*   to stand.
        ELSE
          STATUS = LSTAT
        ENDIF
      ENDIF


*   Release the error stack.
      CALL ERR_RLSE


*   Exit routine.
      END
