      SUBROUTINE TRN_GTNVC( IDT, NVIN, NVOUT, STATUS )
*+
*  Name:
*     TRN_GTNVC

*  Purpose:
*     get numbers of compiled variables.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_GTNVC( IDT, NVIN, NVOUT, STATUS )

*  Description:
*     The routine obtains the numbers of input and output variables from
*     a compiled transformation passed by identifier.

*  Arguments:
*     IDT = INTEGER (given)
*        Identifier for the compiled transformation.
*     NVIN = INTEGER (returned)
*        Number of input variables.
*     NVOUT = INTEGER (returned)
*        Number of output variables.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Ensure the TRANSFORM facility is active.
*     - Obtain the compiled transformation table (CTT) slot number from
*       the identifier supplied, validating the identifier in the
*       process.
*     - Extract the numbers of variables information from the
*       appropriate CTT slot.

*  Authors:
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Arguments Given:
      INTEGER IDT               ! Identifier for the compiled
                                ! transformation


*  Arguments Returned:
      INTEGER NVIN              ! Number of input variables

      INTEGER NVOUT             ! Number of output variables


*  Status:
      INTEGER STATUS            ! Error status


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Local Variables:
      INTEGER SLOT              ! CTT slot number


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Ensure the TRANSFORM facility is active.
      CALL TRN1_SETUP( .TRUE., STATUS )


*   Import the identifier, obtaining the compiled transformation table
*   (CTT) slot number.
      CALL TRN1_IMPID( IDT, %VAL( CNF_PVAL( TRN_PCTTI ) ), SLOT,
     :                 STATUS )

*   Obtain the numbers of variables information from the CTT slot.
      CALL TRN1_GTNVS( SLOT, %VAL( CNF_PVAL( TRN_PCTTI ) ), NVIN, NVOUT,
     :                 STATUS )

*   Exit routine.
      END
