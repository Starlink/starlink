      SUBROUTINE NDF1_IMPID( INDF, IACB, STATUS )
*+
*  Name:
*     NDF1_IMPID

*  Purpose:
*     Import an identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Description:
*     The routine converts an NDF identifier, previously issued by
*     NDF1_EXPID, into an index to the appropriate entry in the ACB.
*     The identifier value is fully checked and an error is reported if
*     it is not valid.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     IACB = INTEGER (Returned)
*        Index to an entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*      -  Set an initial value for the IACB argument before checking
*      the inherited status.
*     -  Decode the INDF value into an index for the ACB.
*     -  If a valid index was not obtained, then report an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Set an initial value for the IACB argument.
      IACB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the identifier to an ACB index.
      CALL NDF1_ID2AC( INDF, IACB )
      
*  If a valid index was not returned, then report an error.
      IF ( IACB .LE. 0 ) THEN
         STATUS = NDF__IDINV
         CALL MSG_SETI( 'INDF', INDF )
         CALL ERR_REP( 'NDF1_IMPID_IDIN',
     :   'NDF identifier invalid; its value is ^INDF (possible ' //
     :   'programming error).', STATUS )
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_IMPID', STATUS )

      END
