      SUBROUTINE NDF_BB( INDF, BADBIT, STATUS )
*+
*  Name:
*     NDF_BB

*  Purpose:
*     Obtain the bad-bits mask value for the quality component of an
*     NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_BB( INDF, BADBIT, STATUS )

*  Description:
*     The routine returns an unsigned byte value representing the
*     bad-bits mask associated with the quality component of an NDF.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     BADBIT = BYTE (Returned)
*        The unsigned byte bad-bits mask.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Obtain the effective bad-bits value.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-FEB-1990 (RFWS):
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

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      BYTE BADBIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Obtain the effective bad-bits value.
      CALL NDF1_GTBB( IACB, BADBIT, STATUS )
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_BB_ERR',
     :   'NDF_BB: Error obtaining the bad-bits mask value for the ' //
     :   'quality component of an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_BB', STATUS )
      END IF

      END
