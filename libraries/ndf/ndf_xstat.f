      SUBROUTINE NDF_XSTAT( INDF, XNAME, THERE, STATUS )
*+
*  Name:
*     NDF_XSTAT

*  Purpose:
*     Determine if a named NDF extension exists.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_XSTAT( INDF, XNAME, THERE, STATUS )

*  Description:
*     The routine returns a logical value indicating whether a named
*     extension is present in an NDF.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the extension.
*     THERE = LOGICAL (Returned)
*        Whether the extension is present in the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Check the extension name.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Ensure that extension information is available in the DCB.
*     -  If an extension (MORE) structure does not exist, then neither
*     does the extension component.
*     -  Otherwise, search for the component to see if it is there.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Completed prologue and corrected minor errors.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ private constants
      INCLUDE 'NDF_CONST'        ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_XLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to extension structure.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) XNAME

*  Arguments Returned:
      LOGICAL THERE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check the extension name.
      CALL NDF1_CHXNM( XNAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB ( IACB )

*  Ensure that extension information is available in the DCB.
         CALL NDF1_DX( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If an extension structure does not exist, then neither does the
*  extension component.
            IF ( DCB_XLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
               THERE = .FALSE.

*  Otherwise, see if the specified component is present.
            ELSE
               CALL DAT_THERE( DCB_XLOC( IDCB ), XNAME, THERE, STATUS )
            END IF
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_XSTAT_ERR',
     :   'NDF_XSTAT: Error determining if a named NDF extension ' //
     :   'exists.', STATUS )
         CALL NDF1_TRACE( 'NDF_XSTAT', STATUS )
      END IF

      END
