      SUBROUTINE IRA1_MAP( IDA, FORWRD, TRID, STATUS )
*+
*  Name:
*     IRA1_MAP

*  Purpose:
*     Get a TRANSFORM identifier for a compiled mapping.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_MAP( IDA, FORWRD, TRID, STATUS )

*  Description:
*     If the mapping has previously been compiled, the identifier is
*     retrieved from common and a check made to ensure that the
*     identifier is still valid. If no valid identifier is obtained by
*     this method, then the relevant component within the astrometry
*     structure is located, and the required mapping is compiled. The
*     new identifier is stored in common. The inverse mapping is
*     defined by structure INV_TRANSFORM if it exists. If INV_TRANSFORM
*     does not exist, then both the forward and inverse mappings are
*     defined by FWD_TRANSFORM.
*
*     This routine assumes that the validity of the IRA identifier
*     given by argument IDA has been established by the calling
*     routine.

*  Arguments:
*     IDA = INTEGER (Given)
*        The IRA identifier for the astrometry information to be used.
*        Assumed to be valid.
*     FORWRD = LOGICAL (Given)
*        True if the forward mapping from image coordinates to sky
*        coordinates is required; false otherwise.
*     TRID = INTEGER (Returned)
*        The TRANSFORM identifier for the compiled mapping.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! STARLINK data constants.
      INCLUDE '/star/include/trn_err'          ! TRANSFORM error values.
      INCLUDE 'IRA_PAR'          ! IRA constants values.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_TRIDF( IRA__MAX ) = INTEGER (Read and Write)
*           TRANSFORM identifies for compiled forward mappings.
*        ACM_TRIDI( IRA__MAX ) = INTEGER (Read and Write)
*           TRANSFORM identifies for compiled inverse mappings.
*        ACM_ASLOC( IRA__MAX ) = CHARACTER (Read)
*           HDS locators for each Astrometry Structure.

*  Arguments Given:
      INTEGER IDA
      LOGICAL FORWRD

*  Arguments Returned:
      INTEGER TRID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COMP*(DAT__SZLOC)! HDS object containing transform
                                 ! structure.
      CHARACTER LOC*(DAT__SZLOC) ! Locator to transform structure.
      INTEGER   NVIN             ! No. of input variables to mapping.
      INTEGER   NVOUT            ! No. of output variables from mapping.
      LOGICAL   THERE            ! True is INV_TRANSFORM exists.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Retrieve the TRANSFORM identifiers stored in common, and set the HDS
*  component name for the TRANSFORM structure.
      IF( FORWRD ) THEN
         TRID = ACM_TRIDF( IDA )
         COMP = 'FWD_TRANSFORM'
      ELSE
         TRID = ACM_TRIDI( IDA )
         COMP = 'INV_TRANSFORM'
         CALL DAT_THERE( ACM_ASLOC( IDA ), COMP, THERE, STATUS )
         IF( .NOT. THERE ) COMP = 'FWD_TRANSFORM'
      END IF

*  If a TRANSFORM identifier was retrieved from common, check it is
*  still valid by attempting to get the number of variables from it.
      IF( TRID .NE. VAL__BADI ) THEN
         CALL ERR_MARK

         CALL TRN_GTNVC( TRID, NVIN, NVOUT, STATUS )

*  If STATUS indicates that the identifier does not correspond to a
*  compiled mapping, reset the identifier to the bad value and annul the
*  error condition.
         IF( STATUS .EQ. TRN__MIDIN ) THEN
            TRID = VAL__BADI
            CALL ERR_ANNUL( STATUS )
         END IF

         CALL ERR_RLSE
      END IF

*  If no TRANSFORM identifier has yet been set up for the compiled
*  mapping (as shown by the stored identifier still being at the BAD
*  values set up in IRA_INIT), then locate the TRANSFORM structure and
*  compile the required mapping.
      IF( TRID .EQ. VAL__BADI ) THEN

         CALL DAT_FIND( ACM_ASLOC( IDA ), COMP, LOC, STATUS )
         CALL TRN_COMP( LOC, FORWRD, TRID, STATUS )
         CALL DAT_ANNUL( LOC, STATUS )

*  Store the TRANSFORM identifier in common.
         IF( FORWRD ) THEN
            ACM_TRIDF( IDA ) = TRID
         ELSE
            ACM_TRIDI( IDA ) = TRID
         END IF

      END IF

*  If an error occurred, give the context.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA1_MAP_ERR1',
     :   'IRA1_MAP: Unable to compile a TRANSFORM mapping', STATUS )
      END IF

      END
