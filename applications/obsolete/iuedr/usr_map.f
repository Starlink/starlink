      SUBROUTINE USR_MAP( STATUS )
*+
*  Name:
*     SUBROUTINE USR_MAP

*  Purpose:
*     Merge extracted order spectra.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_MAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     One or more orders in the current HIRES spectrum are mapped
*     onto a regular (vacuum) wavelength grid.
*     The result is stored in CMCOMB.
*     The list of orders specified by the ORDERS parameter are mapped
*     onto a uniform wavelength grid specified by the BL and
*     BSAMP parameters. Suitable default values for these are generated.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-JAN-82 (JRG):
*       Original version.
*     08-SEP-82 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     10-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
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

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'

*  Local Variables:
      INTEGER IRES       ! Resolution index.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration and Spectrum (raw).
      CALL DASSOC( 'S\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*  Check resolution.
      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: invalid resolution\\', STATUS )

      ELSE IF ( IRES .EQ. 2 ) THEN
         CALL HICOMB( STATUS )

      ELSE IF ( IRES .EQ. 1 ) THEN
         CALL LOCOMB( STATUS )

      ELSE
         CALL ERROUT( 'Error: cannot handle this spectrum\\', STATUS )
      END IF

 999  CONTINUE

      END
