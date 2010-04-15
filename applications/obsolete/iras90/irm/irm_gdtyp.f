      SUBROUTINE IRM_GDTYP( GPTYPE, GDTYPE, STATUS )
*+
*  Name:
*     IRM_GDTYP

*  Purpose:
*     Get the graphic device type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_GDTYP( GPTYPE, GDTYPE, STATUS )

*  Description:
*     This subroutine inquires the workstation class of the current
*     graphic workstation. See document SUN57 (appendix A ) for the
*     defined classes.
*     A graphic workstation should have been opened before calling
*     this routine.

*  Arguments:
*     GPTYPE = CHARACTER*( * ) (Given)
*        The graphics system. It must be 'SGS', or 'GKS' at present.
*     GDTYPE = CHARACTER * ( * )*( GNS__SZKEY ) (Given)
*        The current workstation class. It should have defined length
*        no less than GNS__SZKEY
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GNS_PAR'          ! GNS package constants

*  Arguments Given:
      CHARACTER*( * ) GPTYPE

*  Arguments Returned:
      CHARACTER*( GNS__SZKEY ) GDTYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*( 10 ) GPACK     ! Graphic package name
      INTEGER WKID               ! Current graphic workstation ID
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the graphic package used. If unsupport graphic package is used,
*  report and exit with status set.
      GPACK = GPTYPE
      CALL CHR_LDBLK( GPACK )
      CALL CHR_UCASE( GPACK )
      IF ( GPACK( : 3 ) .NE. 'GKS' .AND. GPACK( : 3 ) .NE. 'SGS' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_GDTYP_ERR1',
     :   'IRM_GDTYP: Unsupported graphic package is used. Unable '//
     :   'to get workstation class.', STATUS )
         GOTO 999
      END IF

*  Start the GNS system for GKS.
      CALL GNS_START( 'GKS', STATUS )

*  Get the current workstation ID.
      CALL SGS_ICURW( WKID )

*  Find the class of the workstation.
      CALL GNS_IWCG( WKID, 'CLASS', GDTYPE, STATUS )

 999  CONTINUE

      END
