      SUBROUTINE GNS_STOP ( PKG, STATUS )

*+
*  Name:
*     GNS_STOP

*  Purpose:
*     Stop the GNS system

*  Invocation:
*     CALL GNS_STOP( PKG, STATUS )

*  Description:
*     The GNS databases for the specified package are closed.

*  Arguments:
*     PKG = CHARACTER*(*) (Given)
*        The package name. The only packages currently supported are
*        GKS and IDI.
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Authors:
*     DLT: D.L. Terrett (STARLINK)
*     NE: Nick Eaton (Durham University)

*  History:
*     16-MAY-1988 (DLT):
*        Original version.
*      9-JUL-1990
*        Added error reporting
*      1-SEP-1992 (NE):
*        Updated prologue.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'GNS_ERR'

*  Arguments Given:
      CHARACTER*(*) PKG

*  Status:
      INTEGER STATUS
*.

      IF (PKG.EQ.'GKS'.OR.PKG.EQ.'gks') THEN
         CALL gns_1TERMG(STATUS)
      ELSE IF (PKG.EQ.'IDI'.OR.PKG.EQ.'idi') THEN
         CALL gns_1TERMI(STATUS)
      ELSE
         STATUS = GNS__PKGNS
         CALL EMS_REP( 'GNS_STOP_PKGNS',
     :                 'Package not supported by GNS', STATUS )
      END IF

      END       

