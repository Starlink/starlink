************************************************************************

      SUBROUTINE AGP_ACTIV ( STATUS )

*+
*  Name :
*     AGP_ACTIV
*
*  Purpose :
*     Initialise PGPLOT
*
*  Invocation :
*     CALL AGP_ACTIV( STATUS )
*
*  Description :
*     Initialise PGPLOT. This has to be called before any other AGP or
*     PGPLOT routines. An error is returned if this or any other
*     graphics interface, other than AGS_, is active.
*
*  Arguments :
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm :
*     Check status on entry.
*     Check that no other graphics packages (except possibly GKS) are
*     active.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*
*  History :
*     Aug 1988 (NE):
*        Original version
*     Jun 1990 (NE):
*        Test graphics package flags
*     Apr 1991 (NE):
*        See if this package is already open
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_idips'

*  Status :
      INTEGER STATUS
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check graphics package flags ( GKS can be started before PGPLOT,
*   but not IDI )
         IF ( CPGPON .OR. CIDION ) THEN
            STATUS = AGI__GRPON
            CALL ERR_REP( 'AGP_ACTIV_GRPON', 'Graphics package in use',
     :                    STATUS )
         ELSE

*   Indicate that PGPLOT is active
            CPGPON = .TRUE.
         ENDIF

      ENDIF

      END

