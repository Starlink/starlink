************************************************************************

      SUBROUTINE AGP_ACTIV ( STATUS )

*+
*  Name :
*     AGP_ACTIV
*
*  Purpose :
*     Initialise PGPLOT (native PGPLOT version)
*
*  Invocation :
*     CALL AGP_ACTIV( STATUS )
*
*  Description :
*     Initialise PGPLOT. This has to be called before any other AGP or
*     PGPLOT routines. An error is returned if this or any other
*     graphics interface is active.
*
*  Arguments :
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm :
*     Check status on entry.
*     Check that no other graphics packages are active.
*
*
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*
*  Authors :
*     NE:  Nick Eaton (Durham University)
*     BKM: Brian McIlwrath (Starlink, RAL)
*
*  History :
*     Aug 1988 (NE):
*        Original version
*     Jun 1990 (NE):
*        Test graphics package flags
*     Apr 1991 (NE):
*        See if this package is already open
*     Dec 1999 (BKM):
*        Revise logic for native PGPLOT

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

*   Check graphics package flags
         IF ( CPGPON .OR. CIDION .OR. CGKSON ) THEN
            STATUS = AGI__GRPON
            CALL ERR_REP( 'AGP_ACTIV_GRPON', 'Graphics package in use',
     :                    STATUS )
         ELSE

*   Indicate that PGPLOT is now active
            CPGPON = .TRUE.
         ENDIF

      ENDIF

      END

