************************************************************************

      SUBROUTINE AGD_ACTIV ( STATUS )

*+
*  Name :
*     AGD_ACTIV
*
*  Purpose :
*     Initialise IDI
*
*  Invocation :
*     CALL AGD_ACTIV( STATUS )
*
*  Description :
*     Initialise IDI. This has to be called before any other AGD or
*     IDI routines. An error is returned if this or any other graphics
*     interface is already active.
*
*  Arguments :
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm :
*     Check status on entry.
*     Check that no other graphics packages are active.
*     Initialise the IDI-related common blocks.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*
*  History :
*     June 1990 (NE):
*        Original version
*     April 1991 (NE):
*        See if the package is already open
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_idips'

*  Status :
      INTEGER STATUS
*.

*   CHeck status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check the graphics package flags
         IF ( CIDION .OR. CGKSON .OR. CPGPON ) THEN
            STATUS = AGI__GRPON
            CALL ERR_REP( 'AGD_ACTIV_GRPON', 'Graphics package in use',
     :                    STATUS )
            GOTO 99
         ELSE
            CIDION = .TRUE.
         ENDIF

*   Initialise the IDI parameter common blocks
         CALL AGD_1IINIT( STATUS )

      ENDIF

  99  CONTINUE

      END

