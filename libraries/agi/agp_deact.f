************************************************************************

      SUBROUTINE AGP_DEACT ( STATUS )

*+
*  Name :
*     AGP_DEACT
*
*  Purpose :
*     Close down PGPLOT
*
*  Invocation :
*     CALL AGP_DEACT( STATUS )
*
*  Description :
*     Close down PGPLOT whatever the value of status. This should be
*     called after all AGP and PGPLOT routines.
*
*  Arguments :
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm :
*     Close down PGPLOT.
*     Clear the PGPLOT active flag.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*
*  History :
*     Aug 1988 (NE):
*        Original version
*     Jun 1990 (NE):
*        Reset graphics package flag
*     Apr 1991 (NE):
*        Return error if interface is not active
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

*   Signal an error if the interface is not active
      IF ( .NOT. CPGPON ) THEN
         STATUS = AGI__GRPNA
         CALL ERR_REP( 'AGP_DEACT_GRPNA', 'Graphics package not active',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Without checking status close down PGPLOT
      CALL PGEND

*   Reset the graphics package flag
      CPGPON = .FALSE.

  99  CONTINUE

      END

