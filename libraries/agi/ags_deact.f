************************************************************************

      SUBROUTINE AGS_DEACT ( STATUS )

*+
*  Name :
*     AGS_DEACT
*
*  Purpose :
*     Close down SGS
*
*  Invocation :
*     CALL AGS_DEACT( STATUS )
*
*  Description :
*     Close down SGS whatever the value of status. This should be
*     called after all AGS and SGS routines.
*
*  Arguments :
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm :
*     Close down SGS.
*     Clear the SGS active flag.
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_idips'
      INCLUDE 'agi_locs'

*  Status :
      INTEGER STATUS
*.

*   Signal an error if the interface is not active
      IF ( .NOT. CGKSON ) THEN
         STATUS = AGI__GRPNA
         CALL ERR_REP( 'AGS_DEACT_GRPNA', 'Graphics package not active',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Without checking status close down SGS
      CALL SGS_CLOSE

*   Reset the graphics package flag
      CGKSON = .FALSE.

*   Flush HDS if the database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGS_DEACT +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

