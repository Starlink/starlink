************************************************************************

      SUBROUTINE AGI_SROOT ( STATUS )

*+
*  Name :
*     AGI_SROOT
*
*  Purpose :
*     Select the root picture for searching
*
*  Invocation :
*     CALL AGI_SROOT( STATUS )
*
*  Description :
*     The root picture is selected for searching operations. The root
*     picture contains all other pictures (including the base picture)
*     and can be used to recall any picture whether it lies within the
*     current picture or not. This is used to override the usual
*     restriction that a recalled picture must lie within the bounds of
*     the current picture. The root picture is automatically deselected
*     after a call to any of the recall routines AGI_RC*. Recall is the
*     only operation allowed with the root picture, any other operation
*     called while the root picture is selected will use the current
*     picture.
*
*  Arguments :
*    STATUS = INTEGER (Given and Returned)
*       The global status
*
*  Algorithm :
*     A flag in the common block is set.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*
*  History :
*     July 1990 (NE):
*        Original version
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*  Global variables :
      INCLUDE 'agi_pfree'

*  Status :
      INTEGER STATUS
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Set the root flag
         CROOT = 1

      ENDIF

      END

