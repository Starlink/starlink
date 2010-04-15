      SUBROUTINE NDPROGS_MON( STATUS )
*+
*  Name:
*     NDPROGS

*  Purpose:
*     Top-level ADAM monolith routine for the NDPROGS package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDPROGS( STATUS )

*  Description:
*     This routine interprets the action name passed to it and calls
*     the appropriate routine to perform the specified action. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     ACTION = CHARACTER * ( 32 ) (Given and Returned)
*        The action name to be interpreted. The value given will be
*        forced to upper case by this routine.
*        On Unix platfor*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This source file is intended for the Unix version only.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     GOLDJIL: Julian Gold (RGO).
*     GJP: Grant Privett (Cardif, Starlink)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (HME):
*        Original version. SUBSET only. (NFIGARO.)
*     26-JUN-1991 (HME):
*        SPECFIT inserted.
*     28-JUN-1991 (HME):
*        EXTRACT inserted. Monolith called SPECDRE now.
*     5-JUL-1991 (HME):
*        ASCIN, ASCOUT, BBODY, GROW inserted.
*     24-JUL-1991 (HME):
*        CORREL, GOODVAR inserted.
*     13-SEP-1991 (HME):
*        Access the essential ELSE-IF structure from IFBLOCK.FOR.
*     15-JUL-1992 (HME):
*        Get 0-th argument (name of the executable, or rather the link
*        used) and extract the action from it.
*     18-AUG-1992 (HME):
*        Adapt from Specdre to Figaro.
*     12-NOV-1992 (GOLDJIL):
*        Adapted from Figaro to NDPROGS
*     28-OCT-1994 (GJP):
*        Adapted to use GET_NAME_TASK.
*     22-NOV-1994 (GJP):
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Standard PAR constants

*  Arguments Given and Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      LOGICAL BATCH
      INTEGER IGNORE
      CHARACTER * ( PAR__SZNAM ) ACTION
      CHARACTER * ( 12 ) ENVVAR

*  Internal References:
      INTEGER ICH_FOLD

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( ACTION, STATUS )
      IGNORE = ICH_FOLD( ACTION )

*  Find out about the batch mode.
      CALL PSX_GETENV( 'FIGARO_MODE', ENVVAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         BATCH = .FALSE.
      ELSE
         IGNORE = ICH_FOLD( ENVVAR )
         IF ( ENVVAR .EQ. 'BATCH' ) THEN
            BATCH = .TRUE.
         ELSE
            BATCH = .FALSE.
         END IF
      END IF

*  Initialise the (F)PAR common block.
      CALL PAR_INIT( ACTION, ' ', 0, BATCH, IGNORE )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...
      IF ( 1 .EQ. 2 ) THEN

      INCLUDE 'IFBLOCK'

*     [ADAM_action]...

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( 'NDPROGS_ERR',
     :      'NDPROGS: The action ''^ACTION'' is not available in ' //
     :      'the Unix release of NDPROGS 3.1.', STATUS )
      END IF

      END
