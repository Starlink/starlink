      SUBROUTINE ESP_MON( STATUS )
*+
*  Name:
*     ESP_MON

*  Purpose:
*     Top-level monolith routine.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ESP_MON( STATUS )

*  Description:
*     This routine gets the action name directly from the Unix kernel.
*     It then calls the appropriate routine to perform the
*     specified action. An error will be reported and STATUS will
*     be set if the action name is not recognized.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*        Original version for CCDPACK.
*     GJP: Grant Privett (STARLINK)
*        Modified slightly for ESP.

*  History:
*     5-JUN-1994 (PDRAPER):
*        Original version - based on CCDPACK monolith routine of the same name.
*     24-Aug-1994 (GJP)
*        Now employs TASK_GET_NAME to obtain command names.
*     10-Oct-1996 (GJP)
*        Added GAUFIT reference.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Parameter constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Length of string excluding trailing blanks

*  Local variables:
      CHARACTER *(PAR__SZNAM) ACTION  ! Action name

*.

*   Check inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Get the requested action the name against each valid value in turn,
*   calling the appropriate routine...
      ACTION=' '
      CALL TASK_GET_NAME(ACTION,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Check each in turn.
      IF ( ACTION .EQ. 'CORR' ) THEN
         CALL CORR( STATUS )
      ELSE IF ( ACTION .EQ. 'ELLFOU' ) THEN
         CALL ELLFOU( STATUS )
      ELSE IF ( ACTION.EQ. 'ELLPRO' ) THEN
         CALL ELLPRO( STATUS )
      ELSE IF ( ACTION .EQ. 'FASTMED' ) THEN
         CALL FASTMED( STATUS )
      ELSE IF ( ACTION.EQ. 'GAUFIT' ) THEN
         CALL GAUFIT( STATUS )
      ELSE IF ( ACTION.EQ. 'GRAPHS' ) THEN
         CALL GRAPHS( STATUS )
      ELSE IF ( ACTION .EQ. 'HISTPEAK' ) THEN
         CALL HISTPEAK( STATUS )
      ELSE IF ( ACTION .EQ. 'HSUB' ) THEN
         CALL HSUB( STATUS )
      ELSE IF ( ACTION.EQ. 'LOBACK' ) THEN
         CALL LOBACK( STATUS )
      ELSE IF ( ACTION .EQ. 'MASK' ) THEN
         CALL MASK( STATUS )
      ELSE IF ( ACTION .EQ. 'MIXUP' ) THEN
         CALL MIXUP( STATUS )
      ELSE IF ( ACTION .EQ. 'SECTOR' ) THEN
         CALL SECTOR( STATUS )
      ELSE IF ( ACTION .EQ. 'SELFC' ) THEN
         CALL SELFC( STATUS )
      ELSE IF ( ACTION .EQ. 'SELFCW' ) THEN
         CALL SELFCW( STATUS )
      ELSE IF ( ACTION .EQ. 'SKEW' ) THEN
         CALL SKEW( STATUS )
      ELSE IF ( ACTION .EQ. 'TOPPED' ) THEN
        CALL TOPPED( STATUS )
      ELSE IF ( ACTION .EQ. 'ESPHELP' ) THEN
        CALL ESPHELP( STATUS )

*    If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( 'ESP_ERR','ESP: The action name ^ACTION is '//
     :                  'not recognised by the ESP C-shell '//
     :                  'monolith.', STATUS )
      END IF

 9999 CONTINUE

      END

