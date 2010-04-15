      SUBROUTINE FIGARO3( STATUS )
*+
*  Name:
*     FIGARO3

*  Purpose:
*     Top-level ADAM monolith routine for the FIGARO package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIGARO3( STATUS )

*  Description:
*     This routine interprets the action name passed to it and calls
*     the appropriate routine to perform the specified action. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     ACD: Clive Davenhall (UoE, Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*     23-NOV-1992 (HME):
*        Change PAR_INIT call to set batch flag false.
*        Use INDEX and ICH_FOLD rather than CHR_DELIM and CHR_UCASE.
*     12-MAR-1993 (HME):
*        Now find out batch flag from environment variable FIGARO_MODE.
*     14-JAN-1994 (HME):
*        Split monolith into three. Use get_task_name.
*     9-NOV-1998 (ACD):
*        Add FLAIRCOMP, FLAIREXT and SKYLINER.
*     11-NOV-1998 (ACD):
*        Added status arguments for FLAIRCOMP, FLAIREXT and SKYLINER
*     24-APR-2006 (TIMJ):
*        Force inclusion of DSA block data
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Standard PAR constants

*  External Block Data:
      EXTERNAL DSA_BLOCK

*  Arguments Given and Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL BATCH
      INTEGER IGNORE
      CHARACTER * ( PAR__SZNAM ) ACTION
      CHARACTER * ( 8 ) ENVVAR

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
      IF (ACTION.EQ.'ARC') THEN
         CALL ARC
      ELSE IF (ACTION.EQ.'CDIST') THEN
         CALL CDIST
      ELSE IF (ACTION.EQ.'ECHARC') THEN
         CALL ECHARC
      ELSE IF (ACTION.EQ.'ECHFIND') THEN
         CALL ECHFIND
      ELSE IF (ACTION.EQ.'ECHMASK') THEN
         CALL ECHMASK
      ELSE IF (ACTION.EQ.'ECHMERGE') THEN
         CALL ECHMERGE
      ELSE IF (ACTION.EQ.'ECHSELECT') THEN
         CALL ECHSELECT
      ELSE IF (ACTION.EQ.'FINDSP') THEN
         CALL FINDSP
      ELSE IF (ACTION.EQ.'FLAIRCOMP') THEN
         CALL FLAIRCOMP (STATUS)
      ELSE IF (ACTION.EQ.'FLAIREXT') THEN
         CALL FLAIREXT (STATUS)
      ELSE IF (ACTION.EQ.'IARC') THEN
         CALL IARC
      ELSE IF (ACTION.EQ.'ISCRUNCH') THEN
         CALL ISCRUNCH
      ELSE IF (ACTION.EQ.'ISCRUNI') THEN
         CALL ISCRUNCH
      ELSE IF (ACTION.EQ.'MASKEXT') THEN
         CALL MASKEXT
      ELSE IF (ACTION.EQ.'OFFDIST') THEN
         CALL OFFDIST
      ELSE IF (ACTION.EQ.'OVERPF') THEN
         CALL OVERPF
      ELSE IF (ACTION.EQ.'POLEXT') THEN
         CALL POLEXT
      ELSE IF (ACTION.EQ.'SDIST') THEN
         CALL SDIST
      ELSE IF (ACTION.EQ.'SKYLINER') THEN
         CALL SKYLINER (STATUS)
      ELSE IF (ACTION.EQ.'VACHEL') THEN
         CALL VACHEL
      ELSE IF (ACTION.EQ.'XCOPI') THEN
         CALL XCOPI
      ELSE IF (ACTION.EQ.'XCOPY') THEN
         CALL XCOPY
      ELSE
         CALL FIG_HELP( 'diversion', STATUS )
      END IF

      END
