      SUBROUTINE INVOKE( NAME, STATUS )
*+
*  Name:
*     SUBROUTINE INVOKE

*  Purpose:
*     Invoke applications from command specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL INVOKE( NAME, STATUS )

*  Arguments:
*     NAME = CHARACTER* ( * ) (Given)
*        Name of task to be executed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Iterate IF ... THEN ... ELSE IF ... structure using FUNCTION cmd_SAM
*     and subsequent calls to application modules (usr_...).

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     05-MAY-82 (JRG):
*       AT4 version.
*     07-SEP-88 (PCTR):
*       IUEDR Vn. 2.0. Conversion to FORTRAN. Command name rationalisation.
*     05-MAY-89 (PCTR):
*       IUEDR Vn. 2.1.
*       Some restructuring and final conversion to SGP/16 style.
*     29-JUN-94 (MJC):
*       IUEDR Vn. 3.1-1.  Tidy up and change to use SAI__OK.
*     15-JUL-94 (MJC):
*       Removed old Vn 2.0 commands.
*     20-JUL-94 (MJC):
*       Ignore SET command as this is used by PAR.
*     20-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2. Added SPAWN command call.
*     22-AUG-94 (MJC):
*       IUEDR Vn. 3.1-3.  Added AGSHIFT command skeleton.
*     17-OCT-94 (MJC):
*       IUEDR Vn. 3.1-4.  Added CLEAN command skeleton.
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2.  Tidy up and add prologue.
*     16-APR-95 (MJC):
*       Added AESHIFT command skeleton.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Defintions:
      IMPLICIT  NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Constants:
      INTEGER ERR
      PARAMETER ( ERR = -3 )

*  Status:
      INTEGER STATUS       ! Global status.

*  Arguments Given:
      CHARACTER* ( * ) NAME
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Force the input string to upper-case before testing.
      CALL CHR_UCASE( NAME )

*   Check the string against valid A-task names - if matched then
*   call the relevant A-task
      IF ( 'IUEDR3' .EQ. NAME ) THEN
         CONTINUE

      ELSE IF ( ' ' .EQ. NAME ) THEN
         CONTINUE

      ELSE IF ( 'QUIT' .EQ. NAME ) THEN
         CONTINUE

      ELSE IF ( 'SET' .EQ. NAME ) THEN
         CONTINUE

      ELSE IF ( 'AESHIFT' .EQ. NAME ) THEN
         CALL usr_AESHIFT( STATUS )

      ELSE IF ( 'AGSHIFT' .EQ. NAME ) THEN
         CALL usr_AGSHIFT( STATUS )

      ELSE IF ( 'BARKER' .EQ. NAME ) THEN
         CALL usr_BARKER( STATUS )

      ELSE IF ( 'CGSHIFT' .EQ. NAME ) THEN
         CALL usr_CGSHIFT( STATUS )

      ELSE IF ( 'CLEAN' .EQ. NAME ) THEN
         CALL usr_CLEAN( STATUS )

      ELSE IF ( 'CULIMITS' .EQ. NAME ) THEN
         CALL usr_CULIMITS( STATUS )

      ELSE IF ( 'CURSOR' .EQ. NAME ) THEN
         CALL usr_CURSOR( STATUS )

      ELSE IF ( 'DRIMAGE' .EQ. NAME ) THEN
         CALL usr_DRIMAGE( STATUS )

      ELSE IF ( 'EDIMAGE' .EQ. NAME ) THEN
         CALL usr_EDIMAGE( STATUS )

      ELSE IF ( 'EDMEAN' .EQ. NAME ) THEN
         CALL usr_EDMEAN( STATUS )

      ELSE IF ( 'EDSPEC' .EQ. NAME ) THEN
         CALL usr_EDSPEC( STATUS )

      ELSE IF ( 'ERASE' .EQ. NAME ) THEN
         CALL usr_ERASE( STATUS )

      ELSE IF ( 'IUEHELP' .EQ. NAME ) THEN
         CALL usr_HELP( STATUS )

      ELSE IF ( 'LBLS' .EQ. NAME ) THEN
         CALL usr_LBLS( STATUS )

      ELSE IF ( 'LISTIUE' .EQ. NAME ) THEN
         CALL usr_LISTIUE( STATUS )

      ELSE IF ( 'MAP' .EQ. NAME ) THEN
         CALL usr_MAP( STATUS )

      ELSE IF ( 'MODIMAGE' .EQ. NAME ) THEN
         CALL usr_MODIMAGE( STATUS )

      ELSE IF ( 'MTMOVE' .EQ. NAME ) THEN
         CALL usr_MTMOVE( STATUS )

      ELSE IF ( 'MTREW' .EQ. NAME ) THEN
         CALL usr_MTREW( STATUS )

      ELSE IF ( 'MTSHOW' .EQ. NAME ) THEN
         CALL usr_MTSHOW( STATUS )

      ELSE IF ( 'MTSKIPEOV' .EQ. NAME ) THEN
         CALL usr_MTSKIPEOV( STATUS )

      ELSE IF ( 'MTSKIPF' .EQ. NAME ) THEN
         CALL usr_MTSKIPF( STATUS )

      ELSE IF ( 'NEWABS' .EQ. NAME ) THEN
         CALL usr_NEWABS( STATUS )

      ELSE IF ( 'NEWCUT' .EQ. NAME ) THEN
         CALL usr_NEWCUT( STATUS )

      ELSE IF ( 'NEWDISP' .EQ. NAME ) THEN
         CALL usr_NEWDISP( STATUS )

      ELSE IF ( 'NEWFID' .EQ. NAME ) THEN
         CALL usr_NEWFID( STATUS )

      ELSE IF ( 'NEWRIP' .EQ. NAME ) THEN
         CALL usr_NEWRIP( STATUS )

      ELSE IF ( 'NEWTEM' .EQ. NAME ) THEN
         CALL usr_NEWTEM( STATUS )

      ELSE IF ( 'OUTEM' .EQ. NAME ) THEN
         CALL usr_OUTEM( STATUS )

      ELSE IF ( 'OUTLBLS' .EQ. NAME ) THEN
         CALL usr_OUTLBLS( STATUS )

      ELSE IF ( 'OUTMEAN' .EQ. NAME ) THEN
         CALL usr_OUTMEAN( STATUS )

      ELSE IF ( 'OUTNET' .EQ. NAME ) THEN
         CALL usr_OUTNET( STATUS )

      ELSE IF ( 'OUTRAK' .EQ. NAME ) THEN
         CALL usr_OUTRAK( STATUS )

      ELSE IF ( 'OUTSCAN' .EQ. NAME ) THEN
         CALL usr_OUTSCAN( STATUS )

      ELSE IF ( 'OUTSPEC' .EQ. NAME ) THEN
         CALL usr_OUTSPEC( STATUS )

      ELSE IF ( 'PLCEN' .EQ. NAME ) THEN
         CALL usr_PLCEN( STATUS )

      ELSE IF ( 'PLGRS' .EQ. NAME ) THEN
         CALL usr_PLGRS( STATUS )

      ELSE IF ( 'PLFLUX' .EQ. NAME ) THEN
         CALL usr_PLFLUX( STATUS )

      ELSE IF ( 'PLMEAN' .EQ. NAME ) THEN
         CALL usr_PLMEAN( STATUS )

      ELSE IF ( 'PLNET' .EQ. NAME ) THEN
         CALL usr_PLNET( STATUS )

      ELSE IF ( 'PLSCAN' .EQ. NAME ) THEN
         CALL usr_PLSCAN( STATUS )

      ELSE IF ( 'PRGRS' .EQ. NAME ) THEN
         CALL usr_PRGRS( STATUS )

      ELSE IF ( 'PRLBLS' .EQ. NAME ) THEN
         CALL usr_PRLBLS( STATUS )

      ELSE IF ( 'PRMEAN' .EQ. NAME ) THEN
         CALL usr_PRMEAN( STATUS )

      ELSE IF ( 'PRSCAN' .EQ. NAME ) THEN
         CALL usr_PRSCAN( STATUS )

      ELSE IF ( 'PRSPEC' .EQ. NAME ) THEN
         CALL usr_PRSPEC( STATUS )

      ELSE IF ( 'READIUE' .EQ. NAME ) THEN
         CALL usr_READIUE( STATUS )

      ELSE IF ( 'READSIPS' .EQ. NAME ) THEN
         CALL usr_READSIPS( STATUS )

      ELSE IF ( 'SAVE' .EQ. NAME ) THEN
         CALL usr_SAVE( STATUS )

      ELSE IF ( 'SCAN' .EQ. NAME ) THEN
         CALL usr_SCAN( STATUS )

      ELSE IF ( 'SETA' .EQ. NAME ) THEN
         CALL usr_SETA( STATUS )

      ELSE IF ( 'SETD' .EQ. NAME ) THEN
         CALL usr_SETD( STATUS )

      ELSE IF ( 'SETM' .EQ. NAME ) THEN
         CALL usr_SETM( STATUS )

      ELSE IF ( 'SGS' .EQ. NAME ) THEN
         CALL usr_SGS( STATUS )

      ELSE IF ( 'SHOW' .EQ. NAME ) THEN
         CALL usr_SHOW( STATUS )

      ELSE IF ( 'SPAWN' .EQ. NAME ) THEN
         CALL usr_SPAWN( STATUS )

      ELSE IF ( 'TRAK' .EQ. NAME ) THEN
         CALL usr_TRAK( STATUS )

      ELSE
         CALL cmd_ERR( 'Command not known or ambiguous\\' )
      END IF

      IF ( STATUS .EQ. ERR ) THEN
         STATUS = SAI__OK
      END IF

 999  CONTINUE
      END
