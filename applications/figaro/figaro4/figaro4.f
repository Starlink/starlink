      SUBROUTINE FIGARO4( STATUS )
*+
*  Name:
*     FIGARO4

*  Purpose:
*     Top-level ADAM monolith routine for the FIGARO package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIGARO4( STATUS )

*  Description:
*     This routine interprets the action name passed to it and calls
*     the appropriate routine to perform the specified action. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This source file is intended for the Unix version only.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     ACC: Anne Charles (RAL, Starlink)
*     MJC: Malcolm J. Currie (RAL, Starlink)
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
*     18-AUG-1993 (HME):
*        Take SPE_HELP out of the ifblock file and put it in here.  We
*        need our own help provider only in the pseudo-monolith.  That
*        apart, it uses spaef which includes sys/termio.h and does not
*        compile on a VAX.
*     17 Feb 1994 (hme):
*        Use TASK_GET_NAME. Include the IF block with the editor rather
*        than at compile time.
*     13 May 1994 (hme):
*        Remove SPE_HELP, since that is a standalone programme now.
*     15 Oct 1997 (acc):
*        Merge SPECDRE into FIGARO. SPECDRE becomes FIGARO4.
*        Change RESAMPLE to RESAMP because of conflict with FIGARO.
*     25 Nov 1997 (acc):
*        Change GROW to GROWS because of name clash with FIGARO.
*     2004 July 16 (MJC):
*        GOODVAR removed as it was transferred to Figaro in 2001 July.
*     24-APR-2006 (TIMJ):
*        Force inclusion of block data
*     2014-11-14 (TIMJ):
*        Cancel NDF parameters to free dangling locators.
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
      EXTERNAL SPD_FBLK
      EXTERNAL SPD_PBLK

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( PAR__SZNAM ) ACTION

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get task name.
      CALL TASK_GET_NAME( ACTION, STATUS )
      CALL CHR_UCASE( ACTION )

*  Mark currently active NDF parameters so that they will not be
*  canceled at the end.
      CALL NDF_CANCL( '*', STATUS)

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...
      IF ( 1 .EQ. 2 ) THEN
         CONTINUE

*  22 Jun 1993
      ELSE IF ( ACTION .EQ. 'ARCDISP' ) THEN
         CALL ARCDISP( STATUS )

*  28 May 1993
      ELSE IF ( ACTION .EQ. 'ARCGENDB' ) THEN
         CALL ARCGENDB( STATUS )

*  03 Jun 1993
      ELSE IF ( ACTION .EQ. 'ARCIDENT' ) THEN
         CALL ARCIDENT( STATUS )

*  08 Jun 1993
      ELSE IF ( ACTION .EQ. 'ARCLOCAT' ) THEN
         CALL ARCLOCAT( STATUS )

*  05-JUL-1991
      ELSE IF ( ACTION .EQ. 'ASCIN' ) THEN
         CALL ASCIN( STATUS )

*  05-JUL-1991
      ELSE IF ( ACTION .EQ. 'ASCOUT' ) THEN
         CALL ASCOUT( STATUS )

*  05-JUL-1991
      ELSE IF ( ACTION .EQ. 'BBODY' ) THEN
         CALL BBODY( STATUS )

*  24-JUL-1991
      ELSE IF ( ACTION .EQ. 'CORREL' ) THEN
         CALL CORREL( STATUS )

*  16-MAR-1992 17:31:47.53
      ELSE IF ( ACTION .EQ. 'EDITEXT' ) THEN
         CALL EDITEXT( STATUS )

*  28-JUL-1992
      ELSE IF ( ACTION .EQ. 'EVALFIT' ) THEN
         CALL EVALFIT( STATUS )

*  14 May 1993
      ELSE IF ( ACTION .EQ. 'FILLCUBE' ) THEN
         CALL FILLCUBE( STATUS )

*  27-JAN-1993
      ELSE IF ( ACTION .EQ. 'FITBB' ) THEN
         CALL FITBB( STATUS )

*  25-NOV-1991 13:23:16.00
      ELSE IF ( ACTION .EQ. 'FITGAUSS' ) THEN
         CALL FITGAUSS( STATUS )

*  21 Nov 1995
      ELSE IF ( ACTION .EQ. 'FITPOLY' ) THEN
         CALL FITPOLY( STATUS )

*  23-JUL-1992
      ELSE IF ( ACTION .EQ. 'FITTRI' ) THEN
         CALL FITTRI( STATUS )

*  05-JUL-1991
      ELSE IF ( ACTION .EQ. 'GROW' ) THEN
         CALL GROWS( STATUS )

*  01 Mar 1994
      ELSE IF ( ACTION .EQ. 'MOMENTS' ) THEN
         CALL MOMENTS( STATUS )

*  19 May 1994
      ELSE IF ( ACTION .EQ. 'MOVIE' ) THEN
         CALL MOVIE( STATUS )

*  17-FEB-1992 10:52:50.62
      ELSE IF ( ACTION .EQ. 'RESAMP' ) THEN
         CALL RESAMP( STATUS )

*  22 Jun 1994
      ELSE IF ( ACTION .EQ. 'SPECCONT' ) THEN
         CALL SPECCONT( STATUS )

*  22 Sep 1994
      ELSE IF ( ACTION .EQ. 'SPECGRID' ) THEN
         CALL SPECGRID( STATUS )

*  21-SEP-1991 15:14:35.96
      ELSE IF ( ACTION .EQ. 'SPECPLOT' ) THEN
         CALL SPECPLOT( STATUS )

*  10-JUN-1991
      ELSE IF ( ACTION .EQ. 'SUBSET' ) THEN
         CALL SUBSET( STATUS )

*  25-NOV-1991 13:24:55.46
      ELSE IF ( ACTION .EQ. 'XTRACT' ) THEN
         CALL XTRACT( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( 'SPECDRE_ERR',
     :                 'SPECDRE: The action name ''^ACTION'' is ' //
     :                 'not recognised by the SPECDRE monolith.',
     :                 STATUS )
      END IF

*  Cancel any remaining NDF parameters that were opened by this action
      CALL NDF_CANCL( ' ', STATUS)

      END
