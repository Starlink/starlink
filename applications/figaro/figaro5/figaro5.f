      SUBROUTINE FIGARO5( STATUS )
*+
*  Name:
*     TWODSPEC

*  Purpose:
*     Top-level ADAM monolith routine for the TWODSPEC package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIGARO5( STATUS )

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
*     TNW: Tim Wilkins (Durham)
*     BLY: M.J.Bly (Starlink, RAL)
*     TDCA: Tim Ash (Starlink, RAL)
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
*     18-MAR-1993 (TNW):
*        Adapted for TWODSPEC
*     28-NOV-1997 (BLY):
*        Renamed to TWODSPEC_MON and converted to Unix monolith standard.
*        Included old IFBLOCK as inline code.
*     22-JUN-1999 (TCDA):
*        Renamed as FIGARO5 for incorporation into FIGARO.
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
      INCLUDE 'PAR_PAR'          ! Parameter system constants

*  External Block Data:
      EXTERNAL DSA_BLOCK

*  Arguments Given and Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGNORE
      CHARACTER * ( 32 ) STRING
      CHARACTER * ( PAR__SZNAM ) ACTION

*  Internal References:
      INTEGER ICH_FOLD
      LOGICAL BATCH

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( ACTION, STATUS )

*  Initialise the (F)PAR common block.

      CALL PSX_GETENV( 'FIGARO_MODE', STRING, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         BATCH = .FALSE.
      ELSE
         IGNORE = ICH_FOLD( STRING )
         IF ( STRING .EQ. 'BATCH' ) THEN
            BATCH = .TRUE.
         ELSE
            BATCH = .FALSE.
         END IF
      END IF
      CALL PAR_INIT( ACTION, ' ', 0, BATCH, IGNORE )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine.

      IF (ACTION.EQ.'LONGSLIT') THEN
         CALL LONGSLIT (STATUS)
      ELSE IF (ACTION.EQ.'CADD') THEN
         CALL CADD (STATUS)
      ELSE IF (ACTION.EQ.'VIG') THEN
         CALL VIG (STATUS)
      ELSE IF (ACTION.EQ.'COMB') THEN
         CALL COMB (STATUS)
      ELSE IF (ACTION.EQ.'ARC2D') THEN
         CALL ARC2D (STATUS)
      ELSE IF (ACTION.EQ.'ARCSDI') THEN
         CALL ARCSDI (STATUS)
      ELSE IF (ACTION.EQ.'CSUB') THEN
         CALL CSUB (STATUS)
      ELSE IF (ACTION.EQ.'ISCAN') THEN
         CALL ISCAN (STATUS)
      ELSE IF (ACTION.EQ.'FITCONT') THEN
         CALL FITCONT (STATUS)
      ELSE IF (ACTION.EQ.'HIMAGE') THEN
         CALL HIMAGE (STATUS)
      ELSE IF (ACTION.EQ.'CSCAN') THEN
         CALL CSCAN (STATUS)
      ELSE IF (ACTION.EQ.'FIBSEP') THEN
         CALL FIBSEP (STATUS)
      ELSE IF (ACTION.EQ.'FIBDISP') THEN
         CALL FIBDISP (STATUS)
      ELSE IF (ACTION.EQ.'FIB2CUBE') THEN
         CALL FIB2CUBE (STATUS)
      ELSE IF (ACTION.EQ.'CRIGAUSS') THEN
         CALL CRIGAUSS (STATUS)
      ELSE IF (ACTION.EQ.'CUBE2LONG') THEN
         CALL CUBE2LONG (STATUS)
      ELSE IF (ACTION.EQ.'CHANGED') THEN
         CALL CHANGED (STATUS)

*  If the action name is not recognised,
*  give some hints for experienced and disappointed users.
      ELSE
         CALL FIG_HELP( 'diversion', STATUS )
      END IF

      END
*
