      SUBROUTINE CCD1_SETRM( INDF, STATUS )
*+
*  Name:
*     CCD1_SETRM

*  Purpose:
*     Erase any Set membership information from an NDF.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_SETWR( INDF, STATUS )

*  Description:
*     This routine erases any SET header information from the 
*     .MORE.CCDPACK extension of an NDF.  If one already exists 
*     it is overwritten.  The WCS extension is also modified, 
*     if any frame in the domain CCD_SET exists it will be removed.
*
*     Note therefore that it is probably wrong to call this routine
*     prior to writing a WCS component into an NDF (with NDF_PTWCS), 
*     unless it is first read from the NDF (with CCD1_GTWCS) in the
*     interim.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier of the NDF to be modified.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-FEB-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants
      
*  Arguments Given:
      INTEGER INDF
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IWCS               ! Identifier for WCS frameset
      INTEGER JSET               ! Frame index of CCD_SET frame
      LOGICAL THERE              ! Is HDS item present?
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator for the .MORE.CCDPACK exension
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Attempt to remove the CCDPACK.MORE.SET extension.
*  =================================================

*  See if the CCDPACK extension exists.
      CALL NDF_XSTAT( INDF, 'CCDPACK', THERE, STATUS )

*  If it does exist, get a locator to it.
      IF ( THERE ) THEN
         CALL NDF_XLOC( INDF, 'CCDPACK', 'UPDATE', XLOC, STATUS )

*  See if a SET structure exists.
         CALL DAT_THERE( XLOC, 'SET', THERE, STATUS )

*  If it does exist, erase it.
         IF ( THERE ) THEN
            CALL DAT_ERASE( XLOC, 'SET', STATUS )
         END IF
         CALL DAT_ANNUL( XLOC, STATUS )
      END IF

*  Attempt to remove the CCD_SET frame from the WCS frameset.
*  ==========================================================

*  Get the WCS frameset.
      CALL CCD1_GTWCS( INDF, IWCS, STATUS )

*  See if it contains any CCD_SET frame.
      CALL CCD1_FRDM( IWCS, 'CCD_SET', JSET, STATUS )

*  If it contains any, remove them and write the frameset back.
      IF ( JSET .NE. AST__NOFRAME ) THEN
         CALL CCD1_DMPRG( IWCS, 'CCD_SET', .FALSE., AST__NOFRAME,
     :                    STATUS )
         CALL NDF_PTWCS( IWCS, INDF, STATUS )
      END IF

*  Release AST resources.
      CALL AST_ANNUL( IWCS, STATUS )

      END
* $Id$
