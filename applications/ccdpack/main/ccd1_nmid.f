      SUBROUTINE CCD1_NMID( INDF, ID, MATCH, STATUS )
*+
*  Name:
*     CCD1_NMID

*  Purpose:
*     Check if stored frameset matches NDF.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_NMID( INDF, ID, MATCH, STATUS )

*  Description:
*     This routine checks a given NDF to see whether it matches the ID
*     string.  The ID string is (of the same type as) that written by
*     the REGISTER task to the AST file identifying framesets, so that
*     matching is in the sense defined by that task.  The ID string 
*     consists of a keyword indicating the kind of test, followed by
*     some text in a format specific to that keyword.  Currently 
*     implemented keywords are:
*
*        FITSID <fitskey> <value>
*           An NDF matches this ID if the first FITS header card with 
*           the keyword <fitskey> has the value <value>.  If the value 
*           is of type CHARACTER it must be in single quotes.  <ftskey>
*           may be compound to permit reading of hierarchical keywords.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     ID = CHARACTER * ( * ) (Given)
*        String identifying the NDF.
*     MATCH = LOGICAL (Returned)
*        Whether the NDF matches the given ID string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     08-MAR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      
*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) ID
      
*  Arguments Returned:
      LOGICAL MATCH
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) FTSVAL  ! Character value of FITS card
      INTEGER IAT                ! Position in string
      INTEGER IWE                ! Position of word end
      INTEGER IWS                ! Position of word start
      
*.

*  Set default return value.
      MATCH = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Identify the type of ID string we have and switch on the result.
      IAT = 1

*  FITS ID type.
      IF ( ID( IAT:IAT + 6 ) .EQ. 'FITSID ' ) THEN
         IAT = IAT + 7
         CALL CHR_FIWS( ID, IAT, STATUS )
         IWS = IAT
         CALL CHR_FIWE( ID, IAT, STATUS )
         IWE = IAT
         CALL CCD1_FTVAL( ID( IWS:IWE ), INDF, FTSVAL, STATUS )
         IAT = IWE + 1
         CALL CHR_FIWS( ID, IAT, STATUS )
         MATCH = ID( IAT: ) .EQ. FTSVAL

*  Unidintified ID string type
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ID', ID )
         CALL CCD1_ERREP( 'CCD1_NMID_BADID', 
     :                    '  ID string "^ID" unrecognised', STATUS )

      END IF
      
      END
* $Id$
