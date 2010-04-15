      SUBROUTINE SETOBJ( STATUS )
*+
*  Name:
*     SETOBJ

*  Purpose:
*     Assign value to an HDS primitive.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETOBJ( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine assigns a specified (numeric or string) value to an
*     existing HDS primitive. The destination object must exist. It can
*     be a primitive scalar or a cell in a primitive array.

*  Usage:
*     setobj value object

*  ADAM Parameters:
*     VALUE = LITERAL (Read)
*        The value the scalar primitive HDS object is to assume.
*     OBJECT = HDSOBJECT (Read)
*        The HDS object to be modified. Specify beginning with directory
*        and file name in the syntax of the operating system, followed by
*        the dot-separated structure hierarchy. Elements of structure
*        arrays are specified in ordinary brackets ().

*  Examples:
*     setobj value=90. object=file.MORE.FIGARO.TIME
*        Store the number or string 90 in the existing scalar HDS
*        object MORE.FIGARO.TIME in the file "file".

*  Authors:
*     KS: Keith Shortridge (AAO)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     JFL: John Lightfoot (ROE)
*     {enter_new_authors_here}

*  History:
*     28-NOV-1983 (KS/CIT):
*        Original version.
*     19-APR-1985 (KS):
*        Bug fix. String now set to blanks before character object is
*        read.  Prevents extraneous characters appearing at end of
*        string.
*     01-MAY-1985 (KS):
*        Very minor bug fix. Format of file open error message changed
*        so parentheses match.
*     05-MAY-1986 (KS):
*        ICH_ENCODE replaced by the more accurate ICH_CF for the setting
*        of numeric DCL symbols.
*     25-MAR-1991 (KS):
*        Now allows for more than one default extension, through use of
*        FIGX_ASFNAM.
*     17-SEP-1991 (HME):
*        If data_object and value are in the same file, LET must not try
*        to open it a second time.
*     17-SEP-1991 (JFL):
*        Length of object name incresed from 32 to 80 characters to cope
*        with long names in JCMT structure.
*     01-OCT-1992 (HME):
*        Re-written in terms of HDS.
*     27-OCT-1992 (HME):
*        Reviewed again with the know-how from re-writing DTA_CYVAR.
*     12-MAR-1993 (HME):
*        Report errors immediately.
*     06-APR-1993 (HME):
*        Split away from LET.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 132 ) CVALUE
      CHARACTER * ( 64 ) MESSAG  ! Error message
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the value, so we know which of the two modes is chosen.
      CALL PAR_GET0C( 'VALUE', CVALUE, STATUS )

*  This is fairly easy, provided DAT_PUT does the tricky bits.
      CALL DAT_ASSOC( 'OBJECT', 'UPDATE', LOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         MESSAG = 'Object does not exist.'
         CALL ERR_REP( 'SETOBJ_ERR', 'SETOBJ: ' // MESSAG, STATUS )
         GO TO 500
      END IF
      CALL DAT_PUT0C( LOC, CVALUE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         MESSAG = 'Failed putting the value into the HDS object.'
         CALL ERR_REP( 'SETOBJ_ERR', 'SETOBJ: ' // MESSAG, STATUS )
         GO TO 500
      END IF
      CALL DAT_ANNUL( LOC, STATUS )
      CALL DAT_CANCL( 'OBJECT', STATUS )

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( LOC, STATUS )
         CALL DAT_CANCL( 'OBJECT', STATUS )
      END IF

      END
