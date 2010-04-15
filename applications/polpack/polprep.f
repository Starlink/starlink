      SUBROUTINE POLPREP( STATUS )
*+
*  Name:
*     POLPREP

*  Purpose:
*     Prepare an input image for use by Polka.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLPREP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application prepares an input image for subsequent use by
*     Polka. It is called from within the Polka.tcl script and is not
*     intended to be used directly by users.

*  Usage:
*     polprep in out ref

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF which is to be prepared.
*     OUT = NDF (Write)
*        The output NDF data structure containing a prepared copy of IN.
*     REF = _LOGICAL (Read)
*        Should be TRUE if the supplied IN image is the reference image.
*        In this case the IN image does not need to have a POLPACK extension.
*     FRAME = LITERAL (Write)
*        The Current Frame Domain in the IN image. If REF is FALSE., then
*        a the string "BADPOL" is returned if the IN image does not have a
*        usable POLPACK extension.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUN-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FRAME*50
      INTEGER IFRM
      INTEGER INDF
      INTEGER INDF2
      INTEGER IWCS
      LOGICAL OK
      LOGICAL REF
      LOGICAL THERE
      REAL ANLANG
      REAL WPLATE
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL NDF_ASSOC( 'IN', 'READ', INDF, STATUS )

*  See if it is the reference image.
      CALL PAR_GET0L( 'REF', REF, STATUS )

*  Unless it is the refernce image, check it has a POLPACK extension
*  containing either ANLANG or WPLATE items
      IF( .NOT. REF ) THEN

*  Assume we do not have a usable NDF.
         OK = .FALSE.

*  See if the POLPACK extension exists.
         CALL NDF_XSTAT( INDF, 'POLPACK', THERE, STATUS )

*  If so, check that at least one of ANLANG or WPLATE is defined within
*  the extension.
         IF( THERE ) THEN
            ANLANG = VAL__BADR
            WPLATE = VAL__BADR
            CALL NDF_XGT0R( INDF, 'POLPACK', 'ANLANG', ANLANG, STATUS )
            CALL NDF_XGT0R( INDF, 'POLPACK', 'WPLATE', WPLATE, STATUS )

*  If so, the NDF is OK.
            OK = ( ANLANG .NE. VAL__BADR .OR. WPLATE .NE. VAL__BADR )

         END IF

*  If the NDF is not the reference NDF we do not need a POLPACK extension.
      ELSE
         OK = .TRUE.
      END IF

*  If the NDF is OK...
      IF( OK ) THEN

*  Get the Domain of the Current Frame in the NDF, and then set it to
*  PIXEL.
         CALL KPG1_GTWCS( INDF, IWCS, STATUS )
         FRAME = AST_GETC( IWCS, 'DOMAIN', STATUS )
         CALL KPG1_ASFFR( IWCS, 'PIXEL', IFRM, STATUS )
         CALL AST_SETI( IWCS, 'CURRENT', IFRM, STATUS )

*  Copy the input NDF to create the output NDF.
         CALL NDF_PROP( INDF, 'Title,Label,Units,Data,Variance,'//
     :                  'Quality,Axis,History,WCS', 'OUT', INDF2,
     :                  STATUS )

*  Save the new WCS FrameSet in the output NDF.
         CALL NDF_PTWCS( IWCS, INDF2, STATUS )

*  Annull the FrameSet pointer.
         CALL AST_ANNUL( IWCS, STATUS )

*  If the NDF is not OK, return BADPOL as the current frame domain.
      ELSE
         FRAME = 'BADPOL'
      END IF

*  Write the Domain name to the output parameter.
      CALL PAR_PUT0C( 'FRAME', FRAME, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLPREP_ERR', 'POLPREP: Error preparing an '//
     :                 'NDF for use by Polka.', STATUS )
      END IF

      END
